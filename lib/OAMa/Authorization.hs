{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OAMa.Authorization (
  authorizeEmail,
  getEmailAuth,
  forceRenew,
  showCreds,
) where

import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Exception (try)
import Control.Monad.Reader
import Data.Aeson (eitherDecode', eitherDecodeStrict, encode)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy.UTF8 qualified as BLU
import Data.ByteString.UTF8 qualified as BSU
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Time.Clock
import Data.Time.Format
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types (renderQuery)
import Network.Socket
import Network.URI qualified as URI

-- import Network.URI qualified as URI
import Network.Wai.Handler.Warp qualified as Warp
import OAMa.Environment
import System.Directory qualified as D
import System.Exit (ExitCode (ExitSuccess), exitFailure, exitWith)
import System.IO qualified as IO
import System.Posix.Syslog (Priority (..))
import System.Process qualified as P
import Text.Pretty.Simple (pPrint, pShowNoColor)
import Text.Printf (printf)
import Web.Twain qualified as TW

-- The OAuth2 authorization flow's implementation is based on these docs:
--
-- https://developers.google.com/identity/protocols/oauth2/native-app
-- https://docs.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow
--
-- The managed credentials are kept in either in Gnome keyring
-- or in GPG encrypted files. Only one of these methods can be used.

getAuthRecord :: Environment -> EmailAddress -> IO AuthRecord
getAuthRecord env email_ = do
  getAR env.config.encryption
 where
  getAR :: Encryption -> IO AuthRecord
  getAR KEYRING = do
    (x, o, e) <- P.readProcessWithExitCode "secret-tool" ["lookup", "oama", email_.unEmailAddress] ""
    if x == ExitSuccess
      then case eitherDecode' (BLU.fromString o) :: Either String AuthRecord of
        Left err -> error $ "readAuthRecord:\n" ++ err
        Right rec -> return rec
      else do
        putStrLn $ "Can't find authorization record for " ++ unEmailAddress email_
        putStr e
        exitWith x
  getAR (GPG _) = do
    let gpgFile = env.state_dir <> "/" <> email_.unEmailAddress <> ".oama"
    authRecExist <- D.doesFileExist gpgFile
    if authRecExist
      then do
        (x, o, e) <- P.readProcessWithExitCode "gpg" ["--decrypt", gpgFile] ""
        if x == ExitSuccess
          then case eitherDecode' (BLU.fromString o) :: Either String AuthRecord of
            Left err -> error $ "readAuthRecord:\n" ++ err
            Right rec -> return rec
          else do
            putStr e
            exitWith x
      else do
        printf "Can't find authorization record for %s\n" (unEmailAddress email_)
        printf "You must run `oama authorize ...` before using other operations.\n"
        logger Error $ printf "Can't find authorization record for %s\n" (unEmailAddress email_)
        exitFailure

putAuthRecord :: Environment -> EmailAddress -> AuthRecord -> IO ()
putAuthRecord env email_ rec = do
  putAR env.config.encryption
 where
  putAR :: Encryption -> IO ()
  putAR KEYRING = do
    let jsrec = BLU.toString $ encode rec
        m = email_.unEmailAddress
    (Just h, _, _, p) <-
      P.createProcess
        (P.proc "secret-tool" ["store", "--label", "oama - " ++ m, "oama", m])
          { P.std_in = P.CreatePipe
          }
    IO.hPutStr h jsrec
    IO.hFlush h
    IO.hClose h
    x <- P.waitForProcess p
    if x == ExitSuccess
      then return ()
      else exitWith x
  putAR (GPG keyID) = do
    let gpgFile = env.state_dir <> "/" <> email_.unEmailAddress <> ".oama"
        jsrec = BLU.toString $ encode rec
    (Just h, _, _, p) <-
      P.createProcess
        (P.proc "gpg" ["--encrypt", "--recipient", keyID, "-o", gpgFile <> ".new"])
          { P.std_in = P.CreatePipe
          }
    IO.hPutStr h jsrec
    IO.hFlush h
    IO.hClose h
    x <- P.waitForProcess p
    if x == ExitSuccess
      then D.renameFile (gpgFile ++ ".new") gpgFile
      else exitWith x

timeStampFormat :: String
timeStampFormat = "%Y-%m-%d %H:%M %Z"

{-| Get access_token for then given email
while renewing it when necessary
-}
getEmailAuth :: Environment -> EmailAddress -> IO ()
getEmailAuth env email_ = do
  getEmailAuth' env email_
    >>= \case
      Right rec -> putStrLn $ access_token rec
      Left errmsg -> error $ "getEmailAuth:\n" ++ errmsg

getEmailAuth' :: Environment -> EmailAddress -> IO (Either String AuthRecord)
getEmailAuth' env email_ = do
  authrec <- getAuthRecord env email_
  now <- getCurrentTime
  let expd = fromMaybe "2000-01-01 12:00 UTC" authrec.exp_date
  if now > parseTimeOrError True defaultTimeLocale timeStampFormat expd
    then do
      renewAccessToken env authrec.service authrec.refresh_token
        >>= \case
          Left err -> return $ Left err
          Right newat -> do
            let expire = addUTCTime (newat.expires_in - 300) now
                expDate = formatTime defaultTimeLocale timeStampFormat expire
                authrec' =
                  authrec
                    { access_token = newat.access_token
                    , expires_in = newat.expires_in
                    , exp_date = Just expDate
                    , -- despite of google's doc refresh_token is not returned!
                      -- , refresh_token = refresh_token newat
                      scope = newat.scope
                    , token_type = newat.token_type
                    }
            putAuthRecord env email_ authrec'
            logger Notice $ printf "new access token for %s - expires at %s" (unEmailAddress email_) expDate
            return $ Right authrec'
    else return $ Right authrec

sendRequest ::
  HTTPMethod
  -> ParamsMode
  -> String
  -> [(String, Maybe String)]
  -> IO (Either String BSU.ByteString)
sendRequest httpMethod paramsMode url params = do
  let params_ = filter (\(_, y) -> isJust y) params
  case paramsMode of
    RequestBody -> do
      req <- parseRequest $ show httpMethod ++ " " ++ url
      let ps = [(x, y) | (x, Just y) <- params]
          mps = M.fromList ps
          req' = setRequestBodyJSON mps req
      runPost req'
    RequestBodyForm -> do
      req <- parseRequest $ show httpMethod ++ " " ++ url
      let ps = [bimap BSU.fromString (BSU.fromString . fromJust) x | x <- params_]
          req' = setRequestBodyURLEncoded ps req
      runPost req'
    QueryString -> do
      req <- parseRequest $ show httpMethod ++ " " ++ url
      let ps = [bimap BSU.fromString (BSU.fromString <$>) x | x <- params_]
          req' = setRequestQueryString ps req
      runPost req'
 where
  runPost query = do
    (try $ httpBS query :: IO (Either HttpException (Response BSU.ByteString)))
      >>= \case
        Left (HttpExceptionRequest _ x) -> return $ Left $ show x
        Left (InvalidUrlException u _) -> return $ Left u
        Right resp -> do
          let body = getResponseBody resp
          return $ Right body

fetchAuthRecord ::
  HTTPMethod
  -> ParamsMode
  -> String
  -> [(String, Maybe String)]
  -> IO (Either String AuthRecord)
fetchAuthRecord httpMethod paramsMode url queries = do
  sendRequest httpMethod paramsMode url queries
    >>= \case
      Left err -> return $ Left err
      Right resp ->
        case eitherDecodeStrict resp :: Either String AuthRecord of
          Left err -> do
            putStrLn $ BSU.toString resp
            return $ Left $ "fetchAuthRecord: " ++ err
          Right rec -> return $ Right rec

renewAccessToken :: Environment -> Maybe String -> Maybe String -> IO (Either String AuthRecord)
renewAccessToken _ Nothing _ = return $ Left "renewAccessToken: Nothing as service string argument"
renewAccessToken _ _ Nothing = return $ Left "renewAccessToken: Nothing as refresh token argument"
renewAccessToken env (Just serv) rft = do
  api <- getServiceAPI env serv
  let qs =
        [ ("client_id", Just api.client_id)
        , ("client_secret", Just api.client_secret)
        , ("grant_type", Just "refresh_token")
        , ("refresh_token", rft)
        ]
  fetchAuthRecord
    (fromJust api.token_http_method)
    (fromJust api.token_params_mode)
    (fromJust api.token_endpoint)
    qs

forceRenew :: Environment -> EmailAddress -> IO ()
forceRenew env email_ = do
  authrec <- getAuthRecord env email_
  now <- getCurrentTime
  renewAccessToken env (service authrec) (refresh_token authrec)
    >>= \case
      Left err -> error err
      Right newat -> do
        let expire = addUTCTime (expires_in newat - 300) now
            expDate = formatTime defaultTimeLocale timeStampFormat expire
            authrec' =
              authrec
                { access_token = access_token newat
                , expires_in = expires_in newat
                , exp_date = Just expDate
                , -- despite of google's doc refresh_token is not returned!
                  -- , refresh_token = refresh_token newat
                  scope = scope newat
                , token_type = token_type newat
                }
        putAuthRecord env email_ authrec'
        logger Notice $ printf "new access token for %s - expires at %s" (unEmailAddress email_) expDate
        printf "Obtained new access token for %s - expires at %s.\n" (unEmailAddress email_) expDate

-- | Show current credentials for the given email
showCreds :: Environment -> EmailAddress -> IO ()
showCreds env email_ = do
  getEmailAuth' env email_
    >>= \case
      Right rec -> do
        printf "email: %s\n" (unEmailAddress $ fromJust rec.email)
        printf "service: %s\n" (fromMaybe "error - missing service" rec.service)
        printf "scope: %s\n" rec.scope
        printf "refresh_token: %s\n" (fromMaybe "error - missing refresh_token" rec.refresh_token)
        printf "access_token: %s\n" rec.access_token
        printf "token_type: %s\n" rec.token_type
        printf "exp_date: %s\n" (fromMaybe "error - missing exp_date" rec.exp_date)
      -- printf "expires_in: %s\n" $ show rec.expires_in
      Left errmsg -> error $ "showCreds:\n" ++ errmsg

-- initial registration for authorization credentials

getHostandPort :: String -> (String, Int, String)
getHostandPort uriString =
  -- to understand this ugliness
  -- see https://hackage.haskell.org/package/network-uri
  let ruri = fromJust $ URI.parseURI uriString
      ruria = URI.uriAuthority ruri
      rurihost = URI.uriRegName $ fromJust ruria
      ruriport = URI.uriPort $ fromJust ruria
      portnum = read (drop 1 ruriport) :: Int
      routepath = URI.uriPath $ fromJust $ URI.parseURI uriString
   in (rurihost, portnum, routepath)

getRandomFreePort :: IO Int
getRandomFreePort = do
  s <- socket AF_INET Stream defaultProtocol
  bind s (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  p <- socketPort s
  close s -- should be closed just before usage
  return $ read (show p)

getAccessToken :: Environment -> String -> String -> String -> IO (Either String AuthRecord)
getAccessToken env serv redirectURI authcode = do
  api <- getServiceAPI env serv
  let qs =
        [ ("client_id", Just api.client_id)
        , ("client_secret", Just api.client_secret)
        , ("code", Just authcode)
        , ("grant_type", Just "authorization_code")
        , ("tenant", api.tenant)
        , ("redirect_uri", Just redirectURI)
        ]
  fetchAuthRecord
    (fromJust api.token_http_method)
    (fromJust api.token_params_mode)
    (fromJust api.token_endpoint)
    qs

data Page = Redirect String | Content BSU.ByteString

generateAuthPage ::
  Environment -> String -> String -> EmailAddress -> Bool -> IO (Either String Page)
generateAuthPage env serv redirectURI email_ noHint = do
  api <- getServiceAPI env serv
  let endpoint = fromJust api.auth_endpoint
      hint = if noHint then "dummy-email-address" else unEmailAddress email_
      qs =
        [ ("client_id", Just api.client_id)
        , ("response_type", Just "code")
        , ("scope", api.auth_scope)
        , ("login_hint", Just hint)
        , ("redirect_uri", Just redirectURI)
        , ("access_type", api.access_type)
        , ("prompt", api.prompt)
        ]
  case fromJust api.auth_http_method of
    GET -> do
      let urlBase = fromJust $ URI.parseURI endpoint
          bsQuery = bimap BSU.fromString (fmap BSU.fromString) <$> qs
          url = urlBase {URI.uriQuery = BSU.toString $ renderQuery True bsQuery}
      pure $ Right $ Redirect $ show url
    POST ->
      fmap Content
        <$> sendRequest
          POST
          (fromJust api.auth_params_mode)
          endpoint
          qs

data AuthResult = AuthSuccess | AuthFailure

localWebServer ::
  MVar AuthResult
  -> Environment
  -> String
  -> String
  -> EmailAddress
  -> Bool
  -> IO ()
localWebServer mvar env redirectURI serv email_ noHint = do
  generateAuthPage env serv redirectURI email_ noHint
    >>= \case
      Left errmsg -> error $ "localWebServer:\n" ++ errmsg
      Right authP -> do
        let startAuth :: TW.ResponderM a
            startAuth = case authP of
              Redirect url -> TW.send $ TW.redirect302 $ fromString url
              Content page -> TW.send $ TW.html $ fromStrict page
            (hostname, portnumber, routepath) = getHostandPort redirectURI
            localhostWebServer =
              Warp.setPort portnumber $ Warp.setHost (fromString hostname) Warp.defaultSettings
            finishAuth :: TW.ResponderM a
            finishAuth = do
              code :: String <- TW.param "code"
              liftIO (getAccessToken env serv redirectURI code)
                >>= \case
                  Left errmsg -> do
                    liftIO $ putMVar mvar AuthFailure
                    error $ "localWebServer:\n" ++ errmsg
                  Right authr -> do
                    now <- liftIO getCurrentTime
                    let expire = addUTCTime (expires_in authr - 300) now
                        expDate = formatTime defaultTimeLocale timeStampFormat expire
                        authRec = authr {exp_date = Just expDate, email = Just email_, service = Just serv}
                    liftIO $ putAuthRecord env email_ authRec
                    liftIO $ putMVar mvar AuthSuccess
                    TW.send $
                      TW.html $
                        BLU.fromString $
                          printf "<h4>Received new refresh and access tokens for %s</h4>" (unEmailAddress email_)
                            <> if env.config.encryption == KEYRING
                              then printf "<p>They have been stored in the keyring of your password manager.</p>"
                              else
                                printf
                                  "<p>They have been saved in <samp>%s</samp> encrypted.</p>"
                                  (env.state_dir <> "/" <> email_.unEmailAddress <> ".oama")
            casService :: TW.ResponderM a
            casService = do
              casURL :: Text <- TW.param "service"
              TW.send $ TW.redirect302 casURL

            routes :: [TW.Middleware]
            routes =
              [ TW.get "/cas/login" casService
              , TW.get "/start" startAuth
              , TW.get (fromString routepath) finishAuth
              ]

            missing :: TW.ResponderM a
            missing = do
              req <- TW.request
              if req.rawPathInfo == "/favicon.ico"
                then TW.send $ TW.html "missing favicon.ico"
                else do
                  liftIO $ printf "localWebServer - invalid request:\n"
                  pPrint req
                  let req' = TLE.encodeUtf8 $ pShowNoColor req
                  TW.send $
                    TW.html $
                      "<h3>localWebServer - invalid request</h3>"
                        <> "<pre>"
                        <> req'
                        <> "</pre>"

        Warp.runSettings localhostWebServer $ foldr ($) (TW.notFound missing) routes

-- when this function is called we always start from scratch
authorizeEmail :: Environment -> String -> EmailAddress -> Bool -> IO ()
authorizeEmail env servName email_ noHint = do
  case M.lookup servName env.services of
    Nothing -> error $ "authorizeEmail: Can't find such service: " ++ servName
    Just _ -> do
      api <- getServiceAPI env servName
      portnumber <- getRandomFreePort
      let redirectURI = api.redirect_uri <|> Just (printf "http://localhost:%d" portnumber)
      mvar <- newEmptyMVar
      _ <- forkIO $ localWebServer mvar env (fromJust redirectURI) servName email_ noHint
      printf
        "Authorization to grant OAuth2 access to %s started ... \n"
        (unEmailAddress email_)
      let (hostname, portno, _) = getHostandPort $ fromJust redirectURI
      printf "Visit http://%s:%d/start in your browser ...\n" hostname portno
      takeMVar mvar
        >>= \case
          AuthSuccess -> do
            printf "Received refresh and access tokens ...\n"
            if env.config.encryption == KEYRING
              then printf "They have been stored in the Gome keyring ...\n"
              else
                printf
                  "They have been saved in %s encrypted ...\n"
                  (env.state_dir <> "/" <> email_.unEmailAddress <> ".oama")
          AuthFailure -> printf "ERROR - Authorization failed.\n"
      threadDelay 5_000_000
      printf "... done.\n"
