{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module MailCtl.Authorization
  ( getEmailPwd
  , getEmailAuth
  , authorizeEmail
  )
where

import Control.Concurrent
import Control.Exception (try)
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode', eitherDecodeStrict)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy.UTF8 qualified as BLU
import Data.ByteString.UTF8 qualified as BSU
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Time.Format
import GHC.Generics (Generic)
import MailCtl.Environment hiding (Google (scope))
import MailCtl.Environment qualified as Google (Google (scope))
import MailCtl.Utilities (logger)
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.Wai.Handler.Warp (run)
import System.Directory qualified as D
import System.Environment qualified as E
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.IO qualified as IO
import System.Posix.Syslog (Priority(..))
import System.Process qualified as P
import Text.Printf
import Web.Twain qualified as TW
-- import Text.Pretty.Simple

data AuthRecord = AuthRecord
  { access_token  :: String
  , expires_in    :: NominalDiffTime
  , scope         :: String
  , token_type    :: String
  , exp_date      :: Maybe String
  , refresh_token :: Maybe String
  , email         :: Maybe String
  , registration  :: Maybe String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- The OAuth2 authorization flow i simplemented along the lines of
-- https://developers.google.com/identity/protocols/oauth2/native-app
-- The managed credentials are kept in encrypted files using GNU PG.

readAuthRecord :: Environment -> String -> IO AuthRecord
readAuthRecord env email' = do
  let gpgFile = oauth2_dir (config env) ++ "/" ++ email' ++ ".auth"
  (x, o, e) <- P.readProcessWithExitCode (exec $ decrypt_cmd $ config env)
                                         (args (decrypt_cmd $ config env) ++ [gpgFile]) ""
  if x == ExitSuccess
    then case eitherDecode' (BLU.fromString o) :: Either String AuthRecord of
              Left err -> error err
              Right rec -> return rec
    else do
      putStr e
      exitWith x

writeAuthRecord :: Environment -> String -> AuthRecord -> IO ()
writeAuthRecord env email' rec = do
  let gpgFile = oauth2_dir (config env) ++ "/" ++ email' ++ ".auth"
      jsrec = BLU.toString $ encode rec
  (Just h, _, _, p) <- P.createProcess (P.proc (exec $ encrypt_cmd $ config env)
                         (args  (encrypt_cmd $ config env) ++ [gpgFile ++ ".new"]))
                         { P.std_in = P.CreatePipe }
  IO.hPutStr h jsrec 
  IO.hFlush h
  IO.hClose h
  x <- P.waitForProcess p
  if x == ExitSuccess
    then D.renameFile (gpgFile ++ ".new") gpgFile
    else exitWith x

timeStampFormat :: String
timeStampFormat = "%Y-%m-%d %H:%M %Z"

-- Provide access_token while renewing it when necessary

getEmailAuth :: Environment -> String -> IO ()
getEmailAuth env email' = do
  rec <- readAuthRecord env email'
  now <- getCurrentTime
  let expd = fromMaybe "2000-01-01 12:00 UTC" (exp_date rec)
  if now > parseTimeOrError True defaultTimeLocale timeStampFormat expd
    then do
      refresh <- renewAccessToken env (fromMaybe "" (refresh_token rec))
      let expire = addUTCTime (expires_in refresh - 300) now
          expire' = formatTime defaultTimeLocale timeStampFormat expire
          rec' = rec { access_token = access_token refresh
                     , expires_in = expires_in refresh, exp_date = Just expire'
                     -- despite of google's doc refresh_token is not returned!
                     -- , refresh_token = refresh_token refresh
                     , scope = scope refresh
                     , token_type = token_type refresh
                     }
      writeAuthRecord env email' rec'
      logger Notice ("new token for " ++ email' ++ "; expires at " ++ expire')
      putStrLn $ access_token rec'
    else putStrLn $ access_token rec

updateRequest :: Request -> [(String, String)] -> Request
updateRequest req xs =
  let ys = [ (BSU.fromString $ fst x, Just $ BSU.fromString $ snd x) | x <- xs ]
  in  setRequestQueryString ys req

renewAccessToken :: Environment -> String -> IO AuthRecord
renewAccessToken env rft = do
  let qs = [ ("client_id", client_id (google $ config env))
           , ("client_secret", client_secret (google $ config env))
           , ("grant_type", "refresh_token")
           , ("refresh_token", rft)
           ]
  req <- parseRequest $ "POST " ++ token_endpoint (google $ config env)
  eresp <- try $ httpBS (updateRequest req qs) :: IO (Either HttpException (Response BSU.ByteString))
  case eresp of
    -- hide request containing sensitive information
    Left (HttpExceptionRequest _ x) -> error $ show x
    Left (InvalidUrlException u _) -> error $ show u
    Right resp ->
      case eitherDecodeStrict (getResponseBody resp) :: Either String AuthRecord of
        Left err -> error err
        Right rec -> return rec


-- initial registration for authorization credentials

getAccessToken :: Environment -> String -> IO AuthRecord
getAccessToken env authcode = do
  let qs = [ ("client_id", client_id (google $ config env))
           , ("client_secret", client_secret (google $ config env))
           , ("code", authcode)
           , ("grant_type", "authorization_code")
           ]
  req <- parseRequest $ "POST " ++ token_endpoint (google $ config env)
  eresp <- try $ httpBS (updateRequest req qs) :: IO (Either HttpException (Response BSU.ByteString))
  case eresp of
    -- hide request containing sensitive information
    Left (HttpExceptionRequest _ x) -> error $ show x
    Left (InvalidUrlException u _) -> error $ show u
    Right resp ->
      case eitherDecodeStrict (getResponseBody resp) :: Either String AuthRecord of
        Left err -> error err
        Right rec -> return rec

generateAuthPage :: Environment -> String -> IO BSU.ByteString
generateAuthPage env email' = do
  let qs = [ ("client_id", client_id (google $ config env))
           , ("redirect_uri", redirect_uri (google $ config env))
           , ("response_type", "code")
           , ("scope", Google.scope (google $ config env))
           , ("login_hint", email')
           ]
  req <- parseRequest $ "POST " ++ auth_endpoint (google $ config env)
  eresp <- try $ httpBS (updateRequest req qs) :: IO (Either HttpException (Response BSU.ByteString))
  case eresp of
    -- hide request containing sensitive information
    Left (HttpExceptionRequest _ x) -> error $ show x
    Left (InvalidUrlException u _) -> error $ show u
    Right resp -> do
      return $ responseBody resp

localWebServer :: Environment -> String -> IO ()
localWebServer env email' = do
  authPage <- generateAuthPage env email'
  let startAuth :: TW.ResponderM a
      startAuth = TW.send $ TW.html $ fromStrict authPage
      finishAuth :: TW.ResponderM a
      finishAuth = do
        code :: String <- TW.param "code"
        scope' :: String <- TW.param "scope"
        TW.send $ TW.html $ BLU.fromString $
          printf "<h4>%s</h4><p>code: %s</p><p>scope: %s</p>" email' code scope'
      routes :: [TW.Middleware]
      routes = [ TW.get "/start" startAuth
               , TW.get "/" finishAuth
               ]
      missing :: TW.ResponderM a
      missing = TW.send $ TW.html "Not found..."
  run 8080 $ foldr ($) (TW.notFound missing) routes

authorizeEmail :: Environment -> String -> IO AuthRecord
authorizeEmail env email' = do
  putStrLn $ "To authorize " ++ email' ++ " visit url below:"
  putStrLn $ redirect_uri (google $ config env) ++ "/start"
  _ <- forkIO $ localWebServer env email'
  putStrLn "Authorization started ... "
  putStrLn "Hit <Enter> when it has completed --> "
  _ <- getLine
  return $ AuthRecord "access" 3000 "scope" "bearer"
          (Just "2022") (Just "refresh") (Just email') (Just "registration")


-- Utilities for traditional password based email services
-- using [pass](https://www.passwordstore.org/) 

getEmailPwd :: Environment -> String -> IO ()
getEmailPwd env email' = do
  password <- getEmailPwd' env email'
  putStr password

getEmailPwd' :: Environment -> String -> IO String
getEmailPwd' env email' = do
  psd <- E.lookupEnv "PASSWORD_STORE_DIR"
  case psd of
    Nothing -> do
      E.setEnv "PASSWORD_STORE_DIR" (password_store $ config env)
      getEPwd
    _ -> getEPwd
 where
  getEPwd = do
    (x, o, e) <- P.readProcessWithExitCode (exec (pass_cmd $ config env))
                                           [head (args (pass_cmd $ config env)) ++ email']
                                           []
    if x == ExitSuccess
      then return $ head (lines o)
      else do
        putStr e
        exitWith x

