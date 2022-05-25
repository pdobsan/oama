{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}

module MailCtl.Authorization
  ( getEmailPwd
  , getEmailOauth2
  )
where

import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode', eitherDecodeStrict)
import Data.ByteString.Lazy.UTF8 qualified as BLU
import Data.ByteString.UTF8 qualified as BSU
import Data.Time.Clock.POSIX qualified as TX
import GHC.Generics (Generic)
import MailCtl.Environment
import MailCtl.Utilities (logger)
import Network.HTTP.Conduit
import Network.HTTP.Simple
import System.Directory qualified as D
import System.Environment qualified as E
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.IO qualified as IO
import System.Process qualified as P

data RefreshRecord = RefreshRecord
  { access_token :: String
  , expires_in   :: Float
  , scope        :: String
  , token_type   :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data OAuth2Record = OAuth2Record
  { email         :: String
  , registration  :: String
  , authflow      :: String
  , refresh_token :: String
  , access_token' :: String
  , expires_at    :: Float
  }
  deriving (Show, Generic, ToJSON, FromJSON)

readOAuth2Record :: Environment -> String -> IO OAuth2Record
readOAuth2Record env emailEntry = do
  let gpgFile = oauth2_dir (config env) ++ "/" ++ emailEntry ++ "-oauth2"
  (x, o, e) <- P.readProcessWithExitCode (exec $ decrypt_cmd $ config env)
                                         ((args $ decrypt_cmd $ config env) ++ [gpgFile]) ""
  if x == ExitSuccess
    then case eitherDecode' (BLU.fromString o) :: Either String OAuth2Record of
              Left err -> error err
              Right rec -> return rec
    else do
      putStr e
      exitWith x

writeOAuth2Record :: Environment -> String -> OAuth2Record -> IO ()
writeOAuth2Record env emailEntry rec = do
  let gpgFile = oauth2_dir (config env) ++ "/" ++ emailEntry ++ "-oauth2"
      jsrec = BLU.toString $ encode rec
  (Just h, _, _, p) <- P.createProcess (P.proc (exec $ encrypt_cmd $ config env)
                         ((args $ encrypt_cmd $ config env) ++ [gpgFile ++ ".new"]))
                         { P.std_in = P.CreatePipe }
  IO.hPutStr h jsrec 
  IO.hFlush h
  IO.hClose h
  x <- P.waitForProcess p
  if x == ExitSuccess
    then D.renameFile (gpgFile ++ ".new") gpgFile
    else do
      exitWith x

getEmailOauth2 :: Environment -> String -> IO ()
getEmailOauth2 env emailEntry = do
  rec <- readOAuth2Record env emailEntry
  now <- TX.getPOSIXTime
  let now' = realToFrac now
  if now' > expires_at rec
    then do
      refresh <- renewAccessToken env (refresh_token rec)
      let expire = now' + expires_in refresh - 300
          rec' = rec { access_token' = access_token refresh, expires_at = expire }
      writeOAuth2Record env emailEntry rec'
      logger "info" ("new token for " ++ emailEntry ++ "; expires at " ++ (show expire))
      putStrLn $ access_token' rec'
    else putStrLn $ access_token' rec

renewAccessToken :: Environment -> String -> IO RefreshRecord
renewAccessToken env rft = do
  req' <- parseRequest $ "POST " ++ token_endpoint (google $ config env)
  let qs = "client_id=" ++ client_id (google $ config env) ++ "&" ++
           "client_secret=" ++ client_secret (google $ config env) ++ "&" ++
           "grant_type=refresh_token&" ++
           "refresh_token=" ++ rft
      req = req'{queryString = BSU.fromString qs}
  resp <- httpBS req
  case eitherDecodeStrict (getResponseBody resp) :: Either String RefreshRecord of
    Left err -> error err
    Right rec -> return rec

getEmailPwd :: Environment -> String -> IO ()
getEmailPwd env emailEntry = do
  psd <- E.lookupEnv "PASSWORD_STORE_DIR"
  case psd of
    Nothing -> do
      E.setEnv "PASSWORD_STORE_DIR" (password_store $ config env)
      getEPwd
    _ -> getEPwd
 where
  getEPwd = do
    (x, o, e) <- P.readProcessWithExitCode (exec (pass_cmd $ config env))
                                           [head (args (pass_cmd $ config env)) ++ emailEntry]
                                           []
    if x == ExitSuccess
      then putStrLn $ head (lines o)
      else do
        putStr e
        exitWith x

