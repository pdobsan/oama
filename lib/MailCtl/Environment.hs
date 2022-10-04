{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module MailCtl.Environment (
  loadEnvironment,
  getPortFromURIStr,
  serviceFieldLookup,
  AuthRecord (..),
  Configuration (..),
  EmailAddress (..),
  Environment (..),
  HTTPMethod (..),
  ParamsMode (..),
  Program (..),
  Service (..),
  Services,
  SystemState (..),
) where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time.Clock
import Data.Yaml
import GHC.Generics
import MailCtl.CommandLine
import Network.URI
import Options.Applicative (customExecParser, prefs, showHelpOnEmpty)
import System.Directory qualified as D
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import System.Process qualified as P
import Text.Printf

newtype EmailAddress = EmailAddress {unEmailAddress :: String}
  deriving (Show, Generic, ToJSON, FromJSON)

data AuthRecord = AuthRecord
  { access_token :: String,
    expires_in :: NominalDiffTime,
    scope :: String,
    token_type :: String,
    exp_date :: Maybe String,
    refresh_token :: Maybe String,
    email :: Maybe EmailAddress,
    service :: Maybe String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Program = Program
  { exec :: FilePath,
    args :: [String]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Configuration = Configuration
  { services_file :: FilePath,
    decrypt_cmd :: Program,
    encrypt_cmd :: Program,
    oauth2_dir :: FilePath,
    fdm_config :: Maybe FilePath,
    fdm_accounts :: Maybe FilePath,
    cron_indicator :: Maybe FilePath,
    password_store :: Maybe FilePath,
    pass_cmd :: Maybe Program
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data SystemState = SystemState
  { crontab :: Maybe String,
    cron_enabled :: Bool
  }
  deriving (Show, Generic, ToJSON)

data ParamsMode = RequestBody | RequestBodyForm | QueryString
data HTTPMethod = POST | GET

data Service = Service
  { auth_endpoint :: Maybe String,
    auth_http_method :: Maybe String,
    auth_params_mode :: Maybe String,
    token_endpoint :: Maybe String,
    token_http_method :: Maybe String,
    token_params_mode :: Maybe String,
    redirect_uri :: Maybe String,
    auth_scope :: Maybe String,
    client_id :: Maybe String,
    client_secret :: Maybe String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

type Services = Map String Service

data Environment = Environment
  { config :: Configuration,
    system_state :: SystemState,
    services :: Services,
    options :: Opts
  }
  deriving (Show)

defaultPORT :: Int
defaultPORT = 8080

serviceFieldLookup :: Services -> String -> (Service -> Maybe String) -> Maybe String
serviceFieldLookup services_ servName field = M.lookup servName services_ >>= field

{-
printURI :: String -> IO ()
printURI uri = do
  case parseURI uri of
    Nothing   -> error $ printf "invalid redirect uri: %s" uri
    Just uri_ -> do
      printf "uriScheme: %s\n" $ uriScheme uri_
      case uriAuthority uri_ of
        Nothing   -> error "no Authority"
        Just auth -> do
          printf "uriUserInfo: %s\n" $ uriUserInfo auth
          printf "uriRegName: %s\n" $ uriRegName auth
          printf "uriPort: %s\n" $ uriPort auth
      printf "uriPath: %s\n" $ uriPath uri_
      printf "uriFragment: %s\n" $ uriFragment uri_
      printf "uriQuery: %s\n" $ uriQuery uri_
-}

getPortFromURIStr :: Maybe String -> Int
getPortFromURIStr Nothing = defaultPORT
getPortFromURIStr (Just uri) = case parseURI uri of
  Nothing -> error $ printf "invalid redirect uri: %s" uri
  Just uri_ -> case uriAuthority uri_ of
    Nothing -> defaultPORT
    Just auth -> convert (uriPort auth)
  where
    convert :: String -> Int
    convert "" = defaultPORT
    convert ps = read (tail ps)

readServices :: FilePath -> IO Services
readServices pfile = do
  (decodeFileEither pfile :: IO (Either ParseException Services))
    >>= \case
      Left err -> error $ prettyPrintParseException err
      Right ps -> return ps

loadEnvironment :: IO Environment
loadEnvironment = do
  env <- mkEnvironment
  enabled <- isCronEnabled env
  return $ env {system_state = env.system_state {cron_enabled = enabled}}

mkEnvironment :: IO Environment
mkEnvironment = do
  configDir <- D.getXdgDirectory D.XdgConfig "mailctl"
  opts <- customExecParser (prefs showHelpOnEmpty) optsParser
  let cfgOption = optConfig opts
      configFile = if cfgOption == "" then configDir <> "/config.yaml" else cfgOption
  configExists <- D.doesFileExist configFile
  if configExists
    then do
      (decodeFileEither configFile :: IO (Either ParseException Configuration))
        >>= \case
          Left err -> error $ prettyPrintParseException err
          Right cfg' ->
            ( Environment cfg'
                <$> (SystemState <$> getCrontab <*> return False)
                <*> readServices (services_file cfg')
            )
              <*> customExecParser (prefs showHelpOnEmpty) optsParser
    else do
      putStrLn $ "Can't find configuration file: " <> configFile
      exitFailure

getCrontab :: IO (Maybe String)
getCrontab = do
  cronExists <- D.doesFileExist "/usr/bin/crontab"
  if cronExists
    then do
      (x, o, e) <- P.readProcessWithExitCode "crontab" ["-l"] ""
      if x == ExitSuccess
        then do
          let xs = T.lines $ T.pack o
              ys = [y | y <- xs, T.isInfixOf "mailctl" y]
              z = T.concat ys
          return $ Just $ T.unpack z
        else do
          if not (T.isInfixOf "no crontab for" (T.pack e))
            then do
              putStr e
              return Nothing
            else return Nothing
    else return Nothing

isCronEnabled :: Environment -> IO Bool
isCronEnabled env = do
  case env.config.cron_indicator of
    Just cronflag -> D.doesFileExist cronflag
    Nothing -> return False
