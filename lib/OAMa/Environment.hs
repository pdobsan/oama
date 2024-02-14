{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OAMa.Environment (
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
) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Strings (strStartsWith, strDrop)
-- import Data.Text qualified as Text
import Data.Time.Clock
import Data.Yaml qualified as Yaml
import GHC.Generics
import OAMa.CommandLine
import Network.URI (parseURI, uriAuthority, uriPort)
import Options.Applicative (customExecParser, prefs, showHelpOnEmpty)
import System.Directory qualified as Dir
import System.Environment (getEnv, setEnv)
import System.Posix.User (getRealUserID)
import System.Exit (exitFailure)
-- import System.Process qualified as Proc
import Text.Printf

newtype EmailAddress = EmailAddress {unEmailAddress :: String}
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

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
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

data Program = Program
  { exec :: FilePath,
    args :: [String]
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

data Configuration = Configuration
  { services_file :: FilePath,
    ring_store :: Maybe Program,
    ring_lookup :: Maybe Program,
    decrypt_cmd :: Maybe Program,
    encrypt_cmd :: Maybe Program,
    oauth2_dir :: Maybe FilePath
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

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
    tenant :: Maybe String,
    auth_scope :: Maybe String,
    client_id :: Maybe String,
    client_secret :: Maybe String
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

type Services = Map String Service

data Environment = Environment
  { config :: Configuration,
    services :: Services,
    options :: Opts
  }
  deriving (Show)

defaultPORT :: Int
defaultPORT = 8080

serviceFieldLookup :: Services -> String -> (Service -> Maybe String) -> Maybe String
serviceFieldLookup services_ servName field = Map.lookup servName services_ >>= field

getPortFromURIStr :: Maybe String -> Int
getPortFromURIStr Nothing = defaultPORT
getPortFromURIStr (Just uri) = case parseURI uri of
  Nothing -> error $ printf "invalid redirect uri: %s" uri
  Just uri_ -> case uriAuthority uri_ of
    Nothing -> defaultPORT
    Just auth -> convert auth.uriPort
  where
    convert :: String -> Int
    convert "" = defaultPORT
    convert ps = read (drop 1 ps)

loadEnvironment :: IO Environment
loadEnvironment = do
  uid <- getRealUserID
  setEnv "DBUS_SESSION_BUS_ADDRESS" ("unix:path=/run/user/" ++ show uid ++ "/bus")
  mkEnvironment

mkEnvironment :: IO Environment
mkEnvironment = do
  configDir <- Dir.getXdgDirectory Dir.XdgConfig "oama"
  opts <- customExecParser (prefs showHelpOnEmpty) optsParser
  let cfgOption = optConfig opts
      configFile = if cfgOption == "" then configDir <> "/config.yaml" else cfgOption
  cfg <- readConfig configFile
  hd <- getEnv "HOME"
  let cfg' = cfg{ services_file = expandTilde cfg.services_file hd,
                  oauth2_dir = expandTilde_ cfg.oauth2_dir hd
                }
  Environment cfg'
    <$> readServices cfg'.services_file
    <*> customExecParser (prefs showHelpOnEmpty) optsParser

expandTilde :: FilePath -> FilePath -> FilePath
expandTilde cpath homeDir =
  if strStartsWith cpath "~"
    then homeDir <> strDrop 1 cpath
    else cpath

expandTilde_ :: Maybe FilePath -> FilePath -> Maybe FilePath
expandTilde_ (Just cpath) homeDir = Just $ expandTilde cpath homeDir
expandTilde_ Nothing _ = Nothing

isFileReadable :: FilePath -> IO Bool
isFileReadable file = do
  Dir.doesFileExist file
  >>= \case
    True -> do
      perms <- Dir.getPermissions file
      return $ Dir.readable perms
    False -> return False

readConfig :: FilePath -> IO Configuration
readConfig configFile = do
  readable <- isFileReadable configFile
  if readable
    then
      (Yaml.decodeFileEither configFile :: IO (Either Yaml.ParseException Configuration))
        >>= \case
          Left err -> error $ Yaml.prettyPrintParseException err
          Right cfg -> return cfg
    else do
      putStrLn $ "Can't find/read configuration file: " <> configFile
      exitFailure

readServices :: FilePath -> IO Services
readServices servicesFile = do
  readable <- isFileReadable servicesFile
  if readable
    then
      (Yaml.decodeFileEither servicesFile :: IO (Either Yaml.ParseException Services))
        >>= \case
          Left err -> error $ Yaml.prettyPrintParseException err
          Right ps -> return ps
    else do
      putStrLn $ "Can't find/read services file: " <> servicesFile
      exitFailure
