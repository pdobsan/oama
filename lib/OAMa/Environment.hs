{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module OAMa.Environment (
  AuthRecord (..),
  Configuration (..),
  Environment (..),
  ServiceAPI (..),
  Encryption (..),
  Credentials (..),
  EmailAddress (..),
  HTTPMethod (..),
  ParamsMode (..),
  checkInit,
  loadEnvironment,
  credentialLookup,
  getServiceAPI,
  pprintEnv,
  logger,
) where

import Control.Applicative ((<|>))
import Data.ByteString.UTF8 qualified as BSU
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.String.QQ
import Data.Time.Clock
import Data.Version (showVersion)
import Data.Yaml qualified as Yaml
import GHC.Generics
import OAMa.CommandLine
import Options.Applicative (customExecParser, prefs, showHelpOnEmpty)
import Paths_oama (version)
import System.Directory qualified as Dir
import System.Environment (getEnv, setEnv)
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import System.Posix.User (getRealUserID)
import System.Process qualified as Proc

import Foreign.C.String
import System.Posix.Syslog (Priority (..), syslog)

data Encryption = GPG String | GRING
  deriving (Eq, Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

data ParamsMode = RequestBody | RequestBodyForm | QueryString
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)
data HTTPMethod = POST | GET
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

data ServiceAPI = ServiceAPI
  { auth_endpoint :: String
  , auth_http_method :: HTTPMethod
  , auth_params_mode :: ParamsMode
  , token_endpoint :: String
  , token_http_method :: HTTPMethod
  , token_params_mode :: ParamsMode
  , auth_scope :: String
  , tenant :: Maybe String
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

googleAPI :: ServiceAPI
googleAPI =
  ServiceAPI
    { auth_endpoint = "https://accounts.google.com/o/oauth2/auth"
    , auth_http_method = POST
    , auth_params_mode = QueryString
    , token_endpoint = "https://accounts.google.com/o/oauth2/token"
    , token_http_method = POST
    , token_params_mode = RequestBody
    , auth_scope = "https://mail.google.com/"
    , tenant = Nothing
    }

microsoftAPI :: ServiceAPI
microsoftAPI =
  ServiceAPI
    { auth_endpoint = "https://login.microsoftonline.com/common/oauth2/v2.0/authorize"
    , auth_http_method = GET
    , auth_params_mode = QueryString
    , token_endpoint = "https://login.microsoftonline.com/common/oauth2/v2.0/token"
    , token_http_method = POST
    , token_params_mode = RequestBodyForm
    , auth_scope =
        "https://outlook.office365.com/IMAP.AccessAsUser.All https://outlook.office365.com/SMTP.Send offline_access"
    , tenant = Just "common"
    }

defaultPort :: Int
defaultPort = 8080

type Services = Map String ServiceAPI

data Credentials = Credentials
  { client_id :: String
  , client_secret :: String
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

data Configuration = Configuration
  { encryption :: Encryption
  , redirect_port :: Maybe Int
  , credentials :: Map String Credentials
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

initialConfig :: String
initialConfig =
  [s|
# oama configuration

## Possible options for keeping refresh and access tokens:
## GPG - in a gpg encrypted file ~/.local/var/oama/<email-address>.oauth
## GRING - in the default Gnome keyring
##
## Choose exactly one.

encryption:
    tag: GRING

# encryption:
#   tag: GPG
#   contents: your-KEY-ID

## It must be >= 1024
# redirect_port: 8080

## Possible service providers
## - google
## - microsoft
## Use your own credentials or the ones of an opensource app (like thunderbird ...)
credentials:
  google:
    client_id: application-CLIENT-ID 
    client_secret: application-CLIENT-SECRET
  # microsoft:
  #   client_id: application-CLIENT-ID 
  #   client_secret: application-CLIENT_SECRET
|]

data Environment = Environment
  { oama_version :: String
  , op_sys :: String
  , data_dir :: String
  , config_dir :: String
  , config :: Configuration
  , services :: Services
  , options :: Opts
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

newtype EmailAddress = EmailAddress {unEmailAddress :: String}
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

data AuthRecord = AuthRecord
  { access_token :: String
  , expires_in :: NominalDiffTime
  , scope :: String
  , token_type :: String
  , exp_date :: Maybe String
  , refresh_token :: Maybe String
  , email :: Maybe EmailAddress
  , service :: Maybe String
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

loadEnvironment :: IO Environment
loadEnvironment = do
  (configDir, dataDir) <- checkInit
  opsys <- uname
  opts <- customExecParser (prefs showHelpOnEmpty) optsParser
  let configFile = configDir <> "/config.yaml"
  cfg <- readConfig configFile :: IO Configuration
  if cfg.encryption == GRING
    then do
      uid <- getRealUserID
      -- gnome needs this envvar set
      setEnv "DBUS_SESSION_BUS_ADDRESS" ("unix:path=/run/user/" ++ show uid ++ "/bus")
    else return ()
  return
    Environment
      { oama_version = showVersion version
      , op_sys = opsys
      , data_dir = dataDir
      , config_dir = configDir
      , config = cfg {redirect_port = cfg.redirect_port <|> Just defaultPort}
      , services = Map.fromList [("google", googleAPI), ("microsoft", microsoftAPI)]
      , options = opts
      }

pprintEnv :: Environment -> IO ()
pprintEnv env = do
  let envYaml = BSU.toString $ Yaml.encode $ env
  putStrLn "###  Runtime environment  ###"
  putStr envYaml
  putStrLn "######"

uname :: IO String
uname = do
  (x, o, e) <- Proc.readProcessWithExitCode "uname" ["-a"] ""
  if x == ExitSuccess
    then return o
    else return $ "Unknown operating system.\n" <> e

checkInit :: IO (String, String)
checkInit = do
  configDir <- Dir.getXdgDirectory Dir.XdgConfig "oama"
  homeDir <- getEnv "HOME"
  let dataDir = homeDir ++ "/.local/var/oama"
  Dir.createDirectoryIfMissing True configDir
  Dir.createDirectoryIfMissing True dataDir
  let defaultConfigFile = configDir <> "/config.yaml"
  cfgOK <- isFileReadable defaultConfigFile
  if cfgOK
    then return (configDir, dataDir)
    else do
      putStrLn $ "WARNING -- Could not find config file: " <> defaultConfigFile
      putStrLn "Creating initial config file ..."
      writeFile defaultConfigFile initialConfig
      putStrLn "Edit it then start oama again."
      exitFailure

isFileReadable :: FilePath -> IO Bool
isFileReadable file =
  do
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

credentialLookup :: Environment -> String -> (Credentials -> String) -> Maybe String
credentialLookup env serv field =
  case Map.lookup serv env.config.credentials of
    Nothing -> Nothing
    Just cred -> Just (field cred)

getServiceAPI :: Environment -> String -> ServiceAPI
getServiceAPI env serv = fromJust (Map.lookup serv env.services)

logger :: Priority -> String -> IO ()
logger pri msg = withCStringLen msg $ syslog Nothing pri
