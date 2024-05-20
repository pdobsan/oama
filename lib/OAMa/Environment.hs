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
  EmailAddress (..),
  HTTPMethod (..),
  ParamsMode (..),
  checkInit,
  loadEnvironment,
  getServiceAPI,
  pprintEnv,
  printTemplate,
  logger,
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.ByteString.UTF8 qualified as BSU
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.String.QQ
import Data.Strings (strDrop, strStartsWith)
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
  { auth_endpoint :: Maybe String
  , auth_http_method :: Maybe HTTPMethod
  , auth_params_mode :: Maybe ParamsMode
  , token_endpoint :: Maybe String
  , token_http_method :: Maybe HTTPMethod
  , token_params_mode :: Maybe ParamsMode
  , auth_scope :: Maybe String
  , tenant :: Maybe String
  , client_id :: String
  , client_secret :: String
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

type Services = Map String ServiceAPI

defaultServices :: Services
defaultServices =
  Map.fromList
    [
      ( "google"
      , ServiceAPI
          { auth_endpoint = Just "https://accounts.google.com/o/oauth2/auth"
          , auth_http_method = Just POST
          , auth_params_mode = Just QueryString
          , token_endpoint = Just "https://accounts.google.com/o/oauth2/token"
          , token_http_method = Just POST
          , token_params_mode = Just RequestBody
          , auth_scope = Just "https://mail.google.com/"
          , tenant = Nothing
          , client_id = "application-CLIENT-ID"
          , client_secret = "application-CLIENT-SECRET"
          }
      )
    ,
      ( "microsoft"
      , ServiceAPI
          { auth_endpoint = Just "https://login.microsoftonline.com/common/oauth2/v2.0/authorize"
          , auth_http_method = Just GET
          , auth_params_mode = Just QueryString
          , token_endpoint = Just "https://login.microsoftonline.com/common/oauth2/v2.0/token"
          , token_http_method = Just POST
          , token_params_mode = Just RequestBodyForm
          , auth_scope =
              Just $
                "https://outlook.office365.com/IMAP.AccessAsUser.All "
                  ++ "https://outlook.office365.com/SMTP.Send "
                  ++ "offline_access"
          , tenant = Just "common"
          , client_id = "application-CLIENT-ID"
          , client_secret = "application-CLIENT-SECRET"
          }
      )
    ]

{-|
Update defaultServices with the values read from the config file. Merge fields
of Maybe type with <|>, for other fields use the config's values.
-}
updateServices :: Configuration -> Services
updateServices conf =
  let servs = zip (Map.toList conf.services) (Map.toList defaultServices)
   in -- x :: ((String, ServiceAPI), (String, ServiceAPI))
      Map.fromList [(fst (snd x), updateServiceAPI (snd (fst x)) (snd (snd x))) | x <- servs]
 where
  updateServiceAPI :: ServiceAPI -> ServiceAPI -> ServiceAPI
  updateServiceAPI cfg def =
    ServiceAPI
      { auth_endpoint = cfg.auth_endpoint <|> def.auth_endpoint
      , auth_http_method = cfg.auth_http_method <|> def.auth_http_method
      , auth_params_mode = cfg.auth_params_mode <|> def.auth_params_mode
      , token_endpoint = cfg.token_endpoint <|> def.token_endpoint
      , token_http_method = cfg.token_http_method <|> def.token_http_method
      , token_params_mode = cfg.token_params_mode <|> def.token_params_mode
      , auth_scope = cfg.auth_scope <|> def.auth_scope
      , tenant = cfg.tenant <|> def.tenant
      , client_id = cfg.client_id
      , client_secret = cfg.client_secret
      }

defaultPort :: Int
defaultPort = 8080

-- | Structure of the configuration YAML file
data Configuration = Configuration
  { encryption :: Encryption
  , redirect_port :: Maybe Int
  , services :: Services
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

data Environment = Environment
  { oama_version :: String
  , op_sys :: String
  , data_dir :: String
  , config_file :: String
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
  (defaultConfigFile, dataDir) <- checkInit
  opts <- customExecParser (prefs showHelpOnEmpty) optsParser
  defaultOptsConfig <- expandTilde opts.optConfig
  opsys <- uname
  let configFile =
        if defaultOptsConfig == defaultConfigFile
          then defaultConfigFile
          else defaultOptsConfig
  cfg <- readConfig configFile :: IO Configuration
  when (cfg.encryption == GRING) $ do
    uid <- getRealUserID
    -- gnome needs this envvar set
    setEnv "DBUS_SESSION_BUS_ADDRESS" ("unix:path=/run/user/" ++ show uid ++ "/bus")
  return
    Environment
      { oama_version = showVersion version
      , op_sys = opsys
      , data_dir = dataDir
      , config_file = configFile
      , config = cfg {redirect_port = cfg.redirect_port <|> Just defaultPort}
      , services = updateServices cfg
      , options = opts
      }

expandTilde :: FilePath -> IO FilePath
expandTilde inpath = do
  homeDir <- getEnv "HOME"
  if strStartsWith inpath "~"
    then return $ homeDir <> strDrop 1 inpath
    else Dir.makeAbsolute inpath

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
    then return (defaultConfigFile, dataDir)
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

pprintEnv :: Environment -> IO ()
pprintEnv env = do
  let envYaml = BSU.toString $ Yaml.encode env
  putStrLn "###  Runtime environment  ###"
  putStr envYaml
  putStrLn "######"

uname :: IO String
uname = do
  (x, o, e) <- Proc.readProcessWithExitCode "uname" ["-a"] ""
  if x == ExitSuccess
    then return o
    else return $ "Unknown operating system.\n" <> e

getServiceAPI :: Environment -> String -> ServiceAPI
getServiceAPI env serv = fromJust (Map.lookup serv env.services)

logger :: Priority -> String -> IO ()
logger pri msg = withCStringLen msg $ syslog Nothing pri

printTemplate :: IO ()
printTemplate = putStr initialConfig

initialConfig :: String
initialConfig =
  "## oama version "
    <> showVersion version
    ++ [s|


## This is a YAML configuration file, indentation matters.
## Double ## indicates comments while single # default values.

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
## The only required arguments are client_id and client_secret
##
services:
  google:
    client_id: application-CLIENT-ID 
    client_secret: application-CLIENT-SECRET
  #  auth_scope: https://mail.google.com/

  # microsoft:
  #   client_id: application-CLIENT-ID 
  #   client_secret: application-CLIENT_SECRET
  #   auth_scope: https://outlook.office365.com/IMAP.AccessAsUser.All
  #     https://outlook.office365.com/SMTP.Send
  #     offline_access
  #   tenant: common
|]
