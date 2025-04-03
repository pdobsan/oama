{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module OAMa.Environment
  ( AuthRecord (..),
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
  )
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Crypto.Manager (secretMethod)
import Data.ByteString.UTF8 qualified as BSU
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.String.QQ
import Data.Strings (sReplace, strDrop, strStartsWith)
import Data.Time.Clock
import Data.Version (showVersion)
import Data.Yaml qualified as Yaml
import Foreign.C.String
import GHC.Generics
import OAMa.CommandLine
import Options.Applicative (customExecParser, prefs, showHelpOnEmpty)
import Paths_oama (version)
import System.Directory qualified as Dir
import System.Environment (getEnv, lookupEnv, setEnv)
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import System.Posix.Syslog (Priority (..), syslog)
import System.Posix.User (getRealUserID)
import System.Process qualified as Proc
import Text.Printf

data Encryption = GPG String | KEYRING
  deriving (Eq, Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

data ParamsMode = RequestBody | RequestBodyForm | QueryString
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

data HTTPMethod = POST | GET
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

data ServiceAPI = ServiceAPI
  { auth_endpoint :: Maybe String,
    auth_http_method :: Maybe HTTPMethod,
    auth_params_mode :: Maybe ParamsMode,
    token_endpoint :: Maybe String,
    token_http_method :: Maybe HTTPMethod,
    token_params_mode :: Maybe ParamsMode,
    auth_scope :: Maybe String,
    redirect_uri :: Maybe String,
    tenant :: Maybe String,
    access_type :: Maybe String,
    prompt :: Maybe String,
    client_id :: String,
    client_secret :: Maybe String
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

-- defualtRedirectURI :: String
-- defualtRedirectURI = "http://localhost:8080"

defaultServiceAPI :: ServiceAPI
defaultServiceAPI =
  ServiceAPI
    { auth_endpoint = Nothing,
      auth_http_method = Just POST,
      auth_params_mode = Just QueryString,
      token_endpoint = Nothing,
      token_http_method = Just POST,
      token_params_mode = Just RequestBody,
      auth_scope = Nothing,
      redirect_uri = Nothing,
      tenant = Nothing,
      access_type = Nothing,
      prompt = Nothing,
      client_id = "application-CLIENT-ID",
      client_secret = Just "application-CLIENT-SECRET"
    }

type Services = Map String ServiceAPI

builtinServices :: Services
builtinServices =
  Map.fromList
    [ ( "google",
        defaultServiceAPI
          { auth_endpoint = Just "https://accounts.google.com/o/oauth2/auth",
            token_endpoint = Just "https://accounts.google.com/o/oauth2/token",
            auth_scope = Just "https://mail.google.com/",
            access_type = Just "offline",
            prompt = Just "consent",
            client_id = "application-CLIENT-ID",
            client_secret = Just "application-CLIENT-SECRET"
          }
      ),
      ( "microsoft",
        defaultServiceAPI
          { auth_endpoint = Just "https://login.microsoftonline.com/common/oauth2/v2.0/authorize",
            auth_http_method = Just GET,
            token_params_mode = Just RequestBodyForm,
            token_endpoint = Just "https://login.microsoftonline.com/common/oauth2/v2.0/token",
            auth_scope =
              Just
                "https://outlook.office.com/IMAP.AccessAsUser.All https://outlook.office.com/POP.AccessAsUser.All https://outlook.office.com/SMTP.Send offline_access",
            tenant = Just "common",
            client_id = "application-CLIENT-ID",
            client_secret = Just "application-CLIENT-SECRET"
          }
      )
    ]

-- defaultPort :: Int
-- defaultPort = 8080

-- | Structure of the configuration YAML file
data Configuration = Configuration
  { encryption :: Encryption,
    services :: Services
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

data Environment = Environment
  { oama_version :: String,
    op_sys :: String,
    state_dir :: String,
    config_file :: String,
    config :: Configuration,
    services :: Services,
    options :: Opts
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

newtype EmailAddress = EmailAddress {unEmailAddress :: String}
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

data AuthRecord = AuthRecord
  { email :: Maybe EmailAddress,
    service :: Maybe String,
    scope :: Maybe String,
    refresh_token :: Maybe String,
    access_token :: String,
    token_type :: String,
    exp_date :: Maybe String,
    expires_in :: NominalDiffTime
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

-- |
-- Update defaultServiceAPI with the values read from the config file. Merge fields
-- of Maybe type with Applicative.(<|>) (short circuting on config values), for
-- other fields use the config's values.
-- Return the new merged ServiceAPI record.
updateServiceAPI :: ServiceAPI -> ServiceAPI -> ServiceAPI
updateServiceAPI def cfg =
  ServiceAPI
    { auth_endpoint = cfg.auth_endpoint <|> def.auth_endpoint,
      auth_http_method = cfg.auth_http_method <|> def.auth_http_method,
      auth_params_mode = cfg.auth_params_mode <|> def.auth_params_mode,
      token_endpoint = cfg.token_endpoint <|> def.token_endpoint,
      token_http_method = cfg.token_http_method <|> def.token_http_method,
      token_params_mode = cfg.token_params_mode <|> def.token_params_mode,
      auth_scope = cfg.auth_scope <|> def.auth_scope,
      redirect_uri = cfg.redirect_uri <|> def.redirect_uri,
      tenant = cfg.tenant <|> def.tenant,
      access_type = cfg.access_type <|> def.access_type,
      prompt = cfg.prompt <|> def.prompt,
      client_id = cfg.client_id,
      client_secret = cfg.client_secret
    }

-- replace ".../common/..." with ".../tenant/..." in endpoints
adjustEndpoints :: ServiceAPI -> ServiceAPI
adjustEndpoints ms =
  let tenant' = fromJust ms.tenant
      auth_endpoint' = sReplace "common" tenant' (fromJust ms.auth_endpoint)
      token_endpoint' = sReplace "common" tenant' (fromJust ms.token_endpoint)
   in ms {auth_endpoint = Just auth_endpoint', token_endpoint = Just token_endpoint'}

getConfiguredServices :: Configuration -> Services
getConfiguredServices conf =
  let cfgServers = Map.toList conf.services
      cfgBuiltins = [(name, Map.lookup name builtinServices) | (name, _) <- cfgServers]
      mergedServers = zipWith update cfgServers cfgBuiltins
      servs = Map.fromList mergedServers
   in case Map.lookup "microsoft" servs of
        Just _ -> Map.adjust adjustEndpoints "microsoft" servs
        Nothing -> servs
  where
    update :: (String, ServiceAPI) -> (String, Maybe ServiceAPI) -> (String, ServiceAPI)
    update (name, configured) (_, Just builtin) = (name, updateServiceAPI builtin configured)
    update (name, configured) (_, Nothing) = (name, updateServiceAPI defaultServiceAPI configured)

loadEnvironment :: IO Environment
loadEnvironment = do
  (defaultConfigFile, stateDir) <- checkInit
  opts <- customExecParser (prefs showHelpOnEmpty) optsParser
  defaultOptsConfig <- expandTilde opts.optConfig
  opsys <- uname
  let configFile =
        if defaultOptsConfig == defaultConfigFile
          then defaultConfigFile
          else defaultOptsConfig
  cfg <- readConfig configFile :: IO Configuration
  when (cfg.encryption == KEYRING) $ do
    -- libsecret based keyrings need this envvar set
    dbus <- lookupEnv "DBUS_SESSION_BUS_ADDRESS"
    when (dbus == Nothing) $ do
      uid <- getRealUserID
      setEnv "DBUS_SESSION_BUS_ADDRESS" ("unix:path=/run/user/" ++ show uid ++ "/bus")
  return
    Environment
      { oama_version = showVersion version,
        op_sys = opsys,
        state_dir = stateDir,
        config_file = configFile,
        config = cfg,
        services = getConfiguredServices cfg,
        options = opts
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
  stateDir <- Dir.getXdgDirectory Dir.XdgState "oama"
  Dir.createDirectoryIfMissing True configDir
  Dir.createDirectoryIfMissing True stateDir
  let defaultConfigFile = configDir <> "/config.yaml"
  cfgOK <- isFileReadable defaultConfigFile
  if cfgOK
    then return (defaultConfigFile, stateDir)
    else do
      printf "WARNING -- Could not find config file: %s\n" defaultConfigFile
      printf "Creating initial config file ...\n"
      logger Warning $ printf "WARNING -- Could not find config file: %s\n" defaultConfigFile
      logger Warning $ printf "Creating initial config file ...\n"
      writeFile defaultConfigFile initialConfig
      printf "... done.\n"
      logger Warning $ printf "... done.\n"
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
      printf "Can't find/read configuration file: %s\n" configFile
      logger Error $ printf "Can't find/read configuration file: %s\n" configFile
      exitFailure

pprintEnv :: Environment -> IO ()
pprintEnv env = do
  let envYaml = BSU.toString $ Yaml.encode env
  printf "### Secret Management Method: %s\n" secretMethod
  putStrLn "###  Runtime environment  ###"
  putStr envYaml
  putStrLn "######"

uname :: IO String
uname = do
  (x, o, e) <- Proc.readProcessWithExitCode "uname" ["-a"] ""
  if x == ExitSuccess
    then return o
    else return $ "Unknown operating system.\n" <> e

getServiceAPI :: Environment -> String -> IO ServiceAPI
getServiceAPI env serv = foo (Map.lookup serv env.services)
  where
    foo :: Maybe ServiceAPI -> IO ServiceAPI
    foo (Just servapi) = return servapi
    foo Nothing = do
      printf "ERROR - No service named '%s' is configured.\n" serv
      printf "        Run`oama printenv` and check its output.\n"
      logger Error $ printf "ERROR - No service named '%s' is configured.\n" serv
      exitFailure

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
## Not all defaults are shown, for full list run `oama printenv`
## and look at the `services:` section.

## Possible options for keeping refresh and access tokens:
## GPG - in a gpg encrypted file $XDG_STATE_HOME/oama/<email-address>.oauth
##       (XDG_STATE_HOME defaults to ~/.local/state)
## GPG - in a gpg encrypted file ~/.local/state/oama/<email-address>.oauth
## KEYRING - in the keyring of a password manager with Secret Service API
##
## Choose exactly one.

encryption:
    tag: KEYRING

# encryption:
#   tag: GPG
#   contents: your-KEY-ID

## Builtin service providers
## - google
## - microsoft
## Required fields: client_id, client_secret
##
services:
  google:
    client_id: application-CLIENT-ID 
    client_secret: application-CLIENT-SECRET
  #  auth_scope: https://mail.google.com/

  # microsoft:
  #   client_id: application-CLIENT-ID 
  #   client_secret: application-CLIENT_SECRET
  #   auth_scope: https://outlook.office.com/IMAP.AccessAsUser.All
  #     https://outlook.office.com/SMTP.Send
  #     offline_access
  #   tenant: common

  ## User configured providers
  ## Required fields: client_id, client_secret, auth_endpoint, auth_scope, token_endpoint  
  ##
  ## For example:
  # yahoo:
  #   client_id: application-CLIENT-ID 
  #   client_secret: application-CLIENT_SECRET
  #   auth_endpoint: EDIT-ME!
  #   auth_scope: EDIT-ME!
  #   token_endpoint: EDIT-ME!
|]
