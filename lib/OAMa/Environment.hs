{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OAMa.Environment (
  AuthRecord (..),
  AuthError (..),
  DeviceAuthResponse (device_code, user_code, verification_uri, verification_uri_complete, interval),
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
  fatalError,
)
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Crypto.Manager (secretMethod)
import Data.Aeson.Types (
  Options (allNullaryToStringTag, constructorTagModifier, sumEncoding),
  SumEncoding (..),
  camelTo2,
  defaultOptions,
  genericParseJSON,
 )
import Data.ByteString.UTF8 qualified as BSU
import Data.Char (isSpace)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
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
    client_id :: Maybe String,
    client_id_cmd :: Maybe String,
    client_secret :: Maybe String,
    client_secret_cmd :: Maybe String
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

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
      client_id = Nothing,
      client_id_cmd = Nothing,
      client_secret = Nothing,
      client_secret_cmd = Nothing
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
            prompt = Just "consent"
          }
      ),
      ( "microsoft",
        defaultServiceAPI
          { auth_endpoint = Just "https://login.microsoftonline.com/common/oauth2/v2.0/devicecode",
            -- auth_endpoint = Just "https://login.microsoftonline.com/common/oauth2/v2.0/authorize",
            auth_http_method = Just GET,
            token_params_mode = Just RequestBodyForm,
            token_endpoint = Just "https://login.microsoftonline.com/common/oauth2/v2.0/token",
            auth_scope =
              Just
                "https://outlook.office.com/IMAP.AccessAsUser.All https://outlook.office.com/POP.AccessAsUser.All https://outlook.office.com/SMTP.Send offline_access",
            tenant = Just "common"
          }
      )
    ]

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

data AuthError
  = InvalidGrant
  | InvalidClient
  | InvalidRequest
  | UnauthorizedClient
  | AccessDenied
  | UnsupportedResponseType
  | InvalidScope
  | ServerError
  | TemporarilyUnavailable
  | AuthorizationPending
  | SlowDown
  | ExpiredToken
  | Unknown String
  deriving (Show, Generic)

instance Yaml.FromJSON AuthError where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = camelTo2 '_',
          allNullaryToStringTag = False,
          sumEncoding = TaggedObject "error" "raw"
        }

data DeviceAuthResponse = DeviceAuthResponse
  { device_code :: String,
    user_code :: String,
    verification_uri :: String,
    verification_uri_complete :: Maybe String,
    expires_in :: NominalDiffTime,
    interval :: Maybe Int
  }
  deriving (Show, Generic, Yaml.FromJSON)

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
      client_id_cmd = cfg.client_id_cmd,
      client_secret = cfg.client_secret,
      client_secret_cmd = cfg.client_secret_cmd
    }

-- replace ".../common/..." with ".../tenant/..." in endpoints
adjustEndpoints :: ServiceAPI -> ServiceAPI
adjustEndpoints ms =
  let tenant' = fromJust ms.tenant
      auth_endpoint' = sReplace "common" tenant' (fromJust ms.auth_endpoint)
      token_endpoint' = sReplace "common" tenant' (fromJust ms.token_endpoint)
   in ms{auth_endpoint = Just auth_endpoint', token_endpoint = Just token_endpoint'}

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

lstrip :: String -> String
lstrip = dropWhile isSpace

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = lstrip . rstrip

execCmd :: Maybe String -> IO (Maybe String)
execCmd (Just cmd) = do
  let shell = Proc.shell cmd
  (x, o, e) <- Proc.readCreateProcessWithExitCode shell ""
  if x == ExitSuccess
    then return (Just $ strip o)
    else fatalError "execCmd" (printf "Failed to execute shell command: %s\n%s" cmd e)
execCmd Nothing = pure Nothing

-- | process id/secret_cmd-s in all configured services
processCmds :: Configuration -> IO Services
processCmds cfg = do
  let servs = getConfiguredServices cfg
  mapM updateIdSecret servs
 where
  updateIdSecret :: ServiceAPI -> IO ServiceAPI
  updateIdSecret sapi = do
    idCmdResult <- execCmd sapi.client_id_cmd
    secretCmdResult <- execCmd sapi.client_secret_cmd
    -- id/secret fields are updated with CmdResult-s if any
    -- priority given to CmdResult-s
    -- _cmd fields are ereased
    return
      sapi
        { client_id = idCmdResult <|> sapi.client_id,
          client_id_cmd = Nothing,
          client_secret = secretCmdResult <|> sapi.client_secret,
          client_secret_cmd = Nothing
        }

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
  services_ <- processCmds cfg
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
        -- services = getConfiguredServices cfg,
        services = services_,
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
          Left err -> fatalError "readConfing" (show $ Yaml.prettyPrintParseException err)
          Right cfg -> return cfg
    else do
      fatalError "readConfig" (printf "Can't find/read configuration file: %s" configFile)

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
  foo Nothing = fatalError "getServiceAPI" (printf "No service named '%s' is configured." serv)

logger :: Priority -> String -> IO ()
logger pri msg = withCStringLen msg $ syslog Nothing pri

fatalError :: String -> String -> IO a
fatalError caller errmsg = do
  printf "%s: %s\n" caller errmsg
  logger Error $ printf "%s: %s" caller errmsg
  exitFailure

printTemplate :: IO ()
printTemplate = putStr initialConfig

initialConfig :: String
initialConfig =
  "## oama version "
    <> showVersion version
    <> """

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
         ## Alternatively get them from a password manager using a shell command.
         ## If both variants are present then the _cmd versions get the priority.
         ## For example:
         # client_id_cmd: |
         #   pass email/my-app | head -1
         # client_secret_cmd: |
         #   pass email/my-app | head -2 | tail -1
         #  auth_scope: https://mail.google.com/

         microsoft:
            client_id: application-CLIENT-ID
         ## client_secret is not needed for device code flow
         #  auth_endpoint: https://login.microsoftonline.com/common/oauth2/v2.0/devicecode
         ##
         ## client_secret might be needed for other authorization flows
         #  client_secret: application-CLIENT_SECRET
         ## auth_endpoint: https://login.microsoftonline.com/common/oauth2/v2.0/authorize
         #
         #  auth_scope: https://outlook.office.com/IMAP.AccessAsUser.All
         #     https://outlook.office.com/SMTP.Send
         #     offline_access
         #  tenant: common

         ## User configured providers
         ## Required fields: client_id, client_secret, auth_endpoint, auth_scope, token_endpoint
         ##
         ## For example:
         # yahoo:
         #   client_id: application-CLIENT-ID
         #   client_id_cmd: |
         #     password manager command ...
         #   client_secret: application-CLIENT_SECRET
         #   client_secret_cmd: |
         #     password manager command ...
         #   auth_endpoint: EDIT-ME!
         #   auth_scope: EDIT-ME!
         #   token_endpoint: EDIT-ME!

       """
