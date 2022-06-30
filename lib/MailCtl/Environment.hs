{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module MailCtl.Environment
  ( loadEnvironment
  , EmailAddress(..)
  , AuthRecord(..)
  , Program(..)
  , Service(..)
  , Services
  , serviceFieldLookup 
  , Configuration(..)
  , SystemState(..)
  , Environment(..)
  )
where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time.Clock
import Data.Yaml
import GHC.Generics
import MailCtl.CommandLine
import Options.Applicative (execParser)
import System.Directory qualified as D
import System.Exit (ExitCode (ExitSuccess), exitWith, exitFailure)    
import System.Process qualified as P    

newtype EmailAddress = EmailAddress { unEmailAddress :: String }
  deriving (Show, Generic, ToJSON, FromJSON)

data AuthRecord = AuthRecord
  { access_token  :: String
  , expires_in    :: NominalDiffTime
  , scope         :: String
  , token_type    :: String
  , exp_date      :: Maybe String
  , refresh_token :: Maybe String
  , email         :: Maybe EmailAddress
  , service       :: Maybe String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Program = Program
  { exec :: FilePath
  , args :: [String]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Configuration = Configuration
  { services_file  :: FilePath
  , decrypt_cmd    :: Program
  , encrypt_cmd    :: Program
  , oauth2_dir     :: FilePath
  , fdm_config     :: Maybe FilePath
  , fdm_accounts   :: Maybe FilePath
  , cron_indicator :: Maybe FilePath
  , password_store :: Maybe FilePath
  , pass_cmd       :: Maybe Program
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data SystemState = SystemState
  { crontab      :: String
  , cron_enabled :: Bool
  }
  deriving (Show, Generic, ToJSON)

data Service = Service
  { auth_endpoint  :: String
  , token_endpoint :: String
  , redirect_uri   :: String
  , auth_scope     :: String
  , client_id      :: String
  , client_secret  :: String
  , tenant         :: Maybe String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

type Services = Map String Service

data Environment = Environment
  { config       :: Configuration
  , system_state :: SystemState
  , services     :: Services
  , options      :: Opts
  }
  deriving Show

serviceFieldLookup :: Services -> String -> (Service -> String) -> Maybe String
serviceFieldLookup services_ servName field =
  case M.lookup servName services_ of
    Nothing -> Nothing
    Just s' -> Just $ field s'

readServices :: FilePath -> IO Services
readServices pfile = do
  ps <- decodeFileEither pfile :: IO (Either ParseException Services)
  case ps of
    Left  err -> error $ prettyPrintParseException err
    Right ps' -> return ps'

loadEnvironment :: IO Environment
loadEnvironment = do
  env     <- mkEnvironment
  enabled <- isCronEnabled env
  let ss  = system_state env
      ss' = ss { cron_enabled = enabled }
  return $ env { system_state = ss' }

mkEnvironment :: IO Environment
mkEnvironment = do
  configDir <- D.getXdgDirectory D.XdgConfig "mailctl"
  opts <- execParser optsParser
  let cfgOption = optConfig opts
      configFile = if cfgOption == "" then configDir <> "/config.yaml" else cfgOption
  configExists <- D.doesFileExist configFile
  if configExists
    then do
      cfg  <- decodeFileEither configFile :: IO (Either ParseException Configuration)
      case cfg of
        Left err -> error $ prettyPrintParseException err
        Right cfg' ->
          (Environment cfg' <$> (SystemState <$> getCrontab <*> return False)
            <*> readServices (services_file cfg'))
            <*> execParser optsParser
    else do
      putStrLn $ "Can't find configuration file: " <> configFile
      exitFailure

getCrontab :: IO String
getCrontab = do
  (x, o, e) <- P.readProcessWithExitCode "crontab" ["-l"] ""
  if x == ExitSuccess
    then do
      let xs = T.lines $ T.pack o
          ys = [ y | y <- xs, T.isInfixOf "mailctl" y ]
          z  = T.concat ys
      return $ T.unpack z
    else do
      putStr e
      exitWith x

isCronEnabled :: Environment -> IO Bool
isCronEnabled env = do
  case cron_indicator $ config env of
    Just cron_indicator' -> D.doesFileExist cron_indicator'
    Nothing -> return False

