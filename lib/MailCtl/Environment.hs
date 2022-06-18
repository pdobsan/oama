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
  , Services(..)
  , serviceLookup 
  , Configuration(..)
  , SystemState(..)
  , Environment(..)
  )
where

import Data.Aeson
import Data.Text qualified as T
import Data.Time.Clock
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
  { fdm_config     :: FilePath
  , fdm_accounts   :: FilePath
  , cron_indicator :: FilePath
  , password_store :: FilePath
  , oauth2_dir     :: FilePath
  , services_file  :: FilePath
  , pass_cmd       :: Program
  , decrypt_cmd    :: Program
  , encrypt_cmd    :: Program
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
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Services = Services [(String, Service)]
  deriving (Show, Generic, ToJSON, FromJSON)

data Environment = Environment
  { config       :: Configuration
  , system_state :: SystemState
  , services     :: Services
  , options      :: Opts
  }
  deriving Show

serviceLookup :: Services -> String -> (Service -> String) -> Maybe String
serviceLookup (Services services_) servName field =
  case lookup servName services_ of
    Nothing -> Nothing
    Just s' -> Just $ field s'

readServices :: FilePath -> IO Services
readServices pfile = do
  ps <- eitherDecodeFileStrict' pfile :: IO (Either String Services)
  case ps of
    Left  err -> error err
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
  opts <- execParser optsParser
  configExists <- D.doesFileExist $ optConfig opts
  if configExists
    then do
      cfg  <- eitherDecodeFileStrict' $ optConfig opts :: IO (Either String Configuration)
      case cfg of
        Left err -> error err
        Right cfg' -> do
          (Environment cfg' <$> (SystemState <$> getCrontab <*> return False)
            <*> readServices (services_file cfg'))
            <*> execParser optsParser
    else do
      putStrLn "Can't find a configuration file."
      putStrLn "This program needs one to work."
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
isCronEnabled env = D.doesFileExist $ cron_indicator $ config env

