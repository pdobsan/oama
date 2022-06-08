{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module MailCtl.Environment
  ( loadEnvironment
  , Program(..)
  , Google(..)
  , Configuration(..)
  , SystemState(..)
  , Environment(..)
  )
where

import Data.Aeson
import Data.Text qualified as T
import System.Directory qualified as D
import GHC.Generics
import MailCtl.CommandLine
import Options.Applicative (execParser)
import System.Exit (ExitCode (ExitSuccess), exitWith)    
import System.Process qualified as P    

data Program = Program
  { exec :: FilePath
  , args :: [String]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Google = Google
  { auth_endpoint  :: String
  , token_endpoint :: String
  , redirect_uri   :: String
  , scope          :: String
  , client_id      :: String
  , client_secret  :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Configuration = Configuration
  { fdm_config     :: FilePath
  , fdm_accounts   :: FilePath
  , cron_indicator :: FilePath
  , password_store :: FilePath
  , oauth2_dir     :: FilePath
  , pass_cmd       :: Program
  , decrypt_cmd    :: Program
  , encrypt_cmd    :: Program
  , google         :: Google
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data SystemState = SystemState
  { crontab      :: String
  , cron_enabled :: Bool
  , online       :: Bool
  , connection   :: String
  }
  deriving (Show, Generic, ToJSON)

data Environment = Environment
  { config       :: Configuration
  , system_state :: SystemState
  , options      :: Opts
  }
  deriving Show

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
  cfg  <- eitherDecodeFileStrict' $ optConfig opts :: IO (Either String Configuration)
  case cfg of
    Left err -> error err
    Right cfg' ->
      (Environment cfg' <$> (SystemState <$> getCrontab <*> return False <*> isOnline <*> getConnections))
        <*> execParser optsParser

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

isOnline :: IO Bool
isOnline = do
  (x, _, _) <- P.readProcessWithExitCode "nm-online" ["-t", "3", "-q"] ""
  if x == ExitSuccess then return True else return False

getConnections :: IO String
getConnections = do
  (_, o, _) <- P.readProcessWithExitCode
                "nmcli"
                ["--fields", "name,type,device", "connection", "show", "--active"]
                ""
  return o

