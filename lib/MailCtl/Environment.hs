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
import GHC.Generics
import MailCtl.CommandLine
import Net.IPv4 (ipv4)
import Network.DNS (makeResolvSeed, defaultResolvConf, withResolver, lookupA)
import Network.Icmp.Ping (host)
import Options.Applicative (execParser)
import System.Directory qualified as D
import System.Exit (ExitCode (ExitSuccess), exitWith, exitFailure)    
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
  , internet_OK  :: Bool
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
  configExists <- D.doesFileExist $ optConfig opts
  if configExists
    then do
      cfg  <- eitherDecodeFileStrict' $ optConfig opts :: IO (Either String Configuration)
      case cfg of
        Left err -> error err
        Right cfg' ->
          (Environment cfg' <$> (SystemState <$> getCrontab <*> return False <*> andM isOnline isDNSWorking))
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

isOnline :: IO Bool
isOnline = do
  pong <- host 3000000 $ ipv4 8 8 8 8
  case pong of
    Left  _ -> return False
    Right _ -> return True

isDNSWorking :: IO Bool
isDNSWorking = do
  rs   <- makeResolvSeed defaultResolvConf
  recA <- withResolver rs $ \resolver -> lookupA resolver "accounts.google.com"
  case recA of
    Left  _ -> return False
    Right _ -> return True

andM :: Monad m => m Bool -> m Bool -> m Bool
andM mx my = do
  x <- mx
  if x then my else return x
