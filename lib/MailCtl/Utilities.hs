{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MailCtl.Utilities
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Foreign.C.String
import MailCtl.CommandLine
import MailCtl.Environment
import System.Directory qualified as D
import System.Exit (ExitCode (ExitSuccess), exitWith, exitSuccess, exitFailure)    
import System.Posix.Syslog (syslog, Priority(..))
import System.Process qualified as P    
import Text.Pretty.Simple

logger :: Priority -> String -> IO ()
logger pri msg = withCStringLen msg $ syslog Nothing pri

pprintEnv :: Environment -> IO ()
pprintEnv env = do
  pPrint env

enableCron :: Environment -> IO ()
enableCron env =
  case cron_indicator $ config env of
    Just cron_indicator' -> do
      TIO.writeFile cron_indicator' ""
      logger Warning "Enabled running fdm by cron."
      env' <- loadEnvironment
      statusCron env'
    Nothing -> do
      putStrLn "enableCron: there is no 'cron_indicator' configured."
      exitFailure

disableCron :: Environment -> IO ()
disableCron env =
  case cron_indicator $ config env of
    Just cron_indicator' -> do
      D.removePathForcibly cron_indicator'
      logger Warning "Disabled running fdm by cron."
      env' <- loadEnvironment
      statusCron env'
    Nothing -> do
      putStrLn "disableCron: there is no 'cron_indicator' configured."
      exitFailure

statusCron :: Environment -> IO ()
statusCron env = do
  if cron_enabled $ system_state env
    then do
      putStrLn "Running from cron is enabled:"
      putStrLn $ crontab $ system_state env
    else putStrLn "Running from cron is disabled."

listAccounts :: Environment -> IO ()
listAccounts env = do
  xs <- TIO.readFile (fdm_accounts $ config env)
  let ys = T.lines xs
      zs = [ extract y | y <- ys, T.isPrefixOf "account" y ]
      z  = T.intercalate "\n" zs
      extract ls =
        let ws          = T.splitOn "\"" ls
            (_ : a : _) = ws
        in  a
  TIO.putStrLn "List of accounts fetched:"
  TIO.putStrLn z

fetchAccounts :: FilePath -> [EmailAddress] -> IO ()
fetchAccounts fdmConfig as = do
  let bs = concat [ ["-a", unEmailAddress a] | a <- as ]
  (x, _, _) <- P.readProcessWithExitCode "fdm" (["-f", fdmConfig, "-l"] ++ bs ++ ["fetch"]) ""
  if x == ExitSuccess then return () else exitWith x

fetch :: Environment -> [EmailAddress] -> IO ()
fetch env accounts = do
  let cronEnabled = cron_enabled $ system_state env
      runByCron   = optCron $ options env
  run cronEnabled runByCron
 where
  run _  False = fetchAccounts (fdm_config $ config env) accounts
  run ce True  = if ce then fetchAccounts (fdm_config $ config env) accounts else exitSuccess

