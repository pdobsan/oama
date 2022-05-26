{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MailCtl.Utilities
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import MailCtl.CommandLine
import MailCtl.Environment
import System.Directory qualified as D
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)    
import System.Process qualified as P    
import Text.Pretty.Simple

logger :: String -> String -> IO ()
logger level msg = do
  (x, _, _) <- P.readProcessWithExitCode "logger" ["-t", "mailctl", "-p", "mail." ++ level, msg] ""
  if x == ExitSuccess then return () else exitWith x

pprintEnv :: Environment -> IO ()
pprintEnv env = do
  pPrint env

enableCron :: Environment -> IO ()
enableCron env = do
  TIO.writeFile (cron_indicator $ config env) ""
  logger "warning" "Enabled running fdm by cron."
  statusCron env

disableCron :: Environment -> IO ()
disableCron env = do
  D.removePathForcibly $ cron_indicator $ config env
  logger "warning" "Disabled running fdm by cron."
  statusCron env

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

fetchAccounts :: FilePath -> [String] -> IO ()
fetchAccounts fdmConfig as = do
  let bs = concat [ ["-a", a] | a <- as ]
  (x, _, _) <- P.readProcessWithExitCode "fdm" (["-f", fdmConfig, "-l"] ++ bs ++ ["fetch"]) ""
  if x == ExitSuccess then return () else exitWith x

fetch :: Environment -> [String] -> IO ()
fetch env accounts = do
  let isOnline    = online $ system_state env
      cronEnabled = cron_enabled $ system_state env
      runByCron   = optCron $ options env
  run isOnline cronEnabled runByCron
 where
  run False _ rbc = do
    if rbc then logger "error" "ERROR - Computer is offline." else putStrLn "ERROR - Computer is offline."
    exitWith (ExitFailure 1)
  run True _  False = fetchAccounts (fdm_config $ config env) accounts
  run True ce True  = if ce then fetchAccounts (fdm_config $ config env) accounts else exitWith ExitSuccess

