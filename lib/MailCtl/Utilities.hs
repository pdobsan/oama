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

disableCron :: Environment -> IO ()
disableCron env = do
  D.removePathForcibly $ cron_indicator $ config env
  logger "warning" "Disabled running fdm by cron."

statusCron :: Environment -> IO ()
statusCron env = do
  flag <- D.doesFileExist $ cron_indicator $ config env
  if flag
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

networkStatus :: Environment -> IO ()
networkStatus env = do
  if online $ system_state env
    then do
      putStrLn "Network connection(s):"
      putStrLn $ connection $ system_state env
    else do
      putStrLn "ERROR - Computer is offline."
      exitWith (ExitFailure 1)

fetchAccounts :: Environment -> [String] -> IO ()    
fetchAccounts env as = do    
  if online $ system_state env    
    then do    
      let bs = concat [ ["-a", a] | a <- as ]    
      (x, _, _) <- P.readProcessWithExitCode    
        "fdm"    
        (["-f", fdm_config $ config env, "-l"] ++ bs ++ ["fetch"])    
        ""    
      if x == ExitSuccess then return () else exitWith x    
    else do    
      if (optQuiet $ options env)
        then logger "error" "ERROR - Computer is offline."    
        else putStrLn "ERROR - Computer is offline."    
      exitWith (ExitFailure 1)    

