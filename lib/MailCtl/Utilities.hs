{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MailCtl.Utilities (
  logger,
  pprintEnv,
  enableCron,
  disableCron,
  statusCron,
  listAccounts,
  fetch,
  getEmailPwd,
  mkCodeVerifier
) where

import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Foreign.C.String
import MailCtl.CommandLine
import MailCtl.Environment
import System.Directory qualified as D
import System.Environment qualified as E
import System.Exit (ExitCode (ExitSuccess), exitFailure, exitSuccess, exitWith)
import System.Posix.Syslog (Priority (..), syslog)
import System.Process qualified as P
import System.Random
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
      putStrLn $ fromMaybe "WARNING: there is no crontab." $ crontab $ system_state env
    else putStrLn "Running from cron is disabled."

listAccounts :: String -> IO ()
listAccounts fdm_accounts_path = do
  xs <- TIO.readFile fdm_accounts_path
  let ys = T.lines xs
      zs = [extract y | y <- ys, T.isPrefixOf "account" y]
      z = T.intercalate "\n" zs
      extract ls =
        let ws = T.splitOn "\"" ls
            (_ : a : _) = ws
         in a
  TIO.putStrLn "List of accounts fetched:"
  TIO.putStrLn z

fetchAccounts :: FilePath -> [EmailAddress] -> IO ()
fetchAccounts fdmConfig as = do
  let bs = concat [["-a", unEmailAddress a] | a <- as]
  (x, _, _) <- P.readProcessWithExitCode "fdm" (["-f", fdmConfig, "-l"] ++ bs ++ ["fetch"]) ""
  if x == ExitSuccess then return () else exitWith x

fetch :: Environment -> String -> [EmailAddress] -> IO ()
fetch env fdm_config_ accounts = do
  let cronEnabled = cron_enabled $ system_state env
      runByCron = optCron $ options env
  run cronEnabled runByCron
  where
    run _ False = fetchAccounts fdm_config_ accounts
    run ce True = if ce then fetchAccounts fdm_config_ accounts else exitSuccess

-- Utilities for traditional password based email services
-- using [pass](https://www.passwordstore.org/)

getEmailPwd :: Environment -> EmailAddress -> IO ()
getEmailPwd env email_ = do
  password <- getEmailPwd' env email_
  putStrLn password

getEmailPwd' :: Environment -> EmailAddress -> IO String
getEmailPwd' env email_ = do
  E.lookupEnv "PASSWORD_STORE_DIR"
    >>= \case
      Nothing -> do
        case password_store $ config env of
          Just password_store' -> do
            E.setEnv "PASSWORD_STORE_DIR" password_store'
            getEPwd
          Nothing -> do
            putStrLn "getEmailPwd': there is no 'password_store' configured nor PASSWORD_STORE_DIR environment variable set."
            exitFailure
      _ -> getEPwd
  where
    getEPwd = do
      case pass_cmd $ config env of
        Just pass_cmd' -> do
          (x, o, e) <-
            P.readProcessWithExitCode
              (exec pass_cmd')
              [head (args pass_cmd') ++ unEmailAddress email_]
              []
          if x == ExitSuccess
            then return $ head (lines o)
            else do
              putStr e
              exitWith x
        Nothing -> do
          putStrLn "getEmailPwd': there is no 'pass_cmd' configured."
          exitFailure

-- Proof Key for Code Exchange by OAuth Public Clients
--  https://datatracker.ietf.org/doc/html/rfc7636

unreservedCharacters :: [Char]
unreservedCharacters = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['-', '.', '_', '~']

randstr :: String -> Int -> IO String
randstr base len = do
  xs <- replicateM len (randomRIO (0, length base - 1))
  return [ base !! x | x <- xs ]

mkCodeVerifier :: Int -> IO String
mkCodeVerifier len = randstr unreservedCharacters len
