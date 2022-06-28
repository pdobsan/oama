module Main where

import MailCtl.Authorization
import MailCtl.CommandLine
import MailCtl.Environment
import MailCtl.Utilities
import System.Posix.Internals (c_umask)
import System.Posix.Syslog (withSyslog, Facility(Mail))

main :: IO ()
main = do
  _ <- c_umask 0o77
  withSyslog "mailctl" [] Mail $ do
    env <- loadEnvironment
    case optCommand $ options env of
      Getpwd emailAddress             -> getEmailPwd env (EmailAddress emailAddress)
      Oauth2 emailAddress             -> getEmailAuth env (EmailAddress emailAddress)
      Renew emailAddress              -> forceRenew env (EmailAddress emailAddress)
      Authorize servName emailAddress -> authorizeEmail env servName (EmailAddress emailAddress)
      Fetch emailAddresses            -> fetch env (EmailAddress <$> emailAddresses)
      ListEmails                      -> listAccounts env
      Cron StatusCron ->
        case cron_indicator $ config env of
          Just _ -> statusCron env
          Nothing -> putStrLn "main: there is no 'cron_indicator' configured."
      Cron EnableCron ->
        case cron_indicator $ config env of
          Just _ -> enableCron env
          Nothing -> putStrLn "main: there is no 'cron_indicator' configured."
      Cron DisableCron ->
        case cron_indicator $ config env of
          Just _ -> disableCron env
          Nothing -> putStrLn "main: there is no 'cron_indicator' configured."
      PrintEnv                        -> pprintEnv env

