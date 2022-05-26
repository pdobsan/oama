module Main where

import MailCtl.Authorization
import MailCtl.CommandLine
import MailCtl.Environment
import MailCtl.Utilities
import System.Posix.Internals (c_umask)

main :: IO ()
main = do
  _   <- c_umask 0o77
  env <- loadEnvironment
  case optCommand $ options env of
    Getpwd emailEntry   -> getEmailPwd env emailEntry
    Oauth2 emailEntry   -> getEmailOauth2 env emailEntry
    Fetch  emailEntries -> fetch env emailEntries
    ListEmails          -> listAccounts env
    Cron StatusCron     -> statusCron env
    Cron EnableCron     -> enableCron env
    Cron DisableCron    -> disableCron env
    PrintEnv            -> pprintEnv env

