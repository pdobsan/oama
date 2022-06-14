module Main where

import MailCtl.Authorization
import MailCtl.CommandLine
import MailCtl.Environment
import MailCtl.Utilities
import System.Posix.Internals (c_umask)
import System.Posix.Syslog (withSyslog, Facility(Mail))

main :: IO ()
main = do
  _   <- c_umask 0o77
  withSyslog "mailctl" [] Mail $ do
    env <- loadEnvironment
    case optCommand $ options env of
      Getpwd emailEntry   -> getEmailPwd env emailEntry
      Oauth2 emailEntry   -> getEmailAuth env emailEntry
      Authorize emailEntry -> do
        auth <- authorizeEmail env emailEntry
        print auth
      Fetch  emailEntries -> fetch env emailEntries
      ListEmails          -> listAccounts env
      Cron StatusCron     -> statusCron env
      Cron EnableCron     -> enableCron env
      Cron DisableCron    -> disableCron env
      PrintEnv            -> pprintEnv env

