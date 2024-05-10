module Main where

import OAMa.Authorization
import OAMa.CommandLine
import OAMa.Environment
import System.Posix.Internals (c_umask)
import System.Posix.Syslog (Facility (Mail), withSyslog)

main :: IO ()
main = do
  _ <- c_umask 0o77
  withSyslog "oama" [] Mail $ do
    env <- loadEnvironment
    case optCommand $ options env of
      Oauth2 emailAddress -> getEmailAuth env (EmailAddress emailAddress)
      Renew emailAddress -> forceRenew env (EmailAddress emailAddress)
      Authorize servName emailAddress nohint -> authorizeEmail env servName (EmailAddress emailAddress) nohint
      PrintEnv -> pprintEnv env
