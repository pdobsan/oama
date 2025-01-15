{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Keyring (
  getARfromKeyring,
  putARintoKeyring,
) where

import Data.Aeson (eitherDecode', encode)
import Data.ByteString.Lazy.UTF8 qualified as BLU
import OAMa.Environment
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.IO qualified as IO
import System.Process qualified as P

-- import Text.Printf (printf)

getARfromKeyring :: EmailAddress -> IO AuthRecord
getARfromKeyring email_ = do
  (x, o, e) <- P.readProcessWithExitCode "security" ["find-generic-password", "-s", "oama", "-a", email_.unEmailAddress, "-w"] ""
  if x == ExitSuccess
    then case eitherDecode' (BLU.fromString o) :: Either String AuthRecord of
      Left err -> error $ "readAuthRecord:\n" ++ err
      Right rec -> return rec
    else do
      putStrLn $ "Can't find authorization record for " ++ unEmailAddress email_
      putStr e
      exitWith x

putARintoKeyring :: EmailAddress -> AuthRecord -> IO ()
putARintoKeyring email_ rec = do
  let jsrec = BLU.toString $ encode rec
      m = email_.unEmailAddress
      securityCommand = " add-generic-password " ++ " -a " ++ m ++ " -s " ++ " oama " ++ " -T " ++ " /usr/bin/security " ++ " -U " ++ " -w " ++ jsrec
  (Just h, _, _, p) <-
    P.createProcess
      (P.proc "security" ["-i"])
        { P.std_in = P.CreatePipe
        }
  IO.hPutStr h securityCommand
  IO.hFlush h
  IO.hClose h
  x <- P.waitForProcess p
  if x == ExitSuccess
    then return ()
    else exitWith x
