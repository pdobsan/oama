{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GnuPG (
  getARwithGPG,
  putARwithGPG,
)
where

import Data.Aeson (eitherDecode', encode)
import Data.ByteString.Lazy.UTF8 qualified as BLU
import OAMa.Environment
import System.Directory qualified as D
import System.Exit (ExitCode (ExitSuccess), exitFailure, exitWith)
import System.IO qualified as IO
import System.Posix.Syslog (Priority (..))
import System.Process qualified as P
import Text.Printf (printf)

getARwithGPG :: Environment -> EmailAddress -> IO AuthRecord
getARwithGPG env email_ = do
  let gpgFile = env.state_dir <> "/" <> email_.unEmailAddress <> ".oama"
  authRecExist <- D.doesFileExist gpgFile
  if authRecExist
    then do
      (x, o, e) <- P.readProcessWithExitCode "gpg" ["--decrypt", gpgFile] ""
      if x == ExitSuccess
        then case eitherDecode' (BLU.fromString o) :: Either String AuthRecord of
          Left err -> error $ "readAuthRecord:\n" ++ err
          Right rec -> return rec
        else do
          putStr e
          exitWith x
    else do
      printf "Can't find authorization record for %s\n" (unEmailAddress email_)
      printf "You must run `oama authorize ...` before using other operations.\n"
      logger Error $ printf "Can't find authorization record for %s\n" (unEmailAddress email_)
      exitFailure

putARwithGPG :: Environment -> String -> EmailAddress -> AuthRecord -> IO ()
putARwithGPG env keyID email_ rec = do
  let gpgFile = env.state_dir <> "/" <> email_.unEmailAddress <> ".oama"
      jsrec = BLU.toString $ encode rec
  (Just h, _, _, p) <-
    P.createProcess
      (P.proc "gpg" ["--encrypt", "--recipient", keyID, "-o", gpgFile <> ".new"])
        { P.std_in = P.CreatePipe
        }
  IO.hPutStr h jsrec
  IO.hFlush h
  IO.hClose h
  x <- P.waitForProcess p
  if x == ExitSuccess
    then D.renameFile (gpgFile ++ ".new") gpgFile
    else exitWith x
