{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GnuPG (
  getARwithGPG,
  putARwithGPG,
)
where

import Crypto.Gpgme
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.UTF8 qualified as BSU
import Data.Maybe
import OAMa.Environment
import System.Directory qualified as D
import System.Environment
import System.Exit (exitFailure)
import System.Posix.Syslog (Priority (..))
import Text.Printf (printf)

getARwithGPG :: Environment -> EmailAddress -> IO AuthRecord
getARwithGPG env email_ = do
  let gpgFile = env.state_dir <> "/" <> email_.unEmailAddress <> ".oama"
  gpgHome <- getEnv "HOME" >>= \x -> pure (x ++ "/.gnupg")
  authRecExist <- D.doesFileExist gpgFile
  if authRecExist
    then do
      enc <- BS.readFile gpgFile
      withCtx gpgHome "C" OpenPGP $ \ctx ->
        decrypt ctx enc
          >>= \case
            Left err -> error $ "decrypt AuthRecord error:\n" ++ show err
            Right o -> case eitherDecodeStrict o :: Either String AuthRecord of
              Left err -> error $ "readAuthRecord:\n" ++ err
              Right rec -> return rec
    else do
      printf "Can't find authorization record for %s\n" (unEmailAddress email_)
      printf "You must run `oama authorize ...` before using other operations.\n"
      logger Error $ printf "Can't find authorization record for %s\n" (unEmailAddress email_)
      exitFailure

putARwithGPG :: Environment -> String -> EmailAddress -> AuthRecord -> IO ()
putARwithGPG env keyID email_ rec = do
  let gpgFile = env.state_dir <> "/" <> email_.unEmailAddress <> ".oama"
      jsrec = BL.toStrict $ encode rec
  gpgHome <- getEnv "HOME" >>= \x -> pure (x ++ "/.gnupg")
  withCtx gpgHome "C" OpenPGP $ \ctx -> do
    key <- getKey ctx (BSU.fromString keyID) NoSecret
    encrypt ctx [fromJust key] NoFlag jsrec
      >>= \case
        Right enc -> BS.writeFile gpgFile enc
        Left err -> error $ "encrypt AuthRecord error:\n" ++ show err
