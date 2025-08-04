{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Crypto.Manager (
  decryptFile,
  encryptFile,
  lookupSecret,
  storeSecret,
  secretMethod,
)
where

#ifdef LIB_GPGME
import Crypto.Gpgme
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as BSU
import Data.Maybe
import System.Environment
#endif
#ifdef LIB_GISECRET
import Data.Map qualified as Map
import GI.Gio qualified as Gio
import GI.Secret.Functions qualified as GIS
import Data.Text qualified as Text
#else
import System.Info qualified as SI
#endif
import System.Directory qualified as D
import Text.Printf (printf)
#if !defined(LIB_GPGME) || !defined(LIB_GISECRET)
import System.Exit (ExitCode (ExitSuccess))
import System.IO qualified as IO
import System.Process qualified as P
#endif

type KeyID = String

type Secret = String

data SecretError
  = FileError String
  | DecryptError String
  | EncryptError String
  | LookupError String
  | StoreError String
  deriving (Show)

secretMethod :: String
secretMethod = gpgMethod <> " " <> ringMethod

#ifndef LIB_GPGME

gpgMethod :: String
gpgMethod = "gpg"

decryptFile :: FilePath -> IO (Either SecretError Secret)
decryptFile f = do
  -- check gpg environment
  fOk <- D.doesFileExist f
  if fOk
    then do
      (x, o, e) <- P.readProcessWithExitCode "gpg" ["--decrypt", f] ""
      if x == ExitSuccess
        then return $ Right o
        else
          pure $ Left $ DecryptError e
    else pure $ Left $ FileError $ printf "Can't open file: %s" f

encryptFile :: FilePath -> Secret -> KeyID -> IO (Either SecretError String)
encryptFile f s k = do
  (Just h, _, _, p) <-
    P.createProcess
      (P.proc "gpg" ["--encrypt", "--recipient", k, "-o", f ++ ".tmp"])
        { P.std_in = P.CreatePipe
        }
  IO.hPutStr h s
  IO.hFlush h
  IO.hClose h
  x <- P.waitForProcess p
  if x == ExitSuccess
    then do
      D.renameFile (f ++ ".tmp") f
      pure $ Right "gpg encryption succeded."
    else pure $ Left $ EncryptError "gpg encryption failed."

#endif

#ifdef LIB_GPGME

gpgMethod :: String
gpgMethod = "lib-gpgme"

decryptFile :: FilePath -> IO (Either SecretError Secret)
decryptFile f = do
  fOk <- D.doesFileExist f
  if fOk
    then do
      gpgHome <- getEnv "HOME" >>= \x -> pure (x ++ "/.gnupg")
      enc <- BS.readFile f
      withCtx gpgHome "C" OpenPGP $ \ctx ->
        decrypt ctx enc
          >>= \case
            Left err -> return $ Left $ DecryptError (show err)
            Right o -> return $ Right (BSU.toString o)
    else pure $ Left $ FileError $ printf "Can't open file: %s" f

encryptFile :: FilePath -> Secret -> KeyID -> IO (Either SecretError String)
encryptFile f s k = do
  gpgHome <- getEnv "HOME" >>= \x -> pure (x ++ "/.gnupg")
  withCtx gpgHome "C" OpenPGP $ \ctx -> do
    key <- getKey ctx (BSU.fromString k) NoSecret
    encrypt ctx [fromJust key] NoFlag (BSU.fromString s)
      >>= \case
        Right enc -> do
          BS.writeFile f enc
          pure $ Right "gpg encryption succeded."
        Left err -> return $ Left $ EncryptError (show err)

#endif

type Attribute = String

type Value = String

type Label = String

#ifndef LIB_GISECRET

ringMethod :: String
ringMethod = "secret-tool/security"

lookupSecret :: Attribute -> Value -> IO (Either SecretError String)
lookupSecret attribute value = do
  case SI.os of
    "linux" ->
      P.readProcessWithExitCode
        "secret-tool"
        ["lookup", attribute, value]
        ""
        >>= result
    "darwin" ->
      P.readProcessWithExitCode
        "security"
        ["find-generic-password", "-s", attribute, "-a", value, "-w"]
        ""
        >>= result
    os -> pure $ Left $ LookupError $ printf "Can't work in %s operating system." os
  where
    result (x, o, e) =
      if x == ExitSuccess
        then pure $ Right o
        else pure $ Left $ LookupError e

storeSecret :: Label -> Attribute -> Value -> Secret -> IO (Either SecretError String)
storeSecret label attribute value secret = do
  case SI.os of
    "linux" -> do
      P.createProcess
        (P.proc "secret-tool" ["store", "--label", label, attribute, value])
          { P.std_in = P.CreatePipe
          }
        >>= \(h, _, _, p) -> write2Pipe h p secret
    "darwin" -> do
      P.readProcessWithExitCode
        "security"
        ["add-generic-password",
         "-l", label,
         "-a", value,
         "-s", attribute,
         "-T", "/usr/bin/security",
         "-U",
         "-w", secret
        ]
        ""
        >>= result
    os -> pure $ Left $ StoreError $ printf "Can't work in %s operating system." os
  where
    write2Pipe (Just h) p s = do
      IO.hPutStrLn h s
      IO.hFlush h
      IO.hClose h
      x <- P.waitForProcess p
      if x == ExitSuccess
        then pure $ Right "secret stored."
        else pure $ Left $ StoreError "Storing secret failed."
    write2Pipe Nothing _ _ = do
      pure $ Left $ StoreError $ printf "Failed to get handle to pipe."
    result (x, o, e) =
      if x == ExitSuccess
        then pure $ Right o
        else pure $ Left $ StoreError e

#endif

#ifdef LIB_GISECRET

ringMethod :: String
ringMethod = "lib-gi-secret"

lookupSecret :: Attribute -> Value -> IO (Either SecretError String)
lookupSecret attribute value = do
  GIS.passwordLookupSync
    Nothing
    (Map.fromList [(Text.pack attribute, Text.pack value)])
    (Nothing @Gio.Cancellable)
    >>= \case
      Just o -> return $ Right (Text.unpack o)
      Nothing ->
        return $
          Left $
            LookupError $
              printf "Can't find secret associated with %s %s" attribute value

storeSecret :: Label -> Attribute -> Value -> Secret -> IO (Either SecretError String)
storeSecret label attribute value secret = do
  -- TODO check retun status
  _ <-
    GIS.passwordStoreSync
      Nothing
      (Map.fromList [(Text.pack attribute, Text.pack value)])
      Nothing
      (Text.pack label)
      (Text.pack secret)
      (Nothing @Gio.Cancellable)
  return $ Right "storing secret succeded."

#endif
