{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Keyring (
  getARfromKeyring,
  putARintoKeyring,
)
where

import Data.Aeson (eitherDecodeStrict, encode)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TSE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import GI.Gio qualified as Gio
import GI.Secret.Functions qualified as GIS
import OAMa.Environment
import System.Exit (exitFailure)

getARfromKeyring :: EmailAddress -> IO AuthRecord
getARfromKeyring email_ = do
  GIS.passwordLookupSync
    Nothing
    (Map.fromList [("oama", Text.pack email_.unEmailAddress)])
    (Nothing @Gio.Cancellable)
    >>= \case
      Just o -> case eitherDecodeStrict (TSE.encodeUtf8 o) :: Either String AuthRecord of
        Left err -> error $ "readAuthRecord:\n" ++ err
        Right rec -> return rec
      Nothing -> do
        putStrLn $ "Can't find authorization record for " ++ unEmailAddress email_
        exitFailure

putARintoKeyring :: EmailAddress -> AuthRecord -> IO ()
putARintoKeyring email_ rec = do
  let jsrec = TL.toStrict $ TLE.decodeUtf8 $ encode rec
      m = Text.pack email_.unEmailAddress
  GIS.passwordStoreSync
    Nothing
    (Map.fromList [("oama", m)])
    Nothing
    (Text.append "oama - " m)
    jsrec
    (Nothing @Gio.Cancellable)
