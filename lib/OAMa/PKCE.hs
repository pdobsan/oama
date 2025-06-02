-- | Module for generating PKCE (Proof Key for Code Exchange) parameters
-- Ref.: https://datatracker.ietf.org/doc/html/rfc7636
module OAMa.PKCE (
  PKCE (..),
  generateVerifier,
  makeChallenge,
  generatePKCE,
) where

import Control.Monad (replicateM)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Base64.URL (encodeUnpadded)
import Data.ByteString.UTF8 (fromString, toString)
import System.Random (randomRIO)

data PKCE = PKCE
  { code_verifier :: String,
    code_challenge :: String,
    code_challenge_method :: String
  }
  deriving (Show)

verifierMaxLength :: Int
verifierMaxLength = 128

verifierMinLength :: Int
verifierMinLength = 43

charsPKCE :: String
charsPKCE = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['-', '_', '.', '~']

-- | generate a random PKCE verifier string of the given length
generateVerifier :: Int -> IO String
generateVerifier len = replicateM len $ randomRIO (0, length charsPKCE - 1) >>= \i -> pure (charsPKCE !! i)

-- | make PKCE challenge string to the given verifier string
makeChallenge :: String -> String
makeChallenge verifier = toString (encodeUnpadded (hash (fromString verifier)))

-- | generate a PKCE record with a random verifier of random length
generatePKCE :: IO PKCE
generatePKCE = do
  -- rlen <- randomRIO (verifierMinLength, verifierMaxLength)
  rlen <- randomRIO (ceiling $ fromIntegral verifierMaxLength * (0.7 :: Double), verifierMaxLength)
  verifier <- generateVerifier (max rlen verifierMinLength)
  return $ PKCE verifier (makeChallenge verifier) "S256"
