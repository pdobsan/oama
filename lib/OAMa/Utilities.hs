{-# LANGUAGE ScopedTypeVariables #-}

module OAMa.Utilities (
  logger,
  pprintEnv,
  mkCodeVerifier
) where

import Control.Monad
import Foreign.C.String
import OAMa.Environment
import System.Posix.Syslog (Priority (..), syslog)
import System.Random
import Text.Pretty.Simple

logger :: Priority -> String -> IO ()
logger pri msg = withCStringLen msg $ syslog Nothing pri

pprintEnv :: Environment -> IO ()
pprintEnv env = do
  pPrint env

-- Proof Key for Code Exchange by OAuth Public Clients
--  https://datatracker.ietf.org/doc/html/rfc7636

unreservedCharacters :: [Char]
unreservedCharacters = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['-', '.', '_', '~']

randstr :: String -> Int -> IO String
randstr base len = do
  xs <- replicateM len (randomRIO (0, length base - 1))
  return [ base !! x | x <- xs ]

mkCodeVerifier :: Int -> IO String
mkCodeVerifier = randstr unreservedCharacters
