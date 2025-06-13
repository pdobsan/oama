{-# LANGUAGE OverloadedStrings #-}

module OAMa.Logging (
  logger,
  fatalError,
)
where

import Foreign.C.String
import System.Exit (exitFailure)
import System.Posix.Syslog (Priority (..), syslog)
import Text.Printf

logger :: Priority -> String -> IO ()
logger pri msg = withCStringLen msg $ syslog Nothing pri

fatalError :: String -> String -> IO a
fatalError caller errmsg = do
  printf "%s: %s\n" caller errmsg
  logger Error $ printf "%s: %s" caller errmsg
  exitFailure
