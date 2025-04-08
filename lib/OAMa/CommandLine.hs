{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module OAMa.CommandLine (
  Opts (..),
  Command (..),
  optsParser,
) where

import Data.Version (showVersion)
import Data.Yaml qualified as Yaml
import GHC.Generics
import Options.Applicative
import Paths_oama (version)

data Opts = Opts
  { optConfig :: !String
  , optDebug :: !Bool
  , optCommand :: !Command
  }
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

data Command
  = Oauth2 String
  | ShowCreds String
  | Renew String
  | Authorize String String Bool Bool
  | PrintEnv
  | PrintTemplate
  deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

optsParser :: ParserInfo Opts
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    ( fullDesc
        <> progDesc
          ( "Oama is an OAuth credential manager providing store/lookup, automatic renewal and authorization operations. "
              ++ "The credentials are stored either in the Gnome keyring or in files encrypted by GnuPG. "
              ++ "Oama is useful for IMAP/SMTP or other network clients which cannot authorize and renew OAuth tokens on their own. "
          )
        <> header "oama - OAuth credential MAnager with store/lookup, renewal, authorization."
    )

versionOption :: Parser (a -> a)
versionOption = do
  let verinfo =
        "oama version "
          <> showVersion version
          <> "\nCopyright (c) 2022-2025, Peter Dobsan"
  infoOption verinfo (long "version" <> help "Show version")

programOptions :: Parser Opts
programOptions =
  Opts
    <$> strOption
      ( long "config"
          <> short 'c'
          <> metavar "<config>"
          <> value "~/.config/oama/config.yaml"
          <> showDefault
          <> help "Configuration file"
      )
    <*> switch (long "debug" <> help "Print HTTP traffic to stdout")
    <*> hsubparser (oauth2 <> showcreds <> renew <> authorize <> printEnv <> printTemplate)

oauth2 :: Mod CommandFields Command
oauth2 = command "access" (info oauth2Options (progDesc "Get the access token for email"))
oauth2Options :: Parser Command
oauth2Options = Oauth2 <$> strArgument (metavar "<email>" <> help "Email address")

showcreds :: Mod CommandFields Command
showcreds = command "show" (info showcredsOptions (progDesc "Show current credentials for email"))
showcredsOptions :: Parser Command
showcredsOptions = ShowCreds <$> strArgument (metavar "<email>" <> help "Email address")

renew :: Mod CommandFields Command
renew = command "renew" (info renewOptions (progDesc "Renew the access token of email"))
renewOptions :: Parser Command
renewOptions = Renew <$> strArgument (metavar "<email>" <> help "Email address")

authorize :: Mod CommandFields Command
authorize = command "authorize" (info authorizeOptions (progDesc "Authorize OAuth2 for service/email"))
authorizeOptions :: Parser Command
authorizeOptions =
  Authorize
    <$> strArgument (metavar "<service>" <> help "Service name")
    <*> strArgument (metavar "<email>" <> help "Email address")
    <*> switch (long "nohint" <> help "Don't pass login hint")
    <*> switch (long "device" <> help "Use OAuth device code flow (RFC 8628)")

printEnv :: Mod CommandFields Command
printEnv = command "printenv" (info (pure PrintEnv) (progDesc "Print the current runtime environment"))

printTemplate :: Mod CommandFields Command
printTemplate = command "template" (info (pure PrintTemplate) (progDesc "Print the default config template"))
