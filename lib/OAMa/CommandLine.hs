module OAMa.CommandLine (
  Opts (..),
  Command (..),
  optsParser,
) where

import Data.Version (showVersion)
import Options.Applicative
import Paths_oama (version)

data Opts = Opts
  { optConfig :: !String,
    optDebug :: !Bool,
    optCommand :: !Command
  }
  deriving (Eq, Show)

data Command
  = Oauth2 String
  | Renew String
  | Authorize String String Bool
  | PrintEnv
  deriving (Eq, Show)

optsParser :: ParserInfo Opts
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    ( fullDesc
        <> progDesc ("OAMa is an OAuth credential manager providing store/lookup, automatic renewal and authorization operations. " ++
                     "The credentials are stored either in the Gnome keyring or in files encrypted by GnuPG. " ++
                     "OAMa is useful for IMAP/SMTP or other network clients which cannot authorize and renew OAuth tokens on their own. "
                    )
        <> header "OAMa - OAuth credential manager with store/lookup, renewal, authorization."
    )

versionOption :: Parser (a -> a)
versionOption = do
  let verinfo =
        "oama version "
          <> showVersion version
          <> "\nCopyright (C) Peter Dobsan 2023"
  infoOption verinfo (long "version" <> help "Show version")

programOptions :: Parser Opts
programOptions =
  Opts
    <$> strOption
      ( long "config-file"
          <> short 'c'
          <> metavar "<config>"
          <> value ""
          <> help "Configuration file"
      )
    <*> switch (long "debug" <> help "Print HTTP traffic to stdout")
    <*> hsubparser (oauth2 <> renew <> authorize <> printEnv)

oauth2 :: Mod CommandFields Command
oauth2 = command "access" (info oauth2Options (progDesc "Get the access token for email"))
oauth2Options :: Parser Command
oauth2Options = Oauth2 <$> strArgument (metavar "<email>" <> help "Email address")

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

printEnv :: Mod CommandFields Command
printEnv = command "printenv" (info (pure PrintEnv) (progDesc "Print the current Environment"))
