{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OAMa.OpenID (
  OpenIDConnect,
  getOpenIDConnect,
)
where

import Control.Exception (try)
import Data.Aeson
import Data.ByteString.UTF8 qualified as BSU
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import GHC.Generics
import Network.HTTP.Conduit
import Network.HTTP.Simple
import OAMa.Environment
import Text.Printf

data OpenIDWellKnown = OpenIDWellKnown
  { tag :: String,
    url :: String
  }
  deriving (Eq, Show)

data OpenIDConnect = OpenIDConnect
  { authorization_endpoint :: String,
    device_authorization_endpoint :: String,
    token_endpoint :: String
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type WellKnownURLs = Map (String, String) String

wellKnownURLs :: WellKnownURLs
wellKnownURLs =
  Map.fromList
    [ ( ("google", "generic"),
        "https://accounts.google.com/.well-known/openid-configuration"
      ),
      ( ("microsoft", "common"),
        "https://login.microsoftonline.com/common/v2.0/.well-known/openid-configuration"
      ),
      ( ("microsoft", "organizations"),
        "https://login.microsoftonline.com/organizations/v2.0/.well-known/openid-configuration"
      ),
      ( ("microsoft", "consumers"),
        "https://login.microsoftonline.com/consumers/v2.0/.well-known/openid-configuration"
      ),
      ( ("microsoft", "tenant-id"),
        "https://login.microsoftonline.com/{tenant-id}/v2.0/.well-known/openid-configuration"
      )
    ]

getOpenIDConnect :: String -> String -> IO OpenIDConnect
getOpenIDConnect service_ tenant_ = do
  case Map.lookup (service_, tenant_) wellKnownURLs of
    Nothing ->
      fatalError "getOpenIDConnect" (printf "Cant't find such .well-known URL: (%s, %s)" service_ tenant_)
    Just link -> do
      req <- parseRequest ("GET" ++ " " ++ link)
      (try $ httpBS req :: IO (Either HttpException (Response BSU.ByteString)))
        >>= \case
          Left (HttpExceptionRequest _ x) -> fatalError "getOpenIDConnect" (printf "request failed: %s" (show x))
          Left (InvalidUrlException u _) -> fatalError "getOpenIDConnect" (printf "invalid url: %s" u)
          Right resp -> do
            let body = getResponseBody resp
                oidc :: Maybe OpenIDConnect = decodeStrict body
            case oidc of
              Just x -> return x
              Nothing -> fatalError "getOpenIDConnect" (printf "Can't decode oidc response: %s" (BSU.toString body))
