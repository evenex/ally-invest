{-# LANGUAGE OverloadedStrings #-}

module AllyInvest.Credentials (
  Credentials(..)
) where

import           Data.Aeson
import qualified Data.ByteString.Char8        as S8

data Credentials
  = Credentials {
      consumerKey :: S8.ByteString
    , consumerSecret :: S8.ByteString
    , oAuthToken :: S8.ByteString
    , oAuthTokenSecret :: S8.ByteString
    }
instance FromJSON Credentials
  where
  parseJSON (Object o)
    = Credentials
      <$> (S8.pack <$> o .: "consumer_key")
      <*> (S8.pack <$> o .: "consumer_secret")
      <*> (S8.pack <$> o .: "oauth_token")
      <*> (S8.pack <$> o .: "oauth_token_secret")
