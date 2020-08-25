{-# LANGUAGE RecordWildCards, OverloadedStrings #-}


module AllyInvest.Internal (
  signRequest
, boxException
, fetchAndDecode
) where

import           AllyInvest.Credentials
import           AllyInvest.Error

import qualified Network.HTTP.Client.TLS      as HTTPS
import           Network.HTTP.Client
import           Web.Authenticate.OAuth

import           Data.Aeson

import           Control.Exception

signRequest :: Credentials -> String -> IO Request
signRequest (Credentials {..}) url
  = do
    initReq <- parseRequest url
    let req = initReq { method = "GET" }
    let oAuth = def { oauthConsumerKey = consumerKey, oauthConsumerSecret = consumerSecret }
    let cred = newCredential oAuthToken oAuthTokenSecret
    signOAuth oAuth cred req

boxException :: (e -> AllyInvestError) -> Either e a -> Either AllyInvestError a
boxException fBox (Left e) = Left $ fBox e
boxException _ (Right x) = Right x

fetchAndDecode :: FromJSON a => Credentials -> String -> IO (Either AllyInvestError a)
fetchAndDecode cred url
  = do
    req <- signRequest cred url
    mgr <- HTTPS.newTlsManager
    res <- try $ httpLbs req mgr
    return $ boxException NetworkError res
        >>= boxException DecodeFailed . eitherDecode . responseBody
