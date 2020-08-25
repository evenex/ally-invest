{-# LANGUAGE OverloadedStrings #-}

module AllyInvest.TimeSales (
  getTimeSales 
, Candle(..)
) where

import           AllyInvest.Credentials
import           AllyInvest.Error
import           AllyInvest.Internal

import           Data.Aeson
import           Data.Time
import           Data.List

getTimeSales :: Credentials -> String -> Day -> IO (Either AllyInvestError [Candle])
getTimeSales cred sym day
  = fmap unCandleResult <$> fetchAndDecode cred url
  where
  url = intercalate "?" [
          "https://api.tradeking.com/v1/market/timesales.json"
        , intercalate "&"
          . fmap (\(x,y) -> intercalate "=" [x, y])
          $ [ ("symbols", sym)
            , ("startdate", show day)
            , ("enddate", show $ addDays 1 day)
            ]
        ]

data Candle
  = Candle {
    tDate :: Day
  , tDateTime :: UTCTime
  , tLowPrice :: Double
  , tHighPrice :: Double
  , tOpenPrice :: Double
  , tLastPrice :: Double
  , tTradeVolume :: Int
  , tCumulativeTradeVolume :: Int
  } deriving Show
instance FromJSON Candle
  where
  parseJSON (Object o)
    = Candle
      <$> (o .: "date")
      <*> (o .: "datetime")
      <*> (read <$> o .: "lo")
      <*> (read <$> o .: "hi")
      <*> (read <$> o .: "opn")
      <*> (read <$> o .: "last")
      <*> (read <$> o .: "incr_vl")
      <*> (read <$> o .: "vl")

--------------------------------------------------------------------------------
-- private
--------------------------------------------------------------------------------
data CandleResult
  = CandleResult {
    unCandleResult :: [Candle]
  }
instance FromJSON CandleResult
  where
  parseJSON (Object o)
    = CandleResult <$> (o .: "response" >>= (.: "quotes") >>= (.: "quote"))
