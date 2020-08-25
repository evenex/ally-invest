{-# LANGUAGE OverloadedStrings #-}

module AllyInvest.Quote (
  getQuotes 
, TradeConditionCode(..)
, TradingSession(..)
, DividendFrequency(..)
, OptionDelivery(..)
, OptionStyle(..)
, OptionClass(..)
, OptionType(..)
, Quote(..)
) where

import           AllyInvest.Credentials
import           AllyInvest.Error
import           AllyInvest.Internal

import           Safe

import           Data.Aeson
import qualified Data.Text                    as T
import           Data.List

getQuotes :: Credentials -> [String] -> IO (Either AllyInvestError [Quote])
getQuotes cred syms 
  = if length syms > 1
    then fmap unQuotesResult <$> fetchAndDecode cred url
    else fmap (return . unQuoteResult) <$> fetchAndDecode cred url
  where
  url = "https://api.tradeking.com/v1/market/ext/quotes.json?symbols=" ++ intercalate "," syms
    
data TradeConditionCode = TradeHalted | TradeResumed | TradeConditionUnknown String
  deriving Show
instance FromJSON TradeConditionCode where
  parseJSON (String "H") = return TradeHalted
  parseJSON (String "R") = return TradeResumed
  parseJSON (String x) = return . TradeConditionUnknown . T.unpack $ x
  parseJSON x = fail (show x)

data TradingSession = PremarketSession | RegularSession | PostmarketSession | MarketClosed
  deriving Show
instance FromJSON TradingSession where
  parseJSON (String "pre") = return PremarketSession
  parseJSON (String "regular") = return RegularSession
  parseJSON (String "post") = return PostmarketSession
  parseJSON (String "na") = return MarketClosed
  parseJSON x = fail (show x)

data DividendFrequency = AnnualDividend | SemiAnnualDividend | QuarterlyDividend | MonthlyDividend | NoDividend
  deriving Show
instance FromJSON DividendFrequency where
  parseJSON (String "A") = return AnnualDividend 
  parseJSON (String "S") = return SemiAnnualDividend 
  parseJSON (String "Q") = return QuarterlyDividend 
  parseJSON (String "M") = return MonthlyDividend 
  parseJSON (String "N") = return NoDividend
  parseJSON x = fail (show x)

data OptionDelivery = StandardSettlement | NonstandardSettlement | NoSettlement
  deriving Show
instance FromJSON OptionDelivery where
  parseJSON (String "S") = return StandardSettlement 
  parseJSON (String "N") = return NonstandardSettlement 
  parseJSON (String "X") = return NoSettlement
  parseJSON x = fail (show x)

data OptionStyle = AmericanOption | EuropeanOption
  deriving Show
instance FromJSON OptionStyle where
  parseJSON (String "A") = return AmericanOption 
  parseJSON (String "E") = return EuropeanOption
  parseJSON x = fail (show x)

data OptionClass = StandardOption | LeapOption | ShortTermOption
  deriving Show
instance FromJSON OptionClass where
  parseJSON (String "0") = return StandardOption 
  parseJSON (String "1") = return LeapOption 
  parseJSON (String "3") = return ShortTermOption
  parseJSON x = fail (show x)

data OptionType = PutOption | CallOption
  deriving Show
instance FromJSON OptionType where
  parseJSON (String "Put") = return PutOption 
  parseJSON (String "Call") = return CallOption 
  parseJSON x = fail (show x)

data Quote
  = StockQuote {
      sqAskPrice :: Double
    , sqLatestAskSize :: Int
    , sqLatestAskTime :: Pending
    , sqReportedPrecision :: Maybe Int
    , sqBidPrice :: Double
    , sqLatestBidSize :: Int
    , sqLatestBidTime :: Pending
    , sqChangeSincePriorDayClose :: Pending
    , sqChangeSign :: Maybe Ordering
    , sqChangeText :: Maybe String
    , sqPreviousClose :: Double
    , sqDateTime :: Pending
    , sqLastTradeDate :: Pending
    , sqTotalValueTradedToday :: Double
    , sqExchangeDescription :: String
    , sqExchangeCode :: Pending
    , sqTodayHighTradePrice :: Double
    , sqLastTradeVolume :: Int
    , sqLastTradePrice :: Double
    , sqTodayLowTradePrice :: Double
    , sqCompanyName :: String
    , sqOpenTradePrice :: Double
    , sqPercentageChangeFromPriorDayClose :: Double
    , sqPercentageChangeSign :: Maybe Ordering
    , sqPriorDayClose :: Double
    , sqPriorDayHighTradePrice :: Double
    , sqPriorDayLowTradePrice :: Double
    , sqPriorDayOpenPrice :: Double
    , sqPriorDayPriceChange :: Double
    , sqPriorLastTradeDate :: Pending
    , sqPriorDayTotalVolume :: Int
    , sqSecurityIsOption :: Bool
    , sqTradingSession :: TradingSession
    , sqSymbol :: String
    , sqTradeConditionCode :: TradeConditionCode
    , sqTimestamp :: Pending
    , sqTickDirFromPriorTrade :: Ordering
    , sqTrendBasedOn10PriorTicks :: Maybe Ordering
    , sqNumTradesSinceMarketOpen :: Int
    , sqCumulativeVolume :: Int
    , sqVolumeWeightedAveragePrice :: Double
    , sq52WeekHigh :: Double
    , sq52WeekHighDate :: Pending
    , sq52WeekLow :: Double
    , sq52WeekLowDate :: Pending
    , sq100DayAveragePrice :: Double
    , sq200DayAveragePrice :: Double
    , sq50DayAveragePrice :: Double
    , sq21DayAverageVolume :: Int
    , sq30DayAverageVolume :: Int
    , sq90DaysAverageVolume :: Int
    , sqBeta :: Double
    , sqTickDirSinceLastBid :: Ordering
    , sqCusip :: Maybe String
    , sqExDividendDate :: Pending
    , sqDividendFrequency :: DividendFrequency
    , sqLatestAnnouncedCashDividend :: Double
    , sqLastAnnouncedDividendPayDate :: Pending
    , sqEarningsPerShare :: Double
    , sqIndicatedAnnualDividend :: Pending
    , sqSecurityHasOptions :: Bool
    , sqPriceEarningsRatio :: Double
    , sqPrior100DayAveragePrice :: Double
    , sqPrior200DayAveragePrice :: Double
    , sqPrior50DayAveragePrice :: Double
    , sqBookValuePrice :: Double
    , sqSharesOutstanding :: Int
    , sqOneYearVolatilityMeasure :: Double
    , sqDividendYield :: Double
    }
  | OptionQuote {
      oqAskPrice :: Double
    , oqLatestAskSize :: Int
    , oqLatestAskTime :: Pending
    , oqReportedPrecision :: Maybe Int
    , oqBidPrice :: Double
    , oqLatestBidSize :: Int
    , oqLatestBidTime :: Pending
    , oqChangeSincePriorDayClose :: Pending
    , oqChangeSign :: Maybe Ordering
    , oqChangeText :: Maybe String
    , oqPreviousClose :: Double
    , oqDateTime :: Pending
    , oqLastTradeDate :: Pending
    , oqTotalValueTradedToday :: Double
    , oqExchangeDescription :: String
    , oqExchangeCode :: Pending
    , oqTodayHighTradePrice :: Double
    , oqLastTradeVolume :: Int
    , oqLastTradePrice :: Double
    , oqTodayLowTradePrice :: Double
    , oqCompanyName :: String
    , oqOpenTradePrice :: Double
    , oqPercentageChangeFromPriorDayClose :: Double
    , oqPercentageChangeSign :: Maybe Ordering
    , oqPriorDayClose :: Double
    , oqPriorDayHighTradePrice :: Double
    , oqPriorDayLowTradePrice :: Double
    , oqPriorDayOpenPrice :: Double
    , oqPriorDayPriceChange :: Double
    , oqPriorLastTradeDate :: Pending
    , oqPriorDayTotalVolume :: Int
    , oqSecurityIsOption :: Bool
    , oqTradingSession :: TradingSession
    , oqSymbol :: String
    , oqTradeConditionCode :: TradeConditionCode
    , oqTimestamp :: Pending
    , oqTickDirFromPriorTrade :: Ordering
    , oqTrendBasedOn10PriorTicks :: Maybe Ordering
    , oqNumTradesSinceMarketOpen :: Int
    , oqCumulativeVolume :: Int
    , oqVolumeWeightedAveragePrice :: Double
    , oq52WeekHigh :: Double
    , oq52WeekHighDate :: Pending
    , oq52WeekLow :: Double
    , oq52WeekLowDate :: Pending
    , oqContractSize :: Int
    , oqDaysUntilExpiration :: Int
    , oqImpliedVolatilityDelta  :: Double
    , oqImpliedVolatilityGamma :: Double
    , oqImpliedVolatilityPrice :: Double
    , oqImpliedVolatilityRho :: Double
    , oqIssueDescription :: String
    , oqImpliedVolatilityTheta :: Double
    , oqImpliedVolatilityVega :: Double
    , oqSettlementDesignation :: OptionDelivery
    , oqOpenInterest :: Double
    , oqOptionStyle :: OptionStyle
    , oqOptionClass :: OptionClass
    , oqEstimatedOptionValue :: Double -- via Ju/Zhong or Black-Scholes
    , oqPremiumMultiplier :: Double
    , oqPriorDayOpenInterest :: Double
    , oqOptionType :: OptionType
    , oqConditionCode :: String
    , oqRootSymbol :: String
    , oqStrikePrice :: Double
    , oqUnderlyingCusip :: String
    , oqUnderlyingSymbol :: String
    , oqExpirationDate :: Pending
    , oqExpirationDay :: Pending
    , oqExpirationMonth :: Pending
    , oqExpirationYear :: Pending
    }
  deriving Show

toOrd :: String -> Ordering
toOrd "u" = GT
toOrd "e" = EQ
toOrd "d" = LT

toOrdMay :: String -> Maybe Ordering
toOrdMay "na" = Nothing
toOrdMay x = Just $ toOrd x

toBool :: String -> Bool
toBool "0" = False
toBool "1" = True

toStrMay :: String -> Maybe String
toStrMay "na" = Nothing
toStrMay x = Just x

instance FromJSON Quote where
  parseJSON (Object o)
    = do
      askPrice                          <-          read <$> o .: "ask"
      latestAskSize                     <- (*100) . read <$> o .: "asksz"
      latestAskTime                     <-                   o .: "ask_time"
      reportedPrecision                 <-          readMay <$> o .: "basis"
      bidPrice                          <-          read <$> o .: "bid"
      latestBidSize                     <- (*100) . read <$> o .: "bidsz"
      latestBidTime                     <-                   o .: "bid_time"
      changeSincePriorDayClose          <-                   o .: "chg"
      changeSign                        <-         toOrdMay <$> o .: "chg_sign"
      changeText                        <-          toStrMay<$> o .: "chg_t"
      previousClose                     <-          read <$> o .: "cl"
      dateTime                          <-                   o .: "datetime"
      lastTradeDate                     <-                   o .: "date"
      totalValueTradedToday             <-          read <$> o .: "dollar_value"
      exchangeDescription               <-                   o .: "exch_desc"
      exchangeCode                      <-                   o .: "exch"
      todayHighTradePrice               <-          read <$> o .: "hi"
      lastTradeVolume                   <-          read <$> o .: "incr_vl"
      lastTradePrice                    <-          read <$> o .: "last"
      todayLowTradePrice                <-          read <$> o .: "lo"
      companyName                       <-                   o .: "name"
      openTradePrice                    <-          read <$> o .: "opn"
      percentageChangeFromPriorDayClose <-          read <$> o .: "pchg"
      percentageChangeSign              <-         toOrdMay <$> o .: "pchg_sign"
      priorDayClose                     <-          read <$> o .: "pcls"
      priorDayHighTradePrice            <-          read <$> o .: "phi"
      priorDayLowTradePrice             <-          read <$> o .: "plo"
      priorDayOpenPrice                 <-          read <$> o .: "popn"
      priorDayPriceChange               <-          read <$> o .: "prchg"
      priorLastTradeDate                <-                   o .: "pr_date"
      priorDayTotalVolume               <-          read <$> o .: "pvol"
      securityIsOption                  <-      toBool <$>   o .: "secclass"
      tradingSession                    <-                   o .: "sesn"
      symbol                            <-                   o .: "symbol"
      tradeConditionCode                <-                 o .: "tcond"
      timestamp                         <-                   o .: "timestamp"
      tickDirFromPriorTrade             <-         toOrd <$> o .: "tradetick"
      trendBasedOn10PriorTicks          <-         toOrdMay <$> o .: "trend"
      numTradesSinceMarketOpen          <-          read <$> o .: "tr_num"
      cumulativeVolume                  <-          read <$> o .: "vl"
      volumeWeightedAveragePrice        <-          read <$> o .: "vwap"
      week52High                        <-          read <$> o .: "wk52hi"
      week52HighDate                    <-                   o .: "wk52hidate"
      week52Low                         <-          read <$> o .: "wk52lo"
      week52LowDate                     <-                   o .: "wk52lodate"
      if securityIsOption
      then  OptionQuote
              askPrice latestAskSize latestAskTime reportedPrecision bidPrice
              latestBidSize latestBidTime changeSincePriorDayClose changeSign changeText
              previousClose dateTime lastTradeDate totalValueTradedToday exchangeDescription
              exchangeCode todayHighTradePrice lastTradeVolume lastTradePrice todayLowTradePrice
              companyName openTradePrice percentageChangeFromPriorDayClose percentageChangeSign priorDayClose
              priorDayHighTradePrice priorDayLowTradePrice priorDayOpenPrice priorDayPriceChange priorLastTradeDate
              priorDayTotalVolume securityIsOption tradingSession symbol tradeConditionCode
              timestamp tickDirFromPriorTrade trendBasedOn10PriorTicks numTradesSinceMarketOpen cumulativeVolume
              volumeWeightedAveragePrice week52High week52HighDate week52Low week52LowDate
              <$> (read <$> o .: "contract_size")
              <*> (read <$> o .: "days_to_expiration")
              <*> (read <$> o .: "idelta")
              <*> (read <$> o .: "igamma")
              <*> (read <$> o .: "imp_volatility")
              <*> (read <$> o .: "irho")
              <*> (read <$> o .: "issue_desc")
              <*> (read <$> o .: "itheta")
              <*> (read <$> o .: "ivega")
              <*> (       o .: "op_delivery")
              <*> (read <$> o .: "openinterest")
              <*> (       o .: "op_style")
              <*> (       o .: "op_subclass")
              <*> (read <$> o .: "opt_val")
              <*> (read <$> o .: "prem_mult")
              <*> (read <$> o .: "pr_openinterest")
              <*> (       o .: "put_call")
              <*> (read <$> o .: "qcond")
              <*> (read <$> o .: "rootsymbol")
              <*> (read <$> o .: "strikeprice")
              <*> (read <$> o .: "under_cusip")
              <*> (read <$> o .: "undersymbol")
              <*> (read <$> o .: "xdate")
              <*> (read <$> o .: "xday")
              <*> (read <$> o .: "xmonth")
              <*> (read <$> o .: "xyear")
      else  StockQuote
              askPrice latestAskSize latestAskTime reportedPrecision bidPrice
              latestBidSize latestBidTime changeSincePriorDayClose changeSign changeText
              previousClose dateTime lastTradeDate totalValueTradedToday exchangeDescription
              exchangeCode todayHighTradePrice lastTradeVolume lastTradePrice todayLowTradePrice
              companyName openTradePrice percentageChangeFromPriorDayClose percentageChangeSign priorDayClose
              priorDayHighTradePrice priorDayLowTradePrice priorDayOpenPrice priorDayPriceChange priorLastTradeDate
              priorDayTotalVolume securityIsOption tradingSession symbol tradeConditionCode
              timestamp tickDirFromPriorTrade trendBasedOn10PriorTicks numTradesSinceMarketOpen cumulativeVolume
              volumeWeightedAveragePrice week52High week52HighDate week52Low week52LowDate
              <$> (read <$> o .: "adp_100")
              <*> (read <$> o .: "adp_200")
              <*> (read <$> o .: "adp_50")
              <*> (read <$> o .: "adv_21")
              <*> (read <$> o .: "adv_30")
              <*> (read <$> o .: "adv_90")
              <*> (read <$> o .: "beta")
              <*> (toOrd<$> o .: "bidtick")
              <*> (toStrMay<$> o .: "cusip")
              <*> (         o .: "divexdate")
              <*> (       o .: "divfreq")
              <*> (read <$> o .: "div")
              <*> (         o .: "divpaydt")
              <*> (read <$> o .: "eps")
              <*> (         o .: "iad")
              <*> (toBool<$>o .: "op_flag")
              <*> (read <$> o .: "pe")
              <*> (read <$> o .: "pr_adp_100")
              <*> (read <$>  o .: "pr_adp_200")
              <*> (read <$>  o .: "pr_adp_50")
              <*> (read <$> o .: "prbook")
              <*> (read <$> o .: "sho")
              <*> (read <$>  o .: "volatility12")
              <*> (read <$>  o .: "yield")
  parseJSON x = fail . show $ x

--------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------

data QuoteResult
  = QuoteResult {
    unQuoteResult :: Quote
  }
instance FromJSON QuoteResult
  where
  parseJSON (Object o)
    = QuoteResult <$> (o .: "response" >>= (.: "quotes") >>= (.: "quote"))

data QuotesResult
  = QuotesResult {
    unQuotesResult :: [Quote]
  }
instance FromJSON QuotesResult
  where
  parseJSON (Object o)
    = QuotesResult <$> (o .: "response" >>= (.: "quotes") >>= (.: "quote"))

