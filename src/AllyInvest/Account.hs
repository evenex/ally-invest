{-# LANGUAGE OverloadedStrings #-}

module AllyInvest.Account (
  getAccounts 
, AccountSummary(..)
, Balance(..)
, BuyingPower(..)
, MoneyBalance(..)
, SecuritiesBalance(..)
, Holding(..)
, Instrument(..)
, AccountType(..)
) where

import           AllyInvest.Credentials
import           AllyInvest.Error
import           AllyInvest.Internal

import           Data.Aeson
import           Data.Time

getAccounts :: Credentials -> IO (Either AllyInvestError AccountSummary)
getAccounts cred = fetchAndDecode cred "https://api.tradeking.com/v1/accounts.json"

data AccountSummary
  = AccountSummary {
    aNumber :: Int
  , aBalance :: Balance
  , aHoldings :: [Holding]
  } deriving Show
instance FromJSON AccountSummary
  where
  parseJSON (Object o)
    = let o' = o .: "response" >>= (.: "accounts") >>= (.: "accountsummary")
      in
      AccountSummary
      <$> (fmap read $ o' >>= (.: "account"))
      <*> (        o' >>= (.: "accountbalance"))
      <*> (        o' >>= (.: "accountholdings") >>= (.: "holding"))

data Balance
  = Balance {
    bAccountValue :: Double
  , bBuyingPower :: BuyingPower
  , bFedCall :: Double
  , bHouseCall :: Double
  , bMoney :: MoneyBalance
  , bSecurities :: SecuritiesBalance
  } deriving Show
instance FromJSON Balance
  where
  parseJSON (Object o)
    = Balance
      <$> (read <$> o .: "accountvalue")
      <*> (       o .: "buyingpower")
      <*> (read <$> o .: "fedcall")
      <*> (read <$> o .: "housecall")
      <*> (       o .: "money")
      <*> (       o .: "securities")

data BuyingPower
  = BuyingPower {
    bpCashAvailableForWithdrawal :: Double
  , bpDayTrading :: Double
  , bpEquityPercentage :: Double
  , bpOptions :: Double
  , bpSodDayTrading :: Double
  , bpSodOptions :: Double
  , bpSodStock :: Double
  , bpStock :: Double
  } deriving Show
instance FromJSON BuyingPower
  where
  parseJSON (Object o)
    = BuyingPower
      <$> (read <$> o .: "cashavailableforwithdrawal")
      <*> (read <$> o .: "daytrading")
      <*> (read <$> o .: "equitypercentage")
      <*> (read <$> o .: "options")
      <*> (read <$> o .: "soddaytrading")
      <*> (read <$> o .: "sodoptions")
      <*> (read <$> o .: "sodstock")
      <*> (read <$> o .: "stock")

data MoneyBalance
  = MoneyBalance {
    mbAccruedInterest :: Double
  , mbCash :: Double
  , mbCashAvailable :: Double
  , mbMarginBalance :: Double
  , mbMoneyMarketFund :: Double
  , mbTotal :: Double
  , mbUnclearedDeposits :: Double
  , mbUnsettledFunds :: Double
  , mbYield :: Double
  } deriving Show
instance FromJSON MoneyBalance
  where
  parseJSON (Object o)
    = MoneyBalance
      <$> (read <$> o .: "accruedinterest")
      <*> (read <$> o .: "cash")
      <*> (read <$> o .: "cashavailable")
      <*> (read <$> o .: "marginbalance")
      <*> (read <$> o .: "mmf")
      <*> (read <$> o .: "total")
      <*> (read <$> o .: "uncleareddeposits")
      <*> (read <$> o .: "unsettledfunds")
      <*> (read <$> o .: "yield")

data SecuritiesBalance
  = SecuritiesBalance {
    sbLongOptions :: Double
  , sbLongStocks :: Double
  , sbOptions :: Double
  , sbShortOptions :: Double
  , sbShortStocks :: Double
  , sbStocks :: Double
  , sbTotal :: Double
  } deriving Show
instance FromJSON SecuritiesBalance
  where
  parseJSON (Object o)
    = SecuritiesBalance
      <$> (read <$> o .: "longoptions")
      <*> (read <$> o .: "longstocks")
      <*> (read <$> o .: "options")
      <*> (read <$> o .: "shortoptions")
      <*> (read <$> o .: "shortstocks")
      <*> (read <$> o .: "stocks")
      <*> (read <$> o .: "total")

data Holding
  = Holding {
    hAccountType :: AccountType
  , hCostBasis :: Double
  , hGainLoss :: Double
  , hInstrument :: Instrument
  , hMarketValue :: Double
  , hMarketValueChange :: Double
  , hPrice :: Double
  , hPurchasePrice :: Double
  , hQuantity :: Double
  , hQuoteChange :: Double
  , hQuoteLastPrice :: Double
  } deriving Show
instance FromJSON Holding
  where
  parseJSON (Object o)
    = Holding
      <$> (       o .: "accounttype")
      <*> (read <$> o .: "costbasis")
      <*> (read <$> o .: "gainloss")
      <*> (       o .: "instrument")
      <*> (read <$> o .: "marketvalue")
      <*> (read <$> o .: "marketvaluechange")
      <*> (read <$> o .: "price")
      <*> (read <$> o .: "purchaseprice")
      <*> (read <$> o .: "qty")
      <*> (read <$> (o .: "quote" >>= (.: "change")))
      <*> (read <$> (o .: "quote" >>= (.: "lastprice")))

data Instrument
  = Instrument {
    iCusip :: String
  , iDesc :: String
  , iFactor :: Double
  , iMaturityDate :: ZonedTime
  , iMaturityMonthYear :: Maybe String
  , iMultiplier :: Double
  , iPutOrCall :: String
  , iSecurityType :: String
  , iStrikePrice :: Double
  , iSymbol :: String
  } deriving Show
instance FromJSON Instrument
  where
  parseJSON (Object o)
    = Instrument
      <$> (       o .: "cusip")
      <*> (       o .: "desc")
      <*> (read <$> o .: "factor")
      <*> (       o .: "matdt")
      <*> (       o .: "mmy")
      <*> (read <$> o .: "mult")
      <*> (       o .: "putcall")
      <*> (       o .: "sectyp")
      <*> (read <$> o .: "strkpx")
      <*> (       o .: "sym")

data AccountType = CashAccount | MarginLongAccount | MarginShortAccount
  deriving Show
instance FromJSON AccountType where
  parseJSON v = case (fromJSON v :: Result String) of
                  Error err -> fail err
                  Success t -> case t of
                                 "1" -> return CashAccount
                                 "2" -> return MarginLongAccount
                                 "5" -> return MarginShortAccount
                                 _ -> fail "bad accounttype enum"
