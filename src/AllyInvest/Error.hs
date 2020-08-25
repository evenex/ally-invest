module AllyInvest.Error (
  AllyInvestError(..)
, Pending
) where

import           Network.HTTP.Client

data AllyInvestError
  = NetworkError HttpException
  | DecodeFailed String
  deriving Show

type Pending = String
