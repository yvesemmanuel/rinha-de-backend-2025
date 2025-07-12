{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Time
import Data.UUID (UUID)
import GHC.Generics

data PaymentRequest = PaymentRequest
  { correlationId :: UUID
  , amount :: Double
  } deriving (Generic, Show, FromJSON, ToJSON)

data PaymentRecord = PaymentRecord
  { recordCorrelationId :: UUID
  , recordAmount :: Double
  , recordRequestedAt :: UTCTime
  , recordProcessor :: ProcessorType
  } deriving (Generic, Show, FromJSON, ToJSON)

data ProcessorType = DefaultProcessor | FallbackProcessor
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data PaymentsSummary = PaymentsSummary
  { defaultSummary :: ProcessorSummary
  , fallbackSummary :: ProcessorSummary
  } deriving (Generic, Show)

data ProcessorSummary = ProcessorSummary
  { totalRequests :: Int
  , totalAmount :: Double
  } deriving (Generic, Show, FromJSON, ToJSON)

instance ToJSON PaymentsSummary where
  toJSON (PaymentsSummary def fallback) = object
    [ "default" .= def
    , "fallback" .= fallback
    ]

instance FromJSON PaymentsSummary where
  parseJSON = withObject "PaymentsSummary" $ \o -> PaymentsSummary
    <$> o .: "default"
    <*> o .: "fallback"