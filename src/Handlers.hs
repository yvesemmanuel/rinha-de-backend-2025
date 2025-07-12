{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Servant
import Control.Monad.IO.Class (liftIO)
import Data.Time

import Types
import Storage
import PaymentService


paymentsHandler :: PaymentRequest -> Handler ()
paymentsHandler req = do
  currentTime <- liftIO getCurrentTime

  result <- liftIO $ runMockPaymentProcessor $ processPayment req
  
  case result of
    Left err -> throwError err500 { errBody = "Payment processing failed" }
    Right processor -> do
      let record = PaymentRecord 
            { recordCorrelationId = correlationId req
            , recordAmount = amount req
            , recordRequestedAt = currentTime
            , recordProcessor = processor
            }
      liftIO $ runInMemoryStorage $ storePayment record
      return ()

paymentsSummaryHandler :: Maybe UTCTime -> Maybe UTCTime -> Handler PaymentsSummary
paymentsSummaryHandler maybeFrom maybeTo = do
  liftIO $ runInMemoryStorage $ getPaymentsSummary maybeFrom maybeTo

healthHandler :: Handler String
healthHandler = return "Payment processor is healthy"