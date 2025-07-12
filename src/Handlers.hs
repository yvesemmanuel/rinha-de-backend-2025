{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Servant
import Control.Monad.IO.Class (liftIO)
import Data.Time
import qualified Data.Text as T

import Types
import Storage
import PaymentService
import Logger

paymentsHandler :: PaymentRequest -> Handler ()
paymentsHandler req = do
  let ctx = addMetadata "amount" (T.pack $ show $ amount req) $
            withOperation "process_payment" $
            addCorrelationId (Types.correlationId req) $
            withComponent "handler"
  
  liftIO $ runConsoleLogger $ logInfoWith ctx "Processing payment request"
  
  currentTime <- liftIO getCurrentTime

  -- Process payment (mock)
  result <- liftIO $ runMockPaymentProcessor $ processPayment req
  
  case result of
    Left err -> do
      liftIO $ runConsoleLogger $ logErrorWith ctx $ "Payment processing failed: " <> T.pack err
      throwError err500 { errBody = "Payment processing failed" }
    Right processor -> do
      let processorText = case processor of
            DefaultProcessor -> "default"
            FallbackProcessor -> "fallback"
      
      liftIO $ runConsoleLogger $ logInfoWith 
        (addMetadata "processor" processorText ctx) 
        "Payment processed successfully"

      let record = PaymentRecord 
            { recordCorrelationId = Types.correlationId req
            , recordAmount = amount req
            , recordRequestedAt = currentTime
            , recordProcessor = processor
            }
      liftIO $ runInMemoryStorage $ storePayment record
      
      liftIO $ runConsoleLogger $ logDebugWith ctx "Payment stored in database"
      return ()

paymentsSummaryHandler :: Maybe UTCTime -> Maybe UTCTime -> Handler PaymentsSummary
paymentsSummaryHandler maybeFrom maybeTo = do
  let ctx = withOperation "get_summary" $ withComponent "handler"
      ctxWithDates = case (maybeFrom, maybeTo) of
        (Just from, Just to) -> addMetadata "to" (T.pack $ show to) $ 
                               addMetadata "from" (T.pack $ show from) ctx
        (Just from, Nothing) -> addMetadata "from" (T.pack $ show from) ctx
        (Nothing, Just to) -> addMetadata "to" (T.pack $ show to) ctx
        (Nothing, Nothing) -> ctx
  
  liftIO $ runConsoleLogger $ logInfoWith ctxWithDates "Retrieving payments summary"
  
  summary <- liftIO $ runInMemoryStorage $ getPaymentsSummary maybeFrom maybeTo
  
  liftIO $ runConsoleLogger $ logDebugWith 
    (addMetadata "fallback_requests" (T.pack $ show $ totalRequests $ fallbackSummary summary) $
     addMetadata "default_requests" (T.pack $ show $ totalRequests $ defaultSummary summary) ctxWithDates)
    "Summary retrieved successfully"
  
  return summary

healthHandler :: Handler String
healthHandler = do
  let ctx = withOperation "health_check" $ withComponent "handler"
  liftIO $ runConsoleLogger $ logDebugWith ctx "Health check requested"
  return "Payment processor is healthy"