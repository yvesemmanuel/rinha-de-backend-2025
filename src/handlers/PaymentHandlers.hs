{-# LANGUAGE OverloadedStrings #-}

module Handlers.PaymentHandlers
  (
    paymentsHandler
  , paymentsSummaryHandler
  , serviceHealthHandler
  ) where

import Servant
  ( err500, throwError, Handler, ServerError(errBody) )
import Control.Monad.IO.Class (liftIO)
import Data.Time ( getCurrentTime, UTCTime )
import qualified Data.Text as T

import Models.Payment
  ( ProcessorSummary(totalRequests),
    PaymentsSummary(defaultSummary, fallbackSummary),
    ProcessorType(FallbackProcessor, DefaultProcessor),
    PaymentRecord(recordProcessor, PaymentRecord, recordCorrelationId,
                  recordAmount, recordRequestedAt),
    PaymentRequest(amount, correlationId) )
import Storage.PaymentStorage
  ( PaymentStorage(getPaymentsSummary, storePayment),
    runInMemoryStorage )
import Services.PaymentService
  ( PaymentProcessorService(processPayment),
    runMockPaymentProcessor )
import Logger.Logger
  ( Logger(logDebugWith, logErrorWith, logInfoWith),
    withComponent,
    withOperation,
    addCorrelationId,
    addMetadata,
    runConsoleLogger )

paymentsHandler :: PaymentRequest -> Handler ()
paymentsHandler req = do
  let ctx = addMetadata "amount" (T.pack $ show $ amount req) $
            withOperation "process_payment" $
            addCorrelationId (Models.Payment.correlationId req) $
            withComponent "handler"
  
  liftIO $ runConsoleLogger $ logInfoWith ctx "Processing payment request"
  
  currentTime <- liftIO getCurrentTime

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
            { recordCorrelationId = Models.Payment.correlationId req
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

serviceHealthHandler :: Handler String
serviceHealthHandler = do
  let ctx = withOperation "health_check" $ withComponent "handler"
  liftIO $ runConsoleLogger $ logDebugWith ctx "Health check requested"
  return "Payment processor is healthy"