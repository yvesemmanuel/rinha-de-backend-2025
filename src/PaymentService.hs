{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module PaymentService where

import Types
import Logger
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Hashable (hash)
import qualified Data.Text as T

-- Payment Processing Service abstraction
class PaymentProcessorService m where
  processPayment :: PaymentRequest -> m (Either String ProcessorType)

-- Mock implementation that simulates payment processing
newtype MockPaymentProcessor a = MockPaymentProcessor (IO a)
  deriving (Functor, Applicative, Monad)

instance PaymentProcessorService MockPaymentProcessor where
  processPayment req = MockPaymentProcessor $ do
    let ctx = addMetadata "amount" (T.pack $ show $ amount req) $
              withOperation "process_payment" $
              addCorrelationId (Types.correlationId req) $
              withComponent "payment_service"
    
    runConsoleLogger $ logInfoWith ctx "Starting payment processing"
    
    -- Simple mock logic: try default first, fallback on failure
    -- For demo purposes, we'll succeed with default 80% of the time
    uuid <- UUID.nextRandom
    let uuidString = UUID.toString uuid
        hashValue = abs $ hash uuidString
        useDefault = hashValue `mod` 5 /= 0
        processor = if useDefault then DefaultProcessor else FallbackProcessor
    
    runConsoleLogger $ logInfoWith 
      (addMetadata "hash_value" (T.pack $ show hashValue) $
       addMetadata "processor" (T.pack $ show processor) ctx)
      "Payment processor selected"
    
    if useDefault
      then do
        runConsoleLogger $ logDebugWith ctx "Using default processor"
        return $ Right DefaultProcessor
      else do
        runConsoleLogger $ logWarnWith ctx "Default processor unavailable, using fallback"
        return $ Right FallbackProcessor

-- Helper function to run MockPaymentProcessor
runMockPaymentProcessor :: MockPaymentProcessor a -> IO a
runMockPaymentProcessor (MockPaymentProcessor action) = action