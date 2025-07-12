{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PaymentService where

import Types
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Hashable (hash)


class PaymentProcessorService m where
  processPayment :: PaymentRequest -> m (Either String ProcessorType)

newtype MockPaymentProcessor a = MockPaymentProcessor (IO a)
  deriving (Functor, Applicative, Monad)

instance PaymentProcessorService MockPaymentProcessor where
  processPayment req = MockPaymentProcessor $ do
    -- Simple mock logic: try default first, fallback on failure
    -- For demo purposes, we'll succeed with default 80% of the time
    uuid <- UUID.nextRandom
    let uuidString = UUID.toString uuid
        hashValue = abs $ hash uuidString
    if hashValue `mod` 5 == 0 
      then return $ Right FallbackProcessor
      else return $ Right DefaultProcessor

runMockPaymentProcessor :: MockPaymentProcessor a -> IO a
runMockPaymentProcessor (MockPaymentProcessor action) = action