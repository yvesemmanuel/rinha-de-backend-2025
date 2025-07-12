{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Storage.PaymentStorage
  ( -- * Storage class
    PaymentStorage(..)
    -- * In-memory implementation
  , InMemoryStorage(..)
  , runInMemoryStorage
  ) where

import Models.Payment
  ( ProcessorSummary(totalAmount, ProcessorSummary, totalRequests),
    PaymentsSummary(..),
    ProcessorType(FallbackProcessor, DefaultProcessor),
    PaymentRecord(recordCorrelationId, recordAmount, recordRequestedAt,
                  recordProcessor) )
import Logger.Logger
  ( Logger(logInfoWith, logDebugWith),
    withComponent,
    withOperation,
    addCorrelationId,
    addMetadata,
    runConsoleLogger )
import Data.Time ( UTCTime )
import Data.IORef ( IORef, modifyIORef, newIORef, readIORef )
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T

class PaymentStorage m where
  storePayment :: PaymentRecord -> m ()
  getPaymentsSummary :: Maybe UTCTime -> Maybe UTCTime -> m PaymentsSummary

type PaymentStore = IORef [PaymentRecord]

newtype InMemoryStorage a = InMemoryStorage (IO a)
  deriving (Functor, Applicative, Monad)

instance PaymentStorage InMemoryStorage where
  storePayment record = InMemoryStorage $ do
    let ctx = addMetadata "processor" (T.pack $ show $ recordProcessor record) $
              addMetadata "amount" (T.pack $ show $ recordAmount record) $
              withOperation "store_payment" $
              addCorrelationId (recordCorrelationId record) $
              withComponent "storage"
    
    runConsoleLogger $ logDebugWith ctx "Storing payment record"
    
    store <- getPaymentStore
    modifyIORef store (record :)
    
    runConsoleLogger $ logDebugWith ctx "Payment record stored successfully"
    
  getPaymentsSummary maybeFrom maybeTo = InMemoryStorage $ do
    let ctx = withOperation "get_summary" $ withComponent "storage"
    
    runConsoleLogger $ logDebugWith ctx "Retrieving payments for summary"
    
    store <- getPaymentStore
    payments <- readIORef store
    let filteredPayments = filterByDateRange maybeFrom maybeTo payments
        summary = calculateSummary filteredPayments
    
    runConsoleLogger $ logInfoWith 
      (addMetadata "fallback_count" (T.pack $ show $ totalRequests $ fallbackSummary summary) $
       addMetadata "default_count" (T.pack $ show $ totalRequests $ defaultSummary summary) $
       addMetadata "total_payments" (T.pack $ show $ length filteredPayments) ctx)
      "Summary calculated successfully"
    
    return summary

-- Global storage (for demo purposes)
{-# NOINLINE globalPaymentStore #-}
globalPaymentStore :: IORef [PaymentRecord]
globalPaymentStore = unsafePerformIO (newIORef [])

getPaymentStore :: IO (IORef [PaymentRecord])
getPaymentStore = return globalPaymentStore

filterByDateRange :: Maybe UTCTime -> Maybe UTCTime -> [PaymentRecord] -> [PaymentRecord]
filterByDateRange maybeFrom maybeTo payments = 
  let fromFilter = case maybeFrom of
        Just from -> filter (\p -> recordRequestedAt p >= from)
        Nothing -> id
      toFilter = case maybeTo of
        Just to -> filter (\p -> recordRequestedAt p <= to)
        Nothing -> id
  in toFilter $ fromFilter payments

calculateSummary :: [PaymentRecord] -> PaymentsSummary
calculateSummary payments = 
  let defaultPayments = filter (\p -> recordProcessor p == DefaultProcessor) payments
      fallbackPayments = filter (\p -> recordProcessor p == FallbackProcessor) payments
      
      defaultSummary = ProcessorSummary 
        { totalRequests = length defaultPayments
        , totalAmount = sum $ map recordAmount defaultPayments
        }
      
      fallbackSummary = ProcessorSummary
        { totalRequests = length fallbackPayments
        , totalAmount = sum $ map recordAmount fallbackPayments
        }
  in PaymentsSummary defaultSummary fallbackSummary

runInMemoryStorage :: InMemoryStorage a -> IO a
runInMemoryStorage (InMemoryStorage action) = action