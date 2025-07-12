{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Storage where

import Types
import Data.Time
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)


class PaymentStorage m where
  storePayment :: PaymentRecord -> m ()
  getPaymentsSummary :: Maybe UTCTime -> Maybe UTCTime -> m PaymentsSummary

type PaymentStore = IORef [PaymentRecord]

newtype InMemoryStorage a = InMemoryStorage (IO a)
  deriving (Functor, Applicative, Monad)

instance PaymentStorage InMemoryStorage where
  storePayment record = InMemoryStorage $ do
    store <- getPaymentStore
    modifyIORef store (record :)
    
  getPaymentsSummary maybeFrom maybeTo = InMemoryStorage $ do
    store <- getPaymentStore
    payments <- readIORef store
    let filteredPayments = filterByDateRange maybeFrom maybeTo payments
    return $ calculateSummary filteredPayments

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