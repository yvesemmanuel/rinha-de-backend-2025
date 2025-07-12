{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Routes.PaymentRoutes
  (
    API
  , PaymentsAPI
  , PaymentsSummaryAPI
  , ServiceHealthAPI
  ) where

import Servant
  ( type (:<|>),
    JSON,
    PlainText,
    QueryParam,
    ReqBody,
    type (:>),
    Get,
    Post )
import Data.Time ( UTCTime )
import Models.Payment ( PaymentsSummary, PaymentRequest )

type PaymentsAPI = "payments" :> ReqBody '[JSON] PaymentRequest :> Post '[JSON] ()

type PaymentsSummaryAPI = "payments-summary" 
                       :> QueryParam "from" UTCTime 
                       :> QueryParam "to" UTCTime 
                       :> Get '[JSON] PaymentsSummary

type ServiceHealthAPI = "payments" :> "service-health" :> Get '[PlainText] String

type API = PaymentsAPI :<|> PaymentsSummaryAPI :<|> ServiceHealthAPI