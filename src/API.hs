{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant
import Data.Time
import Types


type PaymentsAPI = "payments" :> ReqBody '[JSON] PaymentRequest :> Post '[JSON] ()

type PaymentsSummaryAPI = "payments-summary" 
                       :> QueryParam "from" UTCTime 
                       :> QueryParam "to" UTCTime 
                       :> Get '[JSON] PaymentsSummary

type HealthAPI = "health" :> Get '[PlainText] String

type API = PaymentsAPI :<|> PaymentsSummaryAPI :<|> HealthAPI