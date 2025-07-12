{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import API
import Handlers
import Logger

-- Server
server :: Server API
server = paymentsHandler :<|> paymentsSummaryHandler :<|> healthHandler

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
  let ctx = withOperation "startup" $ withComponent "main"
  
  runConsoleLogger $ logInfoWith ctx "Starting Payment Processor API on port 8080"
  runConsoleLogger $ logInfoWith ctx "Available endpoints:"
  runConsoleLogger $ logInfoWith ctx "  POST /payments - Process a payment"
  runConsoleLogger $ logInfoWith ctx "  GET /payments-summary - Get payments summary"
  runConsoleLogger $ logInfoWith ctx "  GET /health - Health check"
  
  runConsoleLogger $ logDebugWith ctx "Example usage:"
  runConsoleLogger $ logDebugWith ctx "  curl -X POST http://localhost:8080/payments \\"
  runConsoleLogger $ logDebugWith ctx "    -H \"Content-Type: application/json\" \\"
  runConsoleLogger $ logDebugWith ctx "    -d '{\"correlationId\": \"550e8400-e29b-41d4-a716-446655440000\", \"amount\": 100.50}'"
  runConsoleLogger $ logDebugWith ctx ""
  runConsoleLogger $ logDebugWith ctx "  curl http://localhost:8080/payments-summary"
  
  runConsoleLogger $ logInfoWith ctx "Server ready to accept connections"
  run 8080 app