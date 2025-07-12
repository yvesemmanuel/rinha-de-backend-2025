{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import API
import Handlers


server :: Server API
server = paymentsHandler :<|> paymentsSummaryHandler :<|> healthHandler

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
  putStrLn "Starting Payment Processor API on port 8080..."
  putStrLn "Available endpoints:"
  putStrLn "  POST /payments - Process a payment"
  putStrLn "  GET /payments-summary - Get payments summary"
  putStrLn "  GET /health - Health check"
  putStrLn ""
  putStrLn "Example usage:"
  putStrLn "  curl -X POST http://localhost:8080/payments \\"
  putStrLn "    -H \"Content-Type: application/json\" \\"
  putStrLn "    -d '{\"correlationId\": \"550e8400-e29b-41d4-a716-446655440000\", \"amount\": 100.50}'"
  putStrLn ""
  putStrLn "  curl http://localhost:8080/payments-summary"
  putStrLn ""
  run 8080 app
