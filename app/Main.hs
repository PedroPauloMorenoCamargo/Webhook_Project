{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Network.Wai.Handler.Warp (run)
import Servant
import Types (WebhookAPI)
import Webhook (server)


-- | Main function that starts the HTTP server on port 5000
main :: IO ()
main = do
  -- Prints a startup message to the console
  putStrLn "[INFO] 🔥 Starting server on http://localhost:5000/webhook …"
  -- Starts the server on port 5000, serving the API defined by `WebhookAPI` using the `server` handler
  run 5000 (serve (Proxy :: Proxy WebhookAPI) server)