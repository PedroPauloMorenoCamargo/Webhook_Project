{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Webhook (WebhookAPI, server) where

import Servant
import Types
import Logic (processPayment, cancelTransaction)
import Config (defaultConfig)

import Control.Monad.IO.Class (liftIO)

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KM



-- | Definition of the HTTP server for the WebhookAPI
server :: Server WebhookAPI
server = webhookHandler

-- | Top-level webhook handler that receives the token and JSON payload.
-- If the JSON is valid, passes it to `handleSuccess`; otherwise handles parse failure.
webhookHandler :: Maybe T.Text -> Value -> Handler NoContent
webhookHandler mToken val = case fromJSON val of
  Success tx -> handleSuccess mToken tx
  Error err  -> handleFailure err val


-- | Handles the case when the JSON is successfully parsed into a Transaction
-- Input: Parsed `Transaction`
-- Output: Either NoContent or error via `throwError`
handleSuccess :: Maybe T.Text -> Transaction -> Handler NoContent
handleSuccess token tx = do
  -- Log the received token (or "none" if missing)
  liftIO $ putStrLn $ "🔑 Token recebido: " ++ T.unpack (maybe "<ausente>" id token)
  -- Log the received transaction
  liftIO $ putStrLn $ "Recebido no webhook: " ++ show tx
  -- Process the payment
  result <- liftIO $ processPayment defaultConfig token tx
  case result of
    -- Invalid token: reject with HTTP 403
    Nothing    -> throwError err403 { errBody = "Token inválido" }
    -- Invalid payload or incorrect amount: reject with HTTP 400
    Just False -> throwError err400 { errBody = "Payload inválido ou amount incorreto" }
    -- Valid transaction: confirm with 200 and return NoContent
    Just True  -> pure NoContent

-- | Handles the case when the JSON fails to parse
-- Input: Parsing error message and raw JSON value
-- Output: Throws error with appropriate message, possibly cancels the transaction
handleFailure :: String -> Value -> Handler NoContent
handleFailure err val = do
  -- Log the parsing error
  liftIO $ putStrLn $ "❌ Erro de parsing: " ++ err
  -- Attempt to extract the transaction ID from the raw JSON
  let txId = extractTransactionId val
  case txId of
    Just tid -> do
      -- Cancel the transaction using the extracted ID
      liftIO $ cancelTransaction defaultConfig (Transaction "invalid" tid 0 "BRL" "")
       -- Throw an error indicating the the transaction fields are missing or invalid
      throwError err400 { errBody = "Campos ausentes ou inválidos" }
    Nothing ->
      -- If no transaction ID is found, throw a more specific error
      throwError err400 { errBody = "Campos ausentes e transaction_id não localizado" }


-- | Attempts to extract the "transaction_id" field from a raw JSON object.
-- Input: JSON `Value`
-- Output: Maybe Text containing the transaction ID, if found
extractTransactionId :: Value -> Maybe T.Text
extractTransactionId (Object o) = case KM.lookup "transaction_id" o of
  Just (String t) -> Just t         -- Found and is a string
  _               -> Nothing        -- Found but wrong type
extractTransactionId _ = Nothing    -- Not an object
