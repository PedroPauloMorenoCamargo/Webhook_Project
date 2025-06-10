{-# LANGUAGE OverloadedStrings #-}

module Logic where

import Config (currencies)
import Types
import qualified Data.Text as T
import Network.HTTP.Req
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import Data.Time (parseTimeM, defaultTimeLocale, UTCTime)




-- | Processes a payment transaction by executing side effects based on its validity.
--
-- This function delegates the decision logic to `decideAction` and performs
-- the corresponding action:
--   - If the transaction is invalid (e.g., wrong amount or currency), it logs and cancels it;
--   - If the token is invalid, it logs and ignores the transaction;
--   - If valid, it logs and confirms the transaction.
--
-- Input:
--   - cfg: configuration with confirmation and cancellation endpoints
--   - token: optional authorization token
--   - tx: the incoming transaction to process
--
-- Output:
--   - IO-wrapped result:
--     * Just True  → valid and confirmed transaction
--     * Just False → invalid transaction, canceled
--     * Nothing    → token was invalid, transaction ignored
processPayment :: WebhookConfig -> Maybe T.Text -> Transaction -> IO (Maybe Bool)
processPayment cfg token tx = case decideAction token tx of
  Just False -> do
    putStrLn "❌ Pagamento inválido — cancelando transação."
    cancelTransaction cfg tx
    return (Just False)
  Nothing -> do
    putStrLn "⚠️ Token falso detectado — ignorando transação."
    return Nothing
  Just True -> do
    putStrLn "✅ Pagamento válido — confirmando transação."
    confirmTransaction cfg tx
    return (Just True)

-- | Pure decision logic to determine what action should be taken for a transaction.
--
-- This function evaluates:
--   - Whether the transaction is valid (structure, amount, currency, event, timestamp)
--   - Whether the token is valid
--
-- It returns:
--   * Just True  → transaction is valid and should be confirmed
--   * Just False → transaction is invalid and should be canceled
--   * Nothing    → token is invalid and transaction should be ignored
--
-- Input:
--   - token: optional authorization token
--   - tx: transaction payload to evaluate
--
-- Output:
--   - Decision outcome as Maybe Bool
decideAction :: Maybe T.Text -> Transaction -> Maybe Bool
decideAction token tx
  | not (validateTransaction tx) = Just False
  | not (validateToken token)    = Nothing
  | otherwise                    = Just True




-- | Validates the essential fields of a transaction.
-- A transaction is considered valid if:
--   - The amount is greater than zero
--   - The currency is present in the accepted currency list (`currencies`)
--   - The event is valid (e.g., "payment_success")
--   - The timestamp is in the correct format
--
-- Input: Transaction to validate
-- Output: True if the transaction is valid, False otherwise
validateTransaction :: Transaction -> Bool
validateTransaction trans =
  hasValidAmount && isValidCurrency && validEvent && validTimestamp
  where
    hasValidAmount    = amount trans > 0
    isValidCurrency   = Set.member (currency trans) currencies
    validEvent        = event trans == "payment_success"
    validTimestamp    = validateTimestamp (timestamp trans)
    




-- | Validates whether the transaction contains the correct authorization token.
--
-- This basic implementation checks if the token field is exactly equal to "meu-token-secreto".
--
-- Input: Transaction to check
-- Output: True if the token is valid, False otherwise
validateToken :: Maybe T.Text -> Bool
validateToken (Just "meu-token-secreto") = True
validateToken _ = False

-- | Validates whether the given timestamp is in the expected ISO 8601 UTC format: YYYY-MM-DDTHH:MM:SSZ
-- Input: Text representing the timestamp
-- Output: True if the timestamp matches the UTC format exactly, False otherwise
validateTimestamp :: T.Text -> Bool
validateTimestamp = isValidIso8601Utc . T.unpack

-- | Helper function that parses a string and checks if it matches the ISO 8601 UTC format
isValidIso8601Utc :: String -> Bool
isValidIso8601Utc str =
  case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" str :: Maybe UTCTime of
    Just _  -> True
    Nothing -> False


-- | Sends a cancellation request for a given transaction to the external service.
--
-- Input:
--   - cfg: Webhook configuration containing cancellation URL and server port
--   - trans: The transaction to cancel (must include a valid transaction_id)
--
-- Output:
--   - IO action that performs an HTTP POST to the /cancelar endpoint
--     (the response is ignored)
cancelTransaction :: WebhookConfig -> Transaction -> IO ()
cancelTransaction cfg trans = runReq defaultHttpConfig $ do
  -- Transform the transaction into a JSON body
  let body = ReqBodyJson trans
  -- Log the transaction ID being cancelled
  liftIO $ putStrLn $ "➡️ Enviando cancelamento para: " ++ show (transaction_id trans)

  -- Perform the HTTP POST request to the cancellation endpoint
  _ <- req
         POST                         -- HTTP method
         (cancelURL cfg)              -- URL (e.g., http://localhost:5001/cancelar)
         body                         -- JSON-encoded body
         ignoreResponse               -- Ignore the response 
         (serverPort cfg <> header "Content-Type" "application/json") -- Set Content-Type header

  return ()


-- | Sends a confirmation request for a given transaction to the external service.
--
-- Input:
--   - cfg: Webhook configuration containing confirmation URL and server port
--   - trans: The transaction to confirm (must include a valid transaction_id)
--
-- Output:
--   - IO action that performs an HTTP POST to the /confirmar endpoint
--     (the response is ignored)
confirmTransaction :: WebhookConfig -> Transaction -> IO ()
confirmTransaction cfg trans = runReq defaultHttpConfig $ do
  -- Transform the transaction into a JSON body
  let body = ReqBodyJson trans

  -- Log the transaction ID being confirmed
  liftIO $ putStrLn $ "➡️ Enviando confirmação para: " ++ show (transaction_id trans)

  -- Perform the HTTP POST request to the confirmation endpoint
  _ <- req
         POST                                -- HTTP method: POST
         (confirmURL cfg)                    -- Target URL for the confirmation request
         body                                -- JSON-encoded transaction payload
         ignoreResponse                      -- Ignore the response body
         (serverPort cfg <> header "Content-Type" "application/json")  -- Set Content-Type header 

  return ()
