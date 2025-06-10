{-# LANGUAGE OverloadedStrings #-}

module Logic where

import Config (currencies)
import Types
import qualified Data.Text as T
import Network.HTTP.Req
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import Data.Time (parseTimeM, defaultTimeLocale, UTCTime)





-- | Processes a payment transaction by validating its content and token.
-- 
-- Input:
--   - cfg: configuration data required to confirm or cancel a transaction
--   - trans: the transaction payload received from the webhook
--
-- Output:
--   - IO (Maybe Bool)
--     * Just True  → valid transaction, confirmed
--     * Just False → invalid payload, transaction canceled
--     * Nothing    → fake token detected, transaction ignored
processPayment :: WebhookConfig -> Maybe T.Text -> Transaction -> IO (Maybe Bool)
processPayment cfg token trans
  -- Invalid Transaction (wrong amount or unknown currency)
  | not (validateTransaction trans) = do
      -- Log the invalid payload
      putStrLn "❌ Pagamento inválido — cancelando transação."
      -- Cancel the transaction using the provided configuration
      cancelTransaction cfg trans
      -- Return Just False to indicate the transaction was invalid
      return (Just False)

  -- Wrong Token
  | not (validateToken token) = do
      -- Log the fake token detection
      putStrLn "⚠️ Token falso detectado — ignorando transação."
      -- No action needed for fake tokens, just returns Nothing to indicate no action taken
      return Nothing

  -- Valid Transaction 
  | otherwise = do
      -- Log the valid transaction
      putStrLn "✅ Pagamento válido — confirmando transação."
      -- Confirm the transaction using the provided configuration
      confirmTransaction cfg trans
      -- Return Just True to indicate the transaction was valid and confirmed
      return (Just True)



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
