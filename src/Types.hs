{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}


module Types(WebhookConfig(..), Transaction(..), WebhookAPI) where

import GHC.Generics (Generic)
import Data.Text (Text)
import Network.HTTP.Req (Url, Scheme(..), Option)
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Scientific as S
import qualified Data.Aeson.KeyMap as KM
import Servant




-- | Type definition for the Webhook API endpoint.
-- Route: POST /webhook
-- Header: X-Webhook-Token (optional)
-- Body: raw JSON (`Value`)
-- Response: No content (200), or appropriate error
type WebhookAPI =
  "webhook"
  :> Header "X-Webhook-Token" T.Text 
  :> ReqBody '[JSON] Value
  :> Post '[JSON] NoContent



-- | Configuration Model used by the webhook processor to contact external services.
data WebhookConfig = WebhookConfig
  { confirmURL  :: Url 'Http         --   Target URL to POST confirmations (e.g., /confirmar)
  , cancelURL   :: Url 'Http         --   Target URL to POST cancellations (e.g., /cancelar)
  , serverPort  :: Option 'Http      --   Port number (e.g., port 5001) to contact the service
  }

-- | Data model representing a payment transaction received from a webhook request.
data Transaction = Transaction
  { event          :: Text   --   Type of the event (e.g., "payment_success", "invalid")
  , transaction_id :: Text   --   Unique identifier for the transaction (used for idempotency)
  , amount         :: Double --   Amount involved in the transaction (must be > 0)
  , currency       :: Text   --   ISO 4217 currency code (e.g., "BRL", "USD")
  , timestamp      :: Text   --   Timestamp in ISO 8601 format (e.g., "2023-10-01T12:34:56Z")
  } deriving (Show, Generic)



-- | Custom JSON parsing for 'Transaction', with validation and amount conversion.
-- Ensures all required fields are present and non-empty.
instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \v -> do
    -- Check presence of all required keys
    let requiredKeys = ["event", "transaction_id", "amount", "currency", "timestamp"]
    let hasAllKeys = all (`KM.member` v) requiredKeys

    if not hasAllKeys
      then fail "Campos obrigatórios ausentes"
      else do
        -- Extract individual fields
        event          <- v .: "event"
        transaction_id <- v .: "transaction_id"
        currency       <- v .: "currency"
        timestamp      <- v .: "timestamp"
        amountRaw      <- v .: "amount"

        -- Validate and parse 'amount'
        case parseAmount amountRaw of
          Left err     -> fail err
          Right amount -> 
            -- Ensure that no required text field is empty
            if any T.null [event, transaction_id, currency, timestamp]
              then fail "Campos obrigatórios vazios"
              else return Transaction {..}

instance ToJSON Transaction


-- | Parses the 'amount' field, accepting either a JSON number or a string.
-- Ensures the value can be converted into a valid Double.
--
-- Examples of accepted formats:
--   { "amount": 49.90 }
--   { "amount": "49.90" }
--
-- Returns:
--   - Right d if parsing succeeds
--   - Left error message otherwise
parseAmount :: Value -> Either String Double
parseAmount (Number n) = Right $ S.toRealFloat n  -- Handles raw JSON number (e.g., 49.90)

parseAmount (String t) =
  case reads (T.unpack t) of
    [(d, "")] -> Right d                          -- Handles stringified number (e.g., "49.90")
    _         -> Left "Invalid string for amount" -- Malformed string (e.g., "abc")

parseAmount _ = Left "Invalid type for amount"    -- Not a string or number