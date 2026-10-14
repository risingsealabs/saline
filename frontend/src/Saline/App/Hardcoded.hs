module Saline.App.Hardcoded where

import Saline.Intents.Condition (Amount, Token(..))

-- * Temporary demo values, exchange rates, and simulated functions.
-- Replace these values with service queries when implementing live integrations.

-- | Utils to get a dollar-equivalent amount from a token

-- TODO: Hardcoded tokens -> USD conversion rate for now
-- will be replaced when QTI / oracle / whatever is available on the chain
toUSD :: Token -> Amount -> Amount
toUSD t coin = tickerToUSD t * coin
--toUSD t coin = tickerToUSD t * (coin / (10 ** fromIntegral (tickerUnits t))) --TODO: treat Amount as satoshi,etc

tickerToUSD :: Fractional a => Token -> a
tickerToUSD = \case
  BTC  -> 62687.13
  ETH  -> 2428.51
  SALT -> 1089.33
  USDC -> 1
  USDT -> 1

-- TODO: replace the placeholder fraction with a defined conversion rule
tickerUnits :: Token -> Word
tickerUnits = \case
  BTC  -> 8
  ETH  -> 9 -- rounding to nearest GWEI
  USDC -> 0
  USDT -> 0
  SALT -> 3
