module Network.Stockfighter.Info where

import Network.Stockfighter.Trade (Price, Quantity, Symbol, Venue)

data OrderBook = OrderBook {
      okay   :: Bool
    , venue  :: Venue
    , symbol :: Symbol
    , bids   :: [Offer]
    , asks   :: [Offer]
    , ts     :: TimeStamp
  }

data Offer = Offer {
      price :: Price
    , qty   :: Quantity
    , isBuy :: Bool     -- ^ If the offer is a bid or not
  }

-- | A timestamp of a response.
--
-- ex. \"2015-12-04T09:02:16.680986205Z\"
type TimeStamp = String
