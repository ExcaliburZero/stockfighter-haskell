{-|
Module      : Network.Stockfighter.Trade
Description : Includes functions and data types for making trades.
Copyright   : (c) Christopher Wells, 2016
License     : MIT
Maintainer  : cwellsny@nycap.rr.com

This module contains several functions and data types for requesting trades in
Stockfighter.

= Examples
The following are some quick examples of how to use the module.

== Requesting orders
To request an order you must first define the Order. Then you can use the
`requestOrder` to make the order request and get the response message.

@
performOrder :: IO String
performOrder = response
  where response = requestOrder myOrder myAPIKey
        myOrder  = Order {
            account   = \"EXB123456\"
          , venue     = \"TESTEX\"
          , symbol    = \"FOOBAR\"
          , price     = 25000
          , quantity  = 100
          , direction = Buy
          , orderType = Limit
        }
        myAPIKey = \"n3vy87nviqufiunusdfnuwefakeapikey\"
@

>>> performOrder
"{\"ok\":false,\"error\":\"Auth/auth failed: %!(EXTRA string=Couldn't find that apiKey, service reports: No fighter with that API key.)\"}\n"
-}
{-# LANGUAGE DeriveGeneric #-}
module Network.Stockfighter.Trade
{-
(
  requestOrder,

  testOrder,

  Order(..), Account, Venue, Symbol, Price, Quantity, Direction(..),
  OrderType(..), APIKey
)
-}
where

import GHC.Generics

import Data.Aeson
import Data.Text
import Control.Monad (mzero)

import Network.Stockfighter.Util

-- | A representation of a stock order. An order can be requested using the
-- `requestOrder` function.
data Order = Order {
      account   :: Account
    , venue     :: Venue
    , symbol    :: Symbol
    , price     :: Price
    , quantity  :: Quantity
    , direction :: Direction
    , orderType :: OrderType
  } deriving (Eq, Show, Generic)

instance FromJSON Order where
  parseJSON (Object v) = Order
    <$> v .: pack "account"
    <*> v .: pack "venue"
    <*> v .: pack "symbol"
    <*> v .: pack "price"
    <*> v .: pack "quantity"
    <*> v .: pack (show "direction")
    <*> v .: pack (show "orderType")
  parseJSON _ = mzero

instance ToJSON Order where
  toJSON order =
    object [
          pack "account"   .= account order
        , pack "venue"     .= venue order
        , pack "symbol"    .= symbol order
        , pack "price"     .= price order
        , pack "quantity"  .= quantity order
        , pack "direction" .= show (direction order)
        , pack "orderType" .= show (orderType order)
      ]

{-
λ ~ import Data.Char
λ ~ import Data.ByteString.Lazy
λ ~ map (chr . fromIntegral) $ unpack $ encode testOrder

<interactive>:4:1:
    Ambiguous occurrence ‘map’
    It could refer to either ‘Prelude.map’,
                             imported from ‘Prelude’ at /home/chris/Code/stockfighter/app/Main.hs:1:8-11
                             (and originally defined in ‘GHC.Base’)
                          or ‘Data.ByteString.Lazy.map’,
                             imported from ‘Data.ByteString.Lazy’

<interactive>:4:37: Not in scope: ‘encode’
λ ~ import Data.Aeson
λ ~ Prelude.map (chr . fromIntegral) $ unpack $ encode testOrder
"{\"venue\":\"TESTEX\",\"direction\":\"buy\",\"quantity\":100,\"orderType\":\"limit\",\"symbol\":\"FOOBAR\",\"account\":\"EXB123456\",\"price\":25000}"
λ ~ putStrLn $ Prelude.map (chr . fromIntegral) $ unpack $ encode testOrder
-}

-- | An account with which an order can be made.
--
-- ex. \"EXB123456\"
type Account = String

-- | A venue which an order can be placed in.
--
-- ex. \"TESTEX\"
type Venue = String

-- | A symbol which represents a stock that can be traded.
--
-- ex. \"FOOBAR\"
type Symbol = String

-- | A price at which a stock can be traded. Units are in pennies USD. For
-- example, a value of 25000 would become $250.00.
--
-- ex. 25000
type Price = Int

-- | A quantity of stocks that can be used in a trade.
--
-- ex. 100
type Quantity = Int

-- | A direction of a trade indicating whether one is buying or selling stocks.
data Direction = Buy  -- ^ Stocks are being purchased
               | Sell -- ^ Stocks are being sold
  deriving (Eq, Generic)

-- | Converts the direction into the string format used by the API
instance Show Direction where
  show Buy  = "buy"
  show Sell = "sell"

instance FromJSON Direction
instance ToJSON Direction

-- | A type of an order. See the Stockfighter API entry on
-- <https://starfighter.readme.io/docs/place-new-order#order-types order types>
-- for more information.
data OrderType = Limit | Market | FillOrKill | ImmediateOrCancel
  deriving (Eq, Generic)

-- | Converts the order type into the string format used by the API
instance Show OrderType where
  show Limit             = "limit"
  show Market            = "market"
  show FillOrKill        = "fill-or-kill"
  show ImmediateOrCancel = "immeditate-or-cancel"

instance FromJSON OrderType
instance ToJSON OrderType

-- | The API key used to place an order.
--
-- ex. "uhdyf872f8ui23hdh98d23ydiuayf7q2yf82y383"
type APIKey = String

-- | Sends a request for the given order using the given APIKey, and returns
-- the response.
--
-- See the Stockfighter API entry on
-- <https://starfighter.readme.io/docs/place-new-order placing stock orders>
-- for more information.
--
-- >>> requestOrder testOrder "invalidAPIKey"
-- "{\"ok\":false,\"error\":\"Auth/auth failed: %!(EXTRA string=Couldn't find that apiKey, service reports: No fighter with that API key.)\"}\n"
requestOrder :: Order     -- ^ Order to request
             -> APIKey    -- ^ APIKey to use for the order request
             -> IO String -- ^ Response to the order request
requestOrder order apikey = response
  where response = sendRequest requestInfo
        requestInfo = createOrderRequest order baseUrl apikey
        baseUrl = "https://api.stockfighter.io/ob/api"

-- | Creates an order request from an order, API base url, and APIKey.
createOrderRequest :: Order -> BaseUrl -> APIKey -> Request
createOrderRequest order baseUrl apikey = (requestUrl, httpHeader, requestContents)
  where requestUrl = baseUrl ++ "/venues/" ++ venue order ++ "/stocks/" ++ symbol order ++ "/orders"
        httpHeader = [("X-Starfighter-Authorization", apikey)]
        requestContents = byteStringToString $ encode order

-- | Converts an order into the json contents of the order request.
orderToRequestContents :: Order -> RequestContents
orderToRequestContents order = requestContents
  where requestContents = "{\n" ++
          section "account" (account order) ++ ",\n" ++
          section "venue" (venue order) ++ ",\n" ++
          section "symbol" (symbol order) ++ ",\n" ++
          section "price" (price order) ++ ",\n" ++
          section "qty" (quantity order) ++ ",\n" ++
          section "direction" (show $ direction order) ++ ",\n" ++
          section "orderType" (show $ orderType order) ++ "\n" ++
          "}\n"
        section label value = "  \"" ++ label ++ "\": " ++ show value

-- | An example Order to test with.
testOrder :: Order
testOrder = Order {
      account   = "EXB123456"
    , venue     = "TESTEX"
    , symbol    = "FOOBAR"
    , price     = 25000
    , quantity  = 100
    , direction = Buy
    , orderType = Limit
  }
