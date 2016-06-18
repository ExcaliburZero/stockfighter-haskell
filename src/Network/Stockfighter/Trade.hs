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
            account   = Account \"EXB123456\"
          , venue     = Venue \"TESTEX\"
          , symbol    = Symbol \"FOOBAR\"
          , price     = Price 25000
          , quantity  = Quantity 100
          , direction = Buy
          , orderType = Limit
        }
        myAPIKey = APIKey \"n3vy87nviqufiunusdfnuwefakeapikey\"
@
-}
module Network.Stockfighter.Trade
(
  requestOrder,

  Order(..), Account(..), Venue(..), Symbol(..), Price(..), Quantity(..),
  Direction(..), OrderType(..),

  createOrderRequest
)
where

import Control.Monad ()
import Data.Aeson (encode, object, pairs, toEncoding, ToJSON, toJSON, (.=))
import Data.Semigroup ((<>))
import Data.Text (pack)

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
  } deriving (Eq, Show)

-- | Handles converting of Orders into json format.
instance ToJSON Order where
  toJSON order =
    object [
          pack "account"   .= (unAccount $ account order)
        , pack "venue"     .= (unVenue $ venue order)
        , pack "symbol"    .= (unSymbol $ symbol order)
        , pack "price"     .= (unPrice $ price order)
        , pack "qty"       .= (unQuantity $ quantity order)
        , pack "direction" .= show (direction order)
        , pack "orderType" .= show (orderType order)
      ]

  toEncoding order =
    pairs (
           pack "account"   .= (unAccount $ account order)
        <> pack "venue"     .= (unVenue $ venue order)
        <> pack "symbol"    .= (unSymbol $ symbol order)
        <> pack "price"     .= (unPrice $ price order)
        <> pack "qty"       .= (unQuantity $ quantity order)
        <> pack "direction" .= show (direction order)
        <> pack "orderType" .= show (orderType order)
      )

-- | An account with which an order can be made.
--
-- >>> Account "EXB123456"
-- Account {unAccount = "EXB123456"}
newtype Account = Account { unAccount :: String }
  deriving (Eq, Show)

-- | A venue which an order can be placed in.
--
-- >>> Venue "TESTEX"
-- Venue {unVenue = "TESTEX"}
newtype Venue = Venue { unVenue :: String }
  deriving (Eq, Show)

-- | A symbol which represents a stock that can be traded.
--
-- >>> Symbol "FOOBAR"
-- Symbol {unSymbol = "FOOBAR"}
newtype Symbol = Symbol { unSymbol :: String }
  deriving (Eq, Show)

-- | A price at which a stock can be traded. Units are in pennies USD. For
-- example, a value of 25000 would become $250.00.
--
-- >>> Price 25000
-- Price {unPrice = 25000}
newtype Price = Price { unPrice :: Int }
  deriving (Eq, Show)

-- | A quantity of stocks that can be used in a trade.
--
-- >>> Quantity 100
-- Quantity {unQuantity = 100}
newtype Quantity = Quantity { unQuantity :: Int }
  deriving (Eq, Show)

-- | A direction of a trade indicating whether one is buying or selling stocks.
data Direction = Buy  -- ^ Stocks are being purchased
               | Sell -- ^ Stocks are being sold
  deriving (Eq)

-- | Converts the direction into the string format used by the API
instance Show Direction where
  show Buy  = "buy"
  show Sell = "sell"

-- | A type of an order. See the Stockfighter API entry on
-- <https://starfighter.readme.io/docs/place-new-order#order-types order types>
-- for more information.
data OrderType = Limit | Market | FillOrKill | ImmediateOrCancel
  deriving (Eq)

-- | Converts the order type into the string format used by the API
instance Show OrderType where
  show Limit             = "limit"
  show Market            = "market"
  show FillOrKill        = "fill-or-kill"
  show ImmediateOrCancel = "immediate-or-cancel"

-- | Sends a request for the given order using the given APIKey, and returns
-- the response.
--
-- See the Stockfighter API entry on
-- <https://starfighter.readme.io/docs/place-new-order placing stock orders>
-- for more information.
--
-- >>> requestOrder testOrder (APIKey "invalidAPIKey")
-- "{\"ok\":false,\"error\":\"Auth/auth failed: %!(EXTRA string=Couldn't find that apiKey, service reports: No fighter with that API key.)\"}\n"
requestOrder :: Order     -- ^ Order to request
             -> APIKey    -- ^ APIKey to use for the order request
             -> IO String -- ^ Response to the order request
requestOrder order apikey = response
  where response = sendRequest requestInfo
        requestInfo = createOrderRequest order baseUrl apikey
        baseUrl = BaseUrl "https://api.stockfighter.io/ob/api"

-- | Creates an order request from an order, API base url, and APIKey.
createOrderRequest :: Order -> BaseUrl -> APIKey -> Request
createOrderRequest order baseUrl apikey = request
  where request = Request {
              url      = requestUrl
            , header   = httpHeader
            , contents = requestContents
          }
        requestUrl = RequestUrl $ unBaseUrl baseUrl ++ urlEnding
        urlEnding  = "/venues/" ++ (unVenue $ venue order) ++ "/stocks/" ++ (unSymbol $ symbol order) ++ "/orders"
        httpHeader = HTTPHeader [("X-Starfighter-Authorization", unAPIKey apikey)]
        requestContents = RequestContents $ byteStringToString $ encode order
