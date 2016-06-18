module Spec where

import Data.Aeson (encode)
import Data.String (fromString)
import Test.Hspec

import Network.Stockfighter.Trade
import Network.Stockfighter.Util

spec :: Spec
spec = do
  -- Trade
  describe "Order" $ do
    it "converts into a JSON String" $ do
      encode testOrder `shouldBe` fromString testOrderJSON

  describe "Direction" $ do
    it "converts to correct String representation for API request" $ do
      show Buy  `shouldBe` "buy"
      show Sell `shouldBe` "sell"

  describe "OrderType" $ do
    it "converts to correct String representation for API request" $ do
      show Limit             `shouldBe` "limit"
      show Market            `shouldBe` "market"
      show FillOrKill        `shouldBe` "fill-or-kill"
      show ImmediateOrCancel `shouldBe` "immediate-or-cancel"

  describe "createOrderRequest" $ do
    it "creates a request from an order, baseurl, and apikey" $ do
      createOrderRequest testOrder testBaseUrl testAPIKey `shouldBe` testOrderRequest

  -- Util
  describe "requestToCurlCommand" $ do
    it "creates a curl command from a request and tmp file" $ do
      requestToCurlCommand testOrderRequest testTempFile `shouldBe` testCurlCommand

  describe "headerToString" $ do
    it "converts a single part header" $ do
      headerToString (HTTPHeader [("key", "value")]) `shouldBe` "key:value;"

    it "converts a multi part header" $ do
      headerToString (HTTPHeader [("key1", "value1"), ("key2","value2")])
        `shouldBe` "key1:value1;key2:value2;"


testOrder :: Order
testOrder = Order {
      account   = Account "EXB123456"
    , venue     = Venue "TESTEX"
    , symbol    = Symbol "FOOBAR"
    , price     = Price 25000
    , quantity  = Quantity 100
    , direction = Buy
    , orderType = Limit
  }

testOrderJSON :: String
testOrderJSON = "{\"account\":\"EXB123456\",\"venue\":\"TESTEX\",\"symbol\":\"FOOBAR\",\"price\":25000,\"qty\":100,\"direction\":\"buy\",\"orderType\":\"limit\"}"

testBaseUrl :: BaseUrl
testBaseUrl = BaseUrl "https://stockfighter.io"

testAPIKey :: APIKey
testAPIKey = APIKey "uhdyf872f8ui23hdh98d23ydiuayf7q2yf82y383"

testOrderRequest :: Request
testOrderRequest = Request {
      url = RequestUrl "https://stockfighter.io/venues/TESTEX/stocks/FOOBAR/orders"
    , header = HTTPHeader [("X-Starfighter-Authorization", "uhdyf872f8ui23hdh98d23ydiuayf7q2yf82y383")]
    , contents = RequestContents "{\"account\":\"EXB123456\",\"venue\":\"TESTEX\",\"symbol\":\"FOOBAR\",\"price\":25000,\"qty\":100,\"direction\":\"buy\",\"orderType\":\"limit\"}"
  }

testTempFile :: String
testTempFile = "tmp-stockfighter.json"

testCurlCommand :: CurlCommand
testCurlCommand = CurlCommand "curl -sS https://stockfighter.io/venues/TESTEX/stocks/FOOBAR/orders -d @tmp-stockfighter.json -H \"X-Starfighter-Authorization:uhdyf872f8ui23hdh98d23ydiuayf7q2yf82y383;\""
