module Spec where

import Data.Aeson (encode)
import Test.Hspec

import Network.Stockfighter.Trade
import Network.Stockfighter.Util

spec :: Spec
spec = do
  -- Trade
  describe "Order" $ do
    it "converts into a JSON String" $ do
      let order = Order {
          account   = Account "EXB123456"
        , venue     = Venue "TESTEX"
        , symbol    = Symbol "FOOBAR"
        , price     = Price 25000
        , quantity  = Quantity 100
        , direction = Buy
        , orderType = Limit
      }
      byteStringToString (encode order) `shouldBe` "{\"account\":\"EXB123456\",\"venue\":\"TESTEX\",\"symbol\":\"FOOBAR\",\"price\":25000,\"qty\":100,\"direction\":\"buy\",\"orderType\":\"limit\"}"

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

  -- Util
  describe "headerToString" $ do
    it "converts a single part header" $ do
      headerToString (HTTPHeader [("key", "value")]) `shouldBe` "key:value;"

    it "converts a multi part header" $ do
      headerToString (HTTPHeader [("key1", "value1"), ("key2","value2")])
        `shouldBe` "key1:value1;key2:value2;"
