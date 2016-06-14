{-|
Module      : Network.Stockfighter.Util
Description : Includes functions for sending requests.
Copyright   : (c) Christopher Wells, 2016
License     : MIT
Maintainer  : cwellsny@nycap.rr.com
-}
module Network.Stockfighter.Util where

import Data.ByteString.Lazy (ByteString, unpack)
import Data.Char (chr)
import System.Directory (removeFile)
import System.IO (hGetContents)
import System.Process (CreateProcess(..), createProcess, shell, StdStream(CreatePipe), waitForProcess)

-- | Converts a lazy ByteString into a String.
byteStringToString :: ByteString -> String
byteStringToString bytestring = map (chr . fromIntegral) $ unpack bytestring

-- | An API key that can be used to make a request.
--
-- >>> APIKey "uhdyf872f8ui23hdh98d23ydiuayf7q2yf82y383"
-- APIKey {unAPIKey = "uhdyf872f8ui23hdh98d23ydiuayf7q2yf82y383"}
newtype APIKey = APIKey { unAPIKey :: String }
  deriving (Eq, Show)

-- | A base url of an API.
--
-- >>> BaseUrl "https://api.stockfighter.io/ob/api"
-- BaseUrl {unBaseUrl = "https://api.stockfighter.io/ob/api"}
newtype BaseUrl = BaseUrl { unBaseUrl :: String }
  deriving (Eq, Show)

-- | A url of an API request.
--
-- >>> RequestUrl "https://api.stockfighter.io/ob/api/venues/TESTEX/stocks/FOOBAR/order"
-- RequestUrl {unRequestUrl = "https://api.stockfighter.io/ob/api/venues/TESTEX/stocks/FOOBAR/order"}
newtype RequestUrl = RequestUrl { unRequestUrl :: String }
  deriving (Eq, Show)

-- | A header of a HTTP request.
--
-- >>> HTTPHeader [("X-Starfighter-Authorization", "apikey81231")]
-- HTTPHeader {unHTTPHeader = [("X-Starfighter-Authorization","apikey81231")]}
newtype HTTPHeader = HTTPHeader { unHTTPHeader :: [(String, String)] }
  deriving (Eq, Show)

-- | Contents of a HTTP request.
--
-- >>> RequestContents "{\"field\":\"key\"}"
-- RequestContents {unRequestContents = "{\"field\":\"key\"}"}
newtype RequestContents = RequestContents { unRequestContents :: String }
  deriving (Eq, Show)

-- | A HTTP request.
--
-- >>> let rUrl      = RequestUrl "url"
-- >>> let rHeader   = HTTPHeader [("key", "value")]
-- >>> let rContents = RequestContents "contents"
-- >>> Request {url = rUrl, header = rHeader, contents = rContents}
-- Request {url = RequestUrl {unRequestUrl = "url"}, header = HTTPHeader {unHTTPHeader = [("key","value")]}, contents = RequestContents {unRequestContents = "contents"}}
data Request = Request {
      url      :: RequestUrl
    , header   :: HTTPHeader
    , contents :: RequestContents
  }
  deriving (Eq, Show)

-- | Sends the given request using a curl command.
--
-- Stores the request body in a temporary file `tmp-stockfighter-request.json`
-- which is deleted once the curl command finishes running.
sendRequest :: Request -> IO String
sendRequest request = response
  where response = do writeFile orderFileName $ unRequestContents $ contents request
                      (_, Just stdout, _, handler) <- createProcess (shell curlCommand){ std_out = CreatePipe }
                      _ <- waitForProcess handler
                      removeFile orderFileName
                      hGetContents stdout
        curlCommand = curlCommand2
        curlCommand2 = unwords ["curl", "-sS", unRequestUrl $ url request, "-d", "@" ++ orderFileName,
                                "-H", "\"" ++ (headerToString $ header request) ++ "\""
                               ]
        orderFileName = "tmp-stockfighter-request.json"

-- | Converts an http header definition into its corresponding String
-- representation.
--
-- >>> headerToString $ HTTPHeader [("key1", "value1"), ("key2", "value2")]
-- "key1:value1;key2:value2;"
headerToString :: HTTPHeader -> String
headerToString (HTTPHeader []) = ""
headerToString (HTTPHeader ((key, value):xs)) = key ++ ":" ++ value ++ ";" ++ (headerToString $ HTTPHeader xs)

