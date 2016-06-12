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

type BaseUrl = String
type RequestUrl = String
type HTTPHeader = [(String, String)]
type RequestContents = String
type Request = (RequestUrl, HTTPHeader, RequestContents)

-- | Sends the given request using a curl command.
--
-- Stores the request body in a temporary file `tmp-stockfighter-request.json`
-- which is deleted once the curl command finishes running.
sendRequest :: Request -> IO String
sendRequest (url, header, contents) = response
  where response = do writeFile orderFileName contents
                      (_, Just stdout, _, handler) <- createProcess (shell curlCommand){ std_out = CreatePipe }
                      _ <- waitForProcess handler
                      removeFile orderFileName
                      hGetContents stdout
        curlCommand = curlCommand2
        curlCommand2 = unwords ["curl", "-sS", url, "-d", "@" ++ orderFileName,
                                "-H", "\"" ++ headerToString header ++ "\""
                               ]
        orderFileName = "tmp-stockfighter-request.json"

-- | Converts an http header definition into its corresponding String
-- representation.
headerToString :: HTTPHeader -> String
headerToString [] = ""
headerToString ((key, value):xs) = key ++ ":" ++ value ++ ";" ++ headerToString xs

