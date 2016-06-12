module Network.Stockfighter.Util where

type BaseUrl = String
type RequestUrl = String
type HTTPHeader = [(String, String)]
type RequestContents = String
type Request = (RequestUrl, HTTPHeader, RequestContents)

-- | Sends the given request.
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
        orderFileName = "tmp-Order.json"

-- | Converts an http header definition into its corresponding String
-- representation.
headerToString :: HTTPHeader -> String
headerToString [] = ""
headerToString ((key, value):xs) = key ++ ":" ++ value ++ ";" ++ headerToString xs

