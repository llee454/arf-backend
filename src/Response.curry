--- This module defines the Response data type and functions for creating and
--- modifying Response values. Response values are used to represent backend
--- responses to front-end requests.

module Response where

import IOExts

--- Represents server responses.
data Response = Response { err :: Bool, content :: String }

--- The initial response.
initResponse :: Response
initResponse = Response False ""

--- Returns the given string as a response.
write :: String -> IORef Response -> IO ()
write msg ref = modifyIORef ref (\res -> res {content = (content res) ++ msg})

--- Returns the given string as an error response.
writeErr :: String -> IORef Response -> IO ()
writeErr msg ref = modifyIORef ref (\res -> res { err = True, content = msg })

--- Sends the response.
respond :: IORef Response -> IO ()
respond ref = do
  Response err content <- readIORef ref
  putStrLn "Content-type: text/html"
  when err $ putStr "Status: 500 Internal Server Error"
  putStrLn "\n"
  putStrLn content

--- Initialize the environment.
init :: IO (IORef Response)
init = newIORef initResponse
