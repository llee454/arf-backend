--- This module defines the Response data type and functions for creating and
--- modifying Response values. Response values are used to represent backend
--- responses to front-end requests.

module Response where

import IOExts

--- Represents server responses.
data Response = Response { err :: Bool, content :: String }

--- The initial response.
init :: Response
init = Response False ""

--- Returns the given string as a response.
write :: String -> Response -> Response
write msg (Response err content) = Response err (content ++ msg)

--- Returns the given string as an error response.
--- Note: if an error has already been queued, the earlier failure is returned.
writeErr :: String -> Response -> Response
writeErr msg res@(Response err _)
  | err       = res
  | otherwise = Response True msg

--- Sends the response.
respond :: Response -> IO ()
respond (Response err content) = do
  putStrLn "Access-Control-Allow-Origin: *"
  putStrLn "Content-type: text/plain"
  when err $ putStr "Status: 500 Internal Server Error"
  putStrLn "\n"
  putStrLn content
