--- This module defines the execution context for the backend.
module Env where

import IO
import IOExts

import Database.CDBI.Connection

import Response
import arf

--- Represents the execution environment.
data Env = Env {
  response   :: IORef Response,
  connection :: Connection
}

--- Initialize the environment.
init :: IO Env
init = do
  conn <- connectSQLite sqliteDBFile
  ref  <- newIORef Response.init
  return Env { response = ref, connection = conn }

--- Accepts a message and writes it to the response.
write :: String -> Env -> IO ()
write msg env = modifyIORef (response env) (Response.write msg)

--- Accepts an error message and write it to the error response.
writeErr :: String -> Env -> IO ()
writeErr msg env = modifyIORef (response env) (Response.writeErr msg)

--- Accepts an environment and finalizes the backend operations - i.e. sends the
--- pending response, closes the database connection, and, if an error occured,
--- exits with the error message.
end :: Env -> IO ()
end env = do
  res <- readIORef $ response env
  Response.respond res
  disconnect $ connection env
  hFlush stdout
  when (Response.err res) $ error $ Response.content res

--- Accepts a message, returns the message, and finalizes backend operations.
reply :: String -> Env -> IO ()
reply msg env = write msg env >> end env

--- Accepts an error message, returns the error in the response, and finalizes
--- backend operations.
endWithError :: String -> Env -> IO ()
endWithError msg env = writeErr msg env >> end env
