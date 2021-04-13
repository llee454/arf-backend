{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

import System
import RegExp
import List
import Maybe
import IOExts

import Database.CDBI.ER
import Database.CDBI.Criteria
import Database.CDBI.Connection
import Database.CDBI.Description

import URL
import Response
import arf

main :: IO ()
main = do
  env <- Response.init
  route   env
  respond env

--- Registers the request handlers.
handlers :: [(String, [String] -> IORef Response -> IO ())]
handlers = [
    ("status", const (write "Available")),
    ("connect", connect)
  ]

--- Routes the incoming request to the appropriate handler.
route :: IORef Response -> IO ()
route ref = do
  parseRes <- System.getEnviron "QUERY_STRING" >>= return . URL.parseQuery
  case parseRes >>- lookup "q" >>- Just . splitOn "/" of
    Nothing -> writeErr "Error: Invalid Query String." ref
    Just [] -> writeErr "Error: Invalid request. No endpoint selected." ref
    Just (path : args) ->
      case lookup path handlers of
      Nothing      -> writeErr "Error: Invalid request. Unrecognized endpoint." ref
      Just handler -> handler args ref

--- Handles connect requests.
connect :: [String] -> IORef Response -> IO ()
connect _ ref = do
  write ("Connecting to the SQLite Database. " ++ sqliteDBFile ++ "\n") ref
  conn <- connectSQLite sqliteDBFile
  write "Connected to the SQLite Database.\n" ref
  disconnect conn
  write "Disconnected from the SQLite Database.\n" ref

