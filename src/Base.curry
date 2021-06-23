--- Defines the insert, update, and delete operations for the Base database types.

module Base where

import IO
import System
import IOExts
import Float
import Time
import Env
import JSON.Data

import Database.CDBI.ER
import Database.CDBI.Criteria
import Database.CDBI.Connection
import Database.CDBI.Description

import arf

import LocalTime

--- Runs the given database action. If an error occurs, ends and returns the
--- given error message; otherwise, passes the result to the given function.
run :: DBAction a -> (String -> String) -> (a -> Env -> IO ()) -> Env -> IO ()
run action err cont env = do
  res <- runDBAction action (Env.connection env)
  case res of
    Left (DBError _ emsg) -> endWithError (err emsg) env
    Right x -> cont x env

--- Accepts an error message and a database action that returns a maybe
--- value; executes the action; and returns an error if the value is
--- Nothing. Otherwise, returns the value.
fromJustDB :: String -> DBAction (Maybe a) -> DBAction a
fromJustDB emsg action =
  action >+= \res ->
  case res of
    Just x  -> returnDB $ Right x
    Nothing -> failDB $ DBError UnknownError emsg
