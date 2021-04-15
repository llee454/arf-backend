--- Defines the insert, update, and delete operations for the Base database types.

module Base where

import Float
import Time
import Env

import Database.CDBI.ER
import Database.CDBI.Criteria
import Database.CDBI.Connection
import Database.CDBI.Description

import arf

--- Runs the given database action. If an error occurs, ends and returns the
--- given error message; otherwise, passes the result to the given function.
run :: DBAction a -> (String -> String) -> (a -> Env -> IO ()) -> Env -> IO ()
run action err cont env = do
  res <- runDBAction action (Env.connection env)
  case res of
    Left (DBError _ emsg) -> endWithError (err emsg) env
    Right x -> cont x env

--- Accepts a string that represents a unix timestamp and attempts to convert
--- this into a clock timAccepts a string that represents a unix timestamp and
--- attempts to convert this into a clock time.
--- Note: this fails 
clockTimeOfString :: String -> ClockTime
clockTimeOfString s = read $ "(CTime " ++ s ++ ")"

---
clockTimeOfNum :: Float -> ClockTime
clockTimeOfNum = clockTimeOfString . show . truncate

---
clockTimeToNum :: ClockTime -> Float
clockTimeToNum = i2f . clockTimeToInt
