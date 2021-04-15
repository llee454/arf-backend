{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

import System
import RegExp
import List
import Maybe
import IOExts
import Time

import Database.CDBI.ER
import Database.CDBI.Criteria
import Database.CDBI.Connection
import Database.CDBI.Description

import Base
import Env
import URL
import arf
import Entity
import Event

main :: IO ()
main = Env.init >>= route

--- Registers the request handlers.
handlers :: [(String, [String] -> Env -> IO ())]
handlers = [
    ("status",  const (reply "Available")),
    ("version", const (reply "1.0.0")),
    ("event",   Event.handler),
    ("demo",    demo)
  ]

--- Routes the incoming request to the appropriate handler.
route :: Env -> IO ()
route env = do
  parseRes <- System.getEnviron "QUERY_STRING" >>= return . URL.parseQuery
  case parseRes >>- lookup "q" >>- Just . splitOn "/" of
    Nothing -> endWithError "Error: Invalid Query String." env
    Just [] -> endWithError "Error: Invalid request. No endpoint selected." env
    Just (path : args) ->
      case lookup path handlers of
      Nothing      -> endWithError ("Error: Invalid request. Unrecognized endpoint. Path: " ++ show path ++ " Args: " ++ show args) env
      Just handler -> handler args env

--- Handles demo requests.
demo :: [String] -> Env -> IO ()
demo _ env = do
  currTime <- getClockTime
  run (Entity.insert $ Entity.Entity Nothing currTime "example")
    ("Error: An error occured while trying to insert the demo data: " ++)
    (\(Entity.Entity (Just k) _ _) ->
      run (Entity.read k)
        ("Error: An error occured while trying to read the demo data: " ++)
        (\_ env -> do
          write ("Inserted entity with key: " ++ show k ++ "\n") env
          _ <- runDBAction (Entity.delete k) (Env.connection env)
          write "Disconnected from the SQLite Database.\n" env
          end env))
    env
