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
import Action
import Activity
import Attribute
import Entity
import Event
import qualified Measurement

import qualified Health

main :: IO ()
main = Env.init >>= route

--- Registers the request handlers.
handlers :: [(String, [String] -> Env -> IO ())]
handlers = [
    ("action",      Action.handler),
    ("activity",    Activity.handler),
    ("attribute",   Attribute.handler),
    ("entity",      Entity.handler),
    ("event",       Event.handler),
    ("health",      Health.handler), 
    ("init",        const init),
    ("measurement", Measurement.handler),
    ("status",      const (reply "Available")),
    ("version",     const (reply "1.0.0"))
  ]

--- Registers modules that perform database initializations.
initializers :: [IO (DBAction ())]
initializers = [
    Health.init
  ]

--- Initializes the database.
init :: Env -> IO ()
init env = do
  query <- foldIO
             (\acc initializer -> initializer >>= return . ((>+) acc))
             (returnDB $ Right ())
             initializers
  run (runInTransaction query)
    ("Error: An error occured while trying to initialize the database. " ++)
    (\_ -> Env.reply "success")
    env

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
