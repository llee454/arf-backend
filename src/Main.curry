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

import Env
import URL
import arf
import Base


main :: IO ()
main = Env.init >>= route

--- Registers the request handlers.
handlers :: [(String, [String] -> Env -> IO ())]
handlers = [
    ("status",  const (reply "Available")),
    ("version", const (reply "1.0.0")),
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
      Nothing      -> endWithError "Error: Invalid request. Unrecognized endpoint." env
      Just handler -> handler args env

--- Runs the given database action. If an error occurs, ends and returns the
--- given error message; otherwise, passes the result to the given function.
run :: DBAction a -> (String -> String) -> (a -> Env -> IO ()) -> Env -> IO ()
run action err cont env = do
  res <- runDBAction action (Env.connection env)
  case res of
    Left (DBError _ emsg) -> endWithError (err emsg) env
    Right x -> cont x env

--- Handles demo requests.
demo :: [String] -> Env -> IO ()
demo _ env = do
  currTime <- getClockTime
  run (Base.insert $ Base.Entity Nothing currTime "example")
    ("Error: An error occured while trying to insert the demo data: " ++)
    (\(Base.Entity (Just k) _ _) ->
      run (Base.read k)
        ("Error: An error occured while trying to read the demo data: " ++)
        (\entity env -> do
          write ("Inserted entity with key: " ++ show k ++ "\n") env
          _ <- runDBAction (Base.delete k) (Env.connection env)
          write "Disconnected from the SQLite Database.\n" env
          end env))
    env
