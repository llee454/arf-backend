--- This module defines the abstract interface for entities.

module EntityIntf where

import IO
import Float
import Time
import Maybe
import JSON.Data
import JSON.Parser
import JSON.Pretty

import Database.CDBI.Connection

import JSONExt
import Base
import Env

data EntityIntf a = EntityIntf {
  name_   :: String,
  key_    :: a -> Maybe Int,
  ofJSON_ :: JValue -> Maybe a,
  toJSON_ :: a -> JValue,
  insert_ :: a -> DBAction a,
  read_    :: Int -> DBAction (Maybe a),
  update_ :: a -> DBAction (),
  delete_ :: Int -> DBAction ()
}

--- Handles CRUD requests and passes unrecognized requests to given extension handler.
--- @param ext - an extension handler
--- @param args - URL arguments
--- @param env - an execution environment
--- @return a request handler
handler :: EntityIntf a -> ([String] -> String -> Env -> IO ()) -> [String] -> Env -> IO ()
handler intf ext args env = do
  req <- getContents
  let json = parseJSON req
    in case (args, json, json >>- ofJSON_ intf) of
      (["create"], _, Just x) ->
        run (runInTransaction $ insert_ intf x)
          (("Error: An error occured while trying to insert an " ++ name_ intf ++ " into the database. ") ++)
          (\x -> Env.reply $ ppJSON $ JObject [
            ("id", maybeIntToJSON (key_ intf x))])
          env
      (["read", k], _, _) ->
        run (runInTransaction $ read_ intf (Prelude.read k :: Int))
          (("Error: An error occured while trying to read a(n) " ++ name_ intf ++ " from the database. ") ++)
          (\x -> Env.reply $ ppJSON $
            maybeToJSON (\e -> JObject [(name_ intf, toJSON_ intf e)]) x)
          env
      (["update"], _, Just x) ->
        run (runInTransaction $ update_ intf x)
          (("Error: An error occured while trying to update an " ++ name_ intf ++ " into the database. ") ++)
          (const $ Env.end)
          env
      (["delete", k], _, _) ->
        run (runInTransaction $ delete_ intf (Prelude.read k :: Int))
          (("Error: An error occured while trying to delete an " ++ name_ intf ++ " from the database. ") ++)
          (const $ Env.end)
          env
      _ -> ext args req env

--- A handler which returns an "Invalid Request" error in response to every request.
defaultHandler :: [String] -> String -> Env -> IO ()
defaultHandler _ req = endWithError ("Error: Invalid request. JSON: " ++ (show $ parseJSON req))
