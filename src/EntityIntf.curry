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

--- 
handler :: EntityIntf a -> [String] -> Env -> IO ()
handler intf args env = do
  req <- getContents
  let json = parseJSON req
    in case (args, json, json >>- ofJSON_ intf) of
      (["create"], _, Just x) ->
        run (insert_ intf x)
          (("Error: An error occured while trying to insert an " ++ name_ intf ++ " into the database. ") ++)
          (\x -> Env.reply $ ppJSON $ JObject [
            ("id", maybeIntToJSON (key_ intf x))])
          env
      (["read", k], _, _) ->
        run (read_ intf (Prelude.read k :: Int))
          (("Error: An error occured while trying to read an " ++ name_ intf ++ " into the database. ") ++)
          (\x -> Env.reply $ ppJSON $
            maybeToJSON (\e -> JObject [(name_ intf, toJSON_ intf e)]) x)
          env
      (["update"], _, Just x) ->
        run (update_ intf x)
          (("Error: An error occured while trying to update an " ++ name_ intf ++ " into the database. ") ++)
          (const $ Env.end)
          env
      (["delete", k], _, _) ->
        run (delete_ intf (Prelude.read k :: Int))
          (("Error: An error occured while trying to delete an " ++ name_ intf ++ " from the database. ") ++)
          (const $ Env.end)
          env
      _ -> endWithError ("Error: Invalid request. JSON: " ++ show json) env
