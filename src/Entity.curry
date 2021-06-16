--- Defines the insert, update, and delete operations for the Entity database types.

module Entity where

import IO
import Float

import LocalTime

import Maybe
import JSON.Data
import JSON.Parser
import JSON.Pretty

import Database.CDBI.ER
import Database.CDBI.Criteria
import Database.CDBI.Connection
import Database.CDBI.Description

import EntityIntf
import JSONExt
import Base
import Env
import arf

--- Represents events
--- @cons key - the entry key
--- @cons created - the date on which the entry was created
--- @cons name - the name of the entity
data Entity = Entity { key :: Maybe Int, created :: Int, name :: String }

---
ofJSON :: JValue -> Maybe Entity
ofJSON json =
  case json of
    (JObject [("key", k), ("created", JNumber created), ("name", JString name)]) ->
      maybeIntOfJSON k >>- \x -> Just $ Entity x (truncate created) name
    _ -> Nothing

---
toJSON :: Entity -> JValue
toJSON (Entity k created name) =
  JObject [
   ("key",     maybeIntToJSON k),
   ("created", JNumber $ i2f $ created),
   ("name",    JString name)]

--- Inserts an entity into the database.
--- @return the inserted entity with the key set.
insert :: Entity -> DBAction Entity
insert x@(Entity k created name)
  | isJust k = failDB $ DBError UnknownError "Error: An error occured while trying to insert an entity into the database."
  | otherwise =
    arf.newEntry created >+=
    (\(arf.Entry (EntryID j) created) ->
      arf.newEntityWithEntryEntity_entryKey name (EntryID j) >+
      returnDB (Right (x {key = Just j})))

--- Accepts an Entity ID and reads the associated Entity.
read :: Int -> DBAction (Maybe Entity)
read k =
  select 
    "SELECT Entry.Timestamp, Entity.Name FROM Entry INNER JOIN Entity On Entity.EntryEntity_entryKey = Entry.Key WHERE Entry.Key = '?';"
    [SQLInt k] [SQLTypeInt, SQLTypeString] >+= from
  where
    from :: [[SQLValue]] -> DBAction (Maybe Entity)
    from res =
      case res of
        [[SQLInt created, SQLString name]] -> returnDB $ Right $ Just $ Entity (Just k) created name
        [] -> returnDB $ Right Nothing
        _  -> failDB $ DBError UnknownError "Error: An error occured while trying to read an Entity from the SQLite database."

--- Accepts an entity and updates the associated database tables.
update :: Entity -> DBAction ()
update (Entity (Just k) created name) =
  execute "UPDATE Entry  SET Timestamp = '?' WHERE Key = '?';" [SQLInt created, SQLInt k] >+
  execute "UPDATE Entity SET Name = '?' WHERE EntryEntity_entryKey = '?';" [SQLString name, SQLInt k]

--- Accepts an Entity ID and deletes the associated entity.
delete :: Int -> DBAction ()
delete k =
  execute "DELETE FROM Entry  WHERE Key = '?';" [SQLInt k] >+
  execute "DELETE FROM Entity WHERE EntryEntity_entryKey = '?';" [SQLInt k]

entityIntf :: EntityIntf Entity
entityIntf = EntityIntf {
  name_   = "entity",
  key_    = key,
  ofJSON_ = ofJSON,
  toJSON_ = toJSON,
  insert_ = insert,
  read_   = read,
  update_ = update,
  delete_ = delete
}

--- Returns the entity that has the given name.
getByName :: String -> DBAction (Maybe Entity)
getByName name =
  select
    ("SELECT " ++
     "  Entry.Key, " ++
     "  Entry.Timestamp " ++
     "FROM Entry " ++
     "INNER JOIN Entity ON Entity.EntryEntity_entryKey = Entry.Key " ++
     "WHERE Entity.Name = '?';")
    [SQLString name] [SQLTypeInt, SQLTypeInt] >+= from
  where
    from :: [[SQLValue]] -> DBAction (Maybe Entity)
    from res =
      case res of
        [[SQLInt k, SQLInt created]] -> returnDB $ Right $ Just $ Entity (Just k) created name
        [] -> returnDB $ Right Nothing
        _  -> failDB $ DBError UnknownError "Error: An error occured while trying to read an Entity from the SQLite database."

--- Handles non-CRUD requests.
--- @param args - the URL arguments
--- @param env  - the execution environment
--- @return handles the given requests
extHandler :: [String] -> String -> Env -> IO ()
extHandler args req =
  case args of
    ["get-by-name", name] ->
      run (runInTransaction $ getByName name)
        ("Error: An error occured while trying to retrieve an entity by name. " ++)
        (\x -> Env.reply $ ppJSON $
          maybeToJSON (\e -> JObject [("entity", toJSON e)]) x)
    _ -> EntityIntf.defaultHandler args req

--- 
handler :: [String] -> Env -> IO ()
handler = EntityIntf.handler entityIntf $ extHandler
