--- Defines the insert, update, and delete operations for the Entity database types.

module Entity where

import IO
import Float
import Time
import Maybe
import JSON.Data
import JSON.Parser

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
data Entity = Entity { key :: Maybe Int, created :: ClockTime, name :: String }

---
ofJSON :: JValue -> Maybe Entity
ofJSON json =
  case json of
    (JObject [("key", k), ("created", JNumber created), ("name", JString name)]) ->
      maybeIntOfJSON k >>- \x -> Just $ Entity x (clockTimeOfNum created) name
    _ -> Nothing

---
toJSON :: Entity -> JValue
toJSON (Entity k created name) =
  JObject [
   ("key",     maybeIntToJSON k),
   ("created", clockTimeToJSON created),
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
    [SQLInt k] [SQLTypeDate, SQLTypeString] >+= from
  where
    from :: [[SQLValue]] -> DBAction (Maybe Entity)
    from res =
      case res of
        [[SQLDate created, SQLString name]] -> returnDB $ Right $ Just $ Entity (Just k) created name
        [] -> returnDB $ Right Nothing
        _  -> failDB $ DBError UnknownError "Error: An error occured while trying to read an Entity from the SQLite database."

--- Accepts an entity and updates the associated database tables.
update :: Entity -> DBAction ()
update (Entity (Just k) created name) =
  execute "UPDATE Entity SET Timestamp = '?' WHERE Key = '?';" [SQLDate created, SQLInt k] >+
  execute "UPDATE Entry  SET Name = '?' WHERE EntryEntity_entryKey = '?';" [SQLString name, SQLInt k]

--- Accepts an Entity ID and deletes the associated entity.
delete :: Int -> DBAction ()
delete k =
  execute "DELETE FROM Entity WHERE Key = '?';" [SQLInt k] >+
  execute "DELETE FROM Entry  WHERE EntryEntity_entryKey = '?';" [SQLInt k]

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

--- 
handler :: [String] -> Env -> IO ()
handler = EntityIntf.handler entityIntf
