{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}
--- Defines the insert, update, and delete operations for the Base database types.

module Base where

import Time

import Database.CDBI.ER
import Database.CDBI.Criteria
import Database.CDBI.Connection
import Database.CDBI.Description

import arf

data Entity = Entity { key :: Maybe Int, created :: ClockTime, name :: String }

--- Inserts an entity into the database.
--- @return the inserted entity with the key set.
insert :: Entity -> DBAction Entity
insert x@(Entity Nothing created name) =
  runInTransaction $
    arf.newEntry created >+=
    (\(arf.Entry (EntryID k) created) ->
      arf.newEntityWithEntryEntity_entryKey name (EntryID k) >+
      returnDB (Right (x {key = Just k})))

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
        _  -> error "Error: An error occured while trying to read an Entity from the SQLite database."

--- Accepts ane entity and updates the associated database tables.
update :: Entity -> DBAction ()
update (Entity (Just k) created name) =
  runInTransaction $
    execute "UPDATE Entity SET Timestamp = '?' WHERE Key = '?';" [SQLDate created, SQLInt k] >+
    execute "UPDATE Entry  SET Name = '?' WHERE EntryEntity_entryKey = '?';" [SQLString name, SQLInt k]

--- Accepts an Entity ID and deletes the associated entity.
delete :: Int -> DBAction ()
delete k =
  runInTransaction $
    execute "DELETE FROM Entity WHERE Key = '?';" [SQLInt k] >+
    execute "DELETE FROM Entry  WHERE EntryEntity_entryKey = '?';" [SQLInt k]
