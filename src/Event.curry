--- Defines the insert, update, and delete operations for the Event database types.

module Event where

import IO
import Float
import Time
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
--- @cons timestamp - the date on which the event occured
data Event = Event { key :: Maybe Int, created :: ClockTime, timestamp :: ClockTime }

---
ofJSON :: JValue -> Maybe Event
ofJSON json =
  case json of
    (JObject [("key", k), ("created", JNumber created), ("timestamp", JNumber timestamp)]) ->
      maybeIntOfJSON k >>- \x -> Just $ Event x (clockTimeOfNum created) (clockTimeOfNum timestamp)
    _ -> Nothing

---
toJSON :: Event -> JValue
toJSON (Event k created timestamp) =
  JObject [
    ("key",       maybeIntToJSON k),
    ("created",   clockTimeToJSON created),
    ("timestamp", clockTimeToJSON timestamp)]

--- Inserts an event into the database.
--- @return the inserted event with the key set.
insert :: Event -> DBAction Event
insert x@(Event k created timestamp)
  | isJust k = failDB $ DBError UnknownError "Error: An error occured while trying to insert an event into the database. Cannot insert an instantiated event."
  | otherwise =
    runInTransaction $
      arf.newEntry created >+=
      (\(arf.Entry (EntryID j) _) ->
       arf.newEventWithEntryEvent_entryKey timestamp (EntryID j) >+
       returnDB (Right (x {key = Just j})))

--- Accepts an Event ID and reads the associated event.
read :: Int -> DBAction (Maybe Event)
read k =
  select
    "SELECT Entry.Timestamp, Event.Timestamp FROM Entry INNER JOIN Event On Event.EntryEvent_entryKey = Entry.Key WHERE Entry.Key = '?';"
    [SQLInt k] [SQLTypeDate, SQLTypeDate] >+= from
  where
    from :: [[SQLValue]] -> DBAction (Maybe Event)
    from res =
      case res of
        [[SQLDate created, SQLDate timestamp]] -> returnDB $ Right $ Just $ Event (Just k) created timestamp
        [] -> returnDB $ Right Nothing
        _  -> failDB $ DBError UnknownError "Error: An error occured while trying to read an event from the SQLite database."

--- Accepts an event and updates the associated database tables.
update :: Event -> DBAction ()
update x =
  case x of
    Event (Just k) created timestamp ->
      runInTransaction $
        execute "UPDATE Entity SET Timestamp = '?' WHERE Key = '?';" [SQLDate created, SQLInt k] >+
        execute "UPDATE Event  SET Name = '?' WHERE EntryEvent_entryKey = '?';" [SQLDate timestamp, SQLInt k]
    _ -> failDB $ DBError UnknownError "Error: An error occured while trying to update an event."

--- Accepts an Event ID and deletes the associated event.
delete :: Int -> DBAction ()
delete k =
  runInTransaction $
    execute "DELETE FROM Entity WHERE Key = '?';" [SQLInt k] >+
    execute "DELETE FROM Event  WHERE EntryEvent_entryKey = '?';" [SQLInt k]

entityIntf :: EntityIntf Event
entityIntf = EntityIntf {
  name_   = "event",
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
