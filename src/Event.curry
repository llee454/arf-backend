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
      case k of
        JNumber x -> Just $ Event (Just $ truncate x) (clockTimeOfNum created) (clockTimeOfNum timestamp)
        JNull     -> Just $ Event Nothing (clockTimeOfNum created) (clockTimeOfNum timestamp)
        _         -> Nothing
    _ -> Nothing

---
toJSON :: Event -> JValue
toJSON (Event k created timestamp) =
  JObject [
    ("key", case k of Nothing -> JNull; Just j -> JNumber (i2f j)),
    ("created", JNumber $ clockTimeToNum created),
    ("timestamp", JNumber $ clockTimeToNum timestamp)]

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

--- 
handler :: [String] -> Env -> IO ()
handler args env = do
  req <- getContents
  let json = parseJSON req
    in case (args, json, json >>- ofJSON) of
      (["create"], _, Just event) ->
        run (insert event)
          ("Error: An error occured while trying to insert an event into the database. " ++)
          (\event -> Env.reply $ ppJSON $ JObject [
            ("id", case (key event) of
                     Nothing -> JNull;
                     Just k -> JNumber (i2f k))])
          env
      (["read", k], _, _) ->
        run (read (Prelude.read k :: Int))
          ("Error: An error occured while trying to read an event into the database. " ++)
          (\event -> Env.reply $ ppJSON $
            case event of
              Nothing -> JNull;
              Just e -> JObject [("event", toJSON e)])
          env
      (["update"], _, Just event) ->
        run (update event)
          ("Error: An error occured while trying to update an event into the database. " ++)
          (const $ Env.end)
          env
      (["delete", k], _, _) ->
        run (delete (Prelude.read k :: Int))
          ("Error: An error occured while trying to delete an event from the database. " ++)
          (const $ Env.end)
          env
      _ -> endWithError ("Error: Invalid request. JSON: " ++ show json) env
