module Activity where

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
import qualified arf
import qualified Action

--- Represents activities
--- @cons key - the entry key
--- @cons created - the date on which the entry was created
--- @cons start - the date on which the activity started
--- @cons duration - the duration of the activity
--- @cons subject - the entry of the entity that performed this activity
data Activity = Activity {
  key      :: Maybe Int,
  created  :: ClockTime,
  start    :: ClockTime,
  duration :: ClockTime,
  subject  :: Int }

---
ofJSON :: JValue -> Maybe Activity
ofJSON json =
  case json of
    (JObject [("key", k), ("created", JNumber created), ("start", JNumber start), ("duration", JNumber duration), ("subject", JNumber subject)]) ->
      maybeIntOfJSON k >>- \x -> Just $ Activity x 
        (clockTimeOfNum created)
        (clockTimeOfNum start)
        (clockTimeOfNum duration)
        (truncate subject)
    _ -> Nothing

---
toJSON :: Activity -> JValue
toJSON (Activity k created start duration subject) =
  JObject [
    ("key",      maybeIntToJSON k),
    ("created",  clockTimeToJSON created),
    ("start",    clockTimeToJSON start),
    ("duration", clockTimeToJSON duration),
    ("subject",  JNumber $ i2f $ subject)]

---
insert :: Activity -> DBAction Activity
insert x@(Activity k created start duration subject)
  | isJust k = failDB $ DBError UnknownError "Error: An error occured while trying to insert an activity into the database. Cannot insert an instantiated activity."
  | otherwise =
    Action.insert (Action.Action Nothing created start subject) >+=
    \(Action.Action (Just j) _ _ _) ->
      arf.newActivityWithEntryActivity_entryKey duration (arf.EntryID j) >+
      returnDB (Right (x {key = Just j}))

---
read :: Int -> DBAction (Maybe Activity)
read k =
  select
    ("SELECT Entry.Timestamp, Event.Timestamp, Action.EntryAction_subjectKey, Activity.Duration" ++
     "FROM Entry" ++
     "INNER JOIN Event    ON Event.EntryEvent_entryKey       = Entry.Key" ++
     "INNER JOIN Action   ON Action.EntryAction_entryKey     = Entry.Key" ++
     "INNER JOIN Activity ON Activity.EntryActivity_entryKey = Entry.Key" ++
     "WHERE Entry.Key = '?';")
    [SQLInt k] [SQLTypeDate, SQLTypeDate, SQLTypeInt, SQLTypeDate] >+= from
  where  
    from :: [[SQLValue]] -> DBAction (Maybe Activity)
    from res =
      case res of
        [[SQLDate created, SQLDate start, SQLInt subject, SQLDate duration]] ->
          returnDB $ Right $ Just $ Activity (Just k) created start duration subject
        [] -> returnDB $ Right Nothing
        _  -> failDB $ DBError UnknownError "Error: An error occured while trying to read an activity from the database."

---
update :: Activity -> DBAction ()
update x =
  case x of
    Activity (Just k) created start duration subject ->
      Action.update (Action.Action (Just k) created start subject) >+
      execute "UPDATE Activity SET Activity.Duration = '?' WHERE Activity.EntryActivity_entryKey = '?';" [SQLDate duration, SQLInt k]
    _ -> failDB $ DBError UnknownError "Error: An error occured while trying to update an activity."

---
delete :: Int -> DBAction ()
delete k =
  Action.delete k >+
  execute "DELETE FROM Activity WHERE EntryActivity_entryKey = '?';" [SQLInt k]

entityIntf :: EntityIntf Activity
entityIntf = EntityIntf {
  name_   = "activity",
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
handler = EntityIntf.handler entityIntf $ EntityIntf.defaultHandler
