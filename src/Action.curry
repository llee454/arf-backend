--- Defines the insert, update, and delete operations for the Action data type.

module Action where

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
import qualified Event

--- Represents actions
--- @cons key - the entry key
--- @cons created - the date on which the entry was created
--- @cons timestamp - the date on which the action occured
--- @cons subject - the entry that this action is declaring to be an action.
data Action = Action {
  key       :: Maybe Int,
  created   :: ClockTime,
  timestamp :: ClockTime,
  subject   :: Int }


---
ofJSON :: JValue -> Maybe Action
ofJSON json =
  case json of
    (JObject [("key", k), ("created", JNumber created), ("timestamp", JNumber timestamp), ("subject", JNumber subject)]) ->
      maybeIntOfJSON k >>- \x -> Just $ Action x (clockTimeOfNum created) (clockTimeOfNum timestamp) (truncate subject)
    _ -> Nothing

---
toJSON :: Action -> JValue
toJSON (Action k created timestamp subject) =
  JObject [
    ("key",       maybeIntToJSON k),
    ("created",   clockTimeToJSON created),
    ("timestamp", clockTimeToJSON timestamp),
    ("subject",   JNumber $ i2f $ subject)]

---
insert :: Action -> DBAction Action
insert x@(Action k created timestamp subject)
  | isJust k = failDB $ DBError UnknownError "Error: An error occured while trying to insert an attribute into the database. Cannot insert an instantiated action."
  | otherwise =
    Event.insert (Event.Event Nothing created timestamp) >+=
    \(Event.Event (Just j) _ _) ->
      arf.newActionWithEntryAction_entryKeyWithEntryAction_subjectKey (arf.EntryID j) (arf.EntryID subject) >+
      returnDB (Right (x {key = Just j}))

---
read :: Int -> DBAction (Maybe Action)
read k =
  select
    ("SELECT Entry.Timestamp, Event.Timestamp, Action.EntryAction_subjectKey" ++
     "FROM Entry" ++
     "INNER JOIN Event  ON Event.EntryEvent_entryKey   = Entry.Key" ++
     "INNER JOIN Action ON Action.EntryAction_entryKey = Entry.Key" ++
     "WHERE Entry.Key = '?';")
    [SQLInt k] [SQLTypeDate, SQLTypeDate, SQLTypeInt] >+= from
  where
    from :: [[SQLValue]] -> DBAction (Maybe Action)
    from res =
      case res of
        [[SQLDate created, SQLDate timestamp, SQLInt subject]] ->
          returnDB $ Right $ Just $ Action (Just k) created timestamp subject
        [] -> returnDB $ Right Nothing
        _  -> failDB $ DBError UnknownError "Error: An error occured while trying to read an action from the database."

---
update :: Action -> DBAction ()
update x =
  case x of
    Action (Just k) created timestamp subject ->
      Event.update (Event.Event (Just k) created timestamp) >+
      execute "UPDATE Action SET EntryAction_subjectKey = '?' WHERE EntryAction_entryKey = '?';" [SQLInt subject, SQLInt k]
    _ -> failDB $ DBError UnknownError "Error: An error occured while trying to update an action."

--- Accepts an Event ID and deletes the associated event.
delete :: Int -> DBAction ()
delete k =
  Event.delete k >+
  execute "DELETE FROM Action WHERE EntryAction_entryKey = '?';" [SQLInt k]

entityIntf :: EntityIntf Action
entityIntf = EntityIntf {
  name_   = "action",
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
