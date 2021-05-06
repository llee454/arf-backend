--- Defines data types and functions for representing "predicate" entity types.
--- A "predicate" entity type is an entity type that is an entry referenced by a
--- corresponding entry in a predicate database table. The predicate database
--- table references those entries for which a given predicate holds. For example,
--- the action, and attribute entity types are predicate entity types. All
--- predicate entity types have the same structure, specifically: a key, a
--- created date, and an entity reference.

module PredicateIntf where

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

--- @cons insert_ - the "new entity" function generated by ERD Tools. 
data PredicateIntf a b = PredicateIntf {
  predicateName_   :: String,
  cons_            :: Maybe Int -> ClockTime -> Int -> a,
  predicateKey_    :: a -> Maybe Int,
  created_         :: a -> ClockTime,
  subject_         :: a -> Int,
  tblName_         :: String,
  predicateInsert_ :: arf.EntryID -> arf.EntryID -> Database.CDBI.Connection.DBAction b}

---
ofJSON :: PredicateIntf a _ -> JValue -> Maybe a
ofJSON intf json =
  case json of
    (JObject [("key", k), ("created", JNumber created), ("subject", JNumber subject)]) ->
      maybeIntOfJSON k >>- \x -> Just $ (cons_ intf) x (clockTimeOfNum created) (truncate subject)
    _ -> Nothing

---
toJSON :: PredicateIntf a _ -> a -> JValue
toJSON intf x =
  JObject [
    ("key",     maybeIntToJSON $ predicateKey_ intf x),
    ("created", clockTimeToJSON $ created_ intf x),
    ("subject", JNumber $ i2f $ subject_ intf x)]

--- Inserts a predicate entity into the database.
--- @return the inserted entity with the key set.
insert :: PredicateIntf a b -> a -> DBAction a
insert intf x
  | isJust (predicateKey_ intf x) = failDB $ DBError UnknownError $ "Error: An error occured while trying to insert an attribute into the database. Cannot insert an instantiated " ++ predicateName_ intf ++ "."
  | otherwise =
    arf.newEntry (created_ intf x) >+=
    (\(arf.Entry (EntryID j) _) ->
     (predicateInsert_ intf (EntryID j) (EntryID $ subject_ intf x)) >+
     returnDB (Right (cons_ intf (Just j) (created_ intf x) (subject_ intf x))))

--- Accepts a predicate entity ID and reads the associated entity.
read :: PredicateIntf a _ -> Int -> DBAction (Maybe a)
read intf k =
  let name = tblName_ intf in
  select
    ("SELECT Entry.Timestamp, Entry" ++ name ++ "_subjectKey FROM Entry INNER JOIN " ++ name ++  " On " ++ name ++ ".Entry" ++ name ++ "_entryKey = Entry.Key WHERE Entry.Key = '?';")
    [SQLInt k] [SQLTypeDate, SQLTypeInt] >+= from
  where
    -- from :: [[SQLValue]] -> DBAction (Maybe a)
    from res =
      case res of
        [[SQLDate created, SQLInt subject]] -> returnDB $ Right $ Just $ (cons_ intf) (Just k) created subject
        [] -> returnDB $ Right Nothing
        _  -> failDB $ DBError UnknownError $ "Error: An error occured while trying to read a(n) " ++ predicateName_ intf ++ " from the SQLite database."

--- Accepts a predicate entity and updates the associated database tables.
update :: PredicateIntf a _ -> a -> DBAction ()
update intf x =
  case predicateKey_ intf x of 
    Just k ->
      execute "UPDATE ENTRY SET Timestamp = '?' WHERE Key = '?';" [SQLDate (created_ intf x), SQLInt k]
    _ -> failDB $ DBError UnknownError $ "Error: An error occured while trying to update a(n) " ++ predicateName_ intf ++ "."

--- Accepts an Event ID and deletes the associated predicate entity.
delete :: PredicateIntf a _ -> Int -> DBAction ()
delete intf k =
  execute "DELETE FROM Entry WHERE Key = '?';" [SQLInt k] >+
  execute ("DELETE FROM " ++ tblName_ intf ++ " WHERE Entry" ++ tblName_ intf ++ " = '?';") [SQLInt k]

entityIntf :: PredicateIntf a _ -> EntityIntf a
entityIntf intf = EntityIntf {
  name_   = predicateName_ intf,
  key_    = predicateKey_ intf,
  ofJSON_ = ofJSON intf,
  toJSON_ = toJSON intf,
  insert_ = insert intf,
  read_   = read intf,
  update_ = update intf,
  delete_ = delete intf
}

--- 
handler :: PredicateIntf a _ -> [String] -> Env -> IO ()
handler intf = EntityIntf.handler $ entityIntf intf
