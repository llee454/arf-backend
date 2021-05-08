--- Defines the insert, update, and delete operations for the Attribute database types.

module Attribute where

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
import qualified Entity

--- Represents attributes
--- @cons key - the entry key
--- @cons created - the date on which the entry was created
--- @cons subject - the entry that the attribute is associated with
data Attribute = Attribute {
  key     :: Maybe Int,
  created :: ClockTime,
  name    :: String,
  subject :: Int
}

---
ofJSON :: JValue -> Maybe Attribute
ofJSON json =
  case json of
    (JObject [
      ("key",     k),
      ("created", JNumber created),
      ("name",    JString name),
      ("subject", JNumber subject)]) ->
      maybeIntOfJSON k >>- \x -> Just $ Attribute x
        (clockTimeOfNum created) name
        (truncate subject)
    _ -> Nothing

---
toJSON :: Attribute -> JValue
toJSON (Attribute k created name subject) =
  JObject [
    ("key",     maybeIntToJSON k),
    ("created", clockTimeToJSON created),
    ("name",    JString name),
    ("subject", JNumber $ i2f $ subject)]

---
insert :: Attribute -> DBAction Attribute
insert x@(Attribute k created name subject)
  | isJust k = failDB $ DBError UnknownError "Error: An error occured while trying to insert an attribute into the database. Cannot insert instantiated attributes."
  | otherwise =
    Entity.insert (Entity.Entity Nothing created name) >+=
    \(Entity.Entity (Just j) _ _) ->
      arf.newAttribWithEntryAttrib_entryKeyWithEntryAttrib_subjectKey (arf.EntryID j) (arf.EntryID subject) >+
      returnDB (Right (x {key = Just j}))

---
read :: Int -> DBAction (Maybe Attribute)
read k =
  select
    ("SELECT " ++
     "  Entry.Timestamp, " ++
     "  Entity.Name, " ++
     "  Attrib.EntryAttrib_subjectKey " ++
     "FROM Entry " ++
     "INNER JOIN Entity ON Entity.EntryEntity_entryKey = Entry.Key " ++
     "INNER JOIN Attrib ON Attrib.EntryAttrib_entryKey = Entry.Key " ++
     "WHERE Entry.Key = '?';")
    [SQLInt k] [SQLTypeDate, SQLTypeString, SQLTypeInt] >+= from
  where
    from :: [[SQLValue]] -> DBAction (Maybe Attribute)
    from res =
      case res of
        [[SQLDate created, SQLString name, SQLInt subject]] ->
          returnDB $ Right $ Just $ Attribute (Just k) created name subject
        [] -> returnDB $ Right Nothing
        _  -> failDB $ DBError UnknownError "Error: An error occured while trying to read an attribute from the database."

---
update :: Attribute -> DBAction ()
update x =
  case x of
    Attribute (Just k) created name subject ->
      Entity.update (Entity.Entity (Just k) created name) >+
      execute
        ("UPDATE Attrib " ++
         "SET EntryAttrib_subjectKey = '?' " ++
         "WHERE EntryAttrib_entryKey = '?';")
        [SQLInt subject, SQLInt k]
    _ -> failDB $ DBError UnknownError "Error: An error occured while trying to update an attribute. Cannot update an uninstantiated attribute."

---
delete :: Int -> DBAction ()
delete k =
  Entity.delete k >+
  execute "DELETE FROM Attrib WHERE EntryAttrib_entryKey = '?';" [SQLInt k]

entityIntf :: EntityIntf Attribute
entityIntf = EntityIntf {
  name_   = "attribute",
  key_    = key,
  ofJSON_ = ofJSON,
  toJSON_ = toJSON,
  insert_ = insert,
  read_   = read,
  update_ = update,
  delete_ = delete
}

--- Returns the attribute that has the given name and is associated with the given entry.
getByEntryAndName :: Int -> String -> DBAction (Maybe Attribute)
getByEntryAndName subject name =
  select
    ("SELECT " ++
     "  Entry.Key, " ++
     "  Entry.Timestamp " ++
     "FROM Entry " ++
     "INNER JOIN Entity ON Entity.EntryEntity_entryKey = Entry.Key " ++
     "INNER JOIN Attrib ON Attrib.EntryAttrib_entryKey = Entry.Key " ++
     "WHERE Attrib.EntryAttrib_subjectKey = '?' AND Entity.Name = '?';")
    [SQLInt subject, SQLString name] [SQLTypeInt, SQLTypeDate] >+= from
  where
    from :: [[SQLValue]] -> DBAction (Maybe Attribute)
    from res =
      case res of
        [[SQLInt k, SQLDate created]] ->
           returnDB $ Right $ Just $ Attribute (Just k) created name subject
        [] -> returnDB $ Right Nothing
        _  -> failDB $ DBError UnknownError "Error: An error occured while trying to retrieve attributes by subject and name."

--- Handles non-CRUD requests.
--- @param args - the URL arguments
--- @param env  - the execution environment
--- @return handles the given requests
extHandler :: [String] -> String -> Env -> IO ()
extHandler args req =
  let json = parseJSON req
    in case (args, json, json >>- ofJSON) of
      (["get-by-entry-and-name", subject, name], _, _) ->
        run (runInTransaction $ getByEntryAndName (Prelude.read subject :: Int) name)
          ("Error: An error occured while trying to retrieve an attribute by subject and name. " ++)
          (\x -> Env.reply $ ppJSON $
            maybeToJSON (\e -> JObject [("attribute", toJSON e)]) x)
      _ -> EntityIntf.defaultHandler args req

handler :: [String] -> Env -> IO ()
handler = EntityIntf.handler entityIntf $ extHandler
