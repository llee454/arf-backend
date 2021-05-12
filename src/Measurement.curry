module Measurement where

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

--- Represents measurements
--- @cons key - the entry key
--- @cons created - the date on which the measurement was recorded
--- @cons timestamp - the date on which the measurement was made
--- @cons measurer - the entity that made the measurement
--- @cons measured - the entity that was measured (an attribute entry)
--- @cons value - the value of the measurement
--- @cons unit - the units associated with the measurement (e.g. "lbs")
--- @cons precision - the error margin associated with the measurement (e.g. 2.0)
data Measurement = Measurement {
  key        :: Maybe Int,
  created    :: ClockTime,
  timestamp  :: ClockTime,
  measurer   :: Int,
  measured   :: Int,
  value      :: Float,
  unit       :: String,
  precision  :: Float
}

---
ofJSON :: JValue -> Maybe Measurement
ofJSON json =
  case json of
    (JObject [
      ("key",       k),
      ("created",   JNumber created),
      ("timestamp", JNumber timestamp),
      ("measurer",  JNumber measurer),
      ("measured",  JNumber measured),
      ("value",     JNumber value),
      ("unit",      JString unit),
      ("precision", JNumber precision)]) ->
      maybeIntOfJSON k >>- \x -> Just $ Measurement x 
        (clockTimeOfNum created)
        (clockTimeOfNum timestamp)
        (truncate measurer)
        (truncate measured)
        value unit precision
    _ -> Nothing

---
toJSON :: Measurement -> JValue
toJSON (Measurement k created timestamp measurer measured value unit precision) =
  JObject [
    ("key",       maybeIntToJSON k),
    ("created",   clockTimeToJSON created),
    ("timestamp", clockTimeToJSON timestamp),
    ("measurer",  JNumber $ i2f $ measurer),
    ("measured",  JNumber $ i2f $ measured),
    ("value",     JNumber value),
    ("unit",      JString unit),
    ("precision", JNumber precision)]

---
insert :: Measurement -> DBAction Measurement
insert x@(Measurement k created timestamp measurer measured value unit precision)
  | isJust k = failDB $ DBError UnknownError "Error: An error occured while trying to insert a measurement into the database. Cannot insert an instantiated measurement."
  | otherwise =
    Action.insert (Action.Action Nothing created timestamp measurer) >+=
    \(Action.Action (Just j) _ _ _) ->
      arf.newMeasurementWithEntryMeasurement_entryKeyWithEntryMeasurement_ofKey
        unit value precision (arf.EntryID j) (arf.EntryID measured) >+
        returnDB (Right (x {key = Just j}))

---
read :: Int -> DBAction (Maybe Measurement)
read k =
  select
    ("SELECT " ++
     "  Entry.Timestamp, " ++
     "  Event.Timestamp, " ++
     "  Action.EntryAction_subjectKey, " ++
     "  Measurement.Unit, " ++
     "  Measurement.Value, " ++
     "  Measurement.Precision, " ++
     "  Measurement.EntryMeasurement_ofKey " ++
     "FROM Entry " ++
     "INNER JOIN Event       ON Event.EntryEvent_entryKey       = Entry.Key " ++
     "INNER JOIN Action      ON Action.EntryAction_entryKey     = Entry.Key " ++
     "INNER JOIN Measurement ON Measurement.EntryMeasurement_entryKey = Entry.Key " ++
     "WHERE Entry.Key = '?';")
    [SQLInt k] [SQLTypeDate, SQLTypeDate, SQLTypeInt, SQLTypeString, SQLTypeFloat, SQLTypeFloat, SQLTypeInt] >+= from
  where  
    from :: [[SQLValue]] -> DBAction (Maybe Measurement)
    from res =
      case res of
        [[SQLDate created, SQLDate timestamp, SQLInt measurer, SQLString unit, SQLFloat value, SQLFloat precision, SQLInt measured]] ->
          returnDB $ Right $ Just $ Measurement (Just k) created timestamp measurer measured value unit precision
        [] -> returnDB $ Right Nothing
        _  -> failDB $ DBError UnknownError "Error: An error occured while trying to read a measurement from the database."

---
update :: Measurement -> DBAction ()
update x =
  case x of
    Measurement (Just k) created timestamp measurer measured value unit precision ->
      Action.update (Action.Action (Just k) created timestamp measurer) >+
      execute 
        ("UPDATE Measurement " ++
         "SET (Unit, Value, Precision, EntryMeasurement_ofKey) = ('?', '?', '?', '?') " ++
         "WHERE EntryMeasurement_entryKey = '?';")
        [SQLString unit, SQLFloat value, SQLFloat precision, SQLInt measured, SQLInt k]
    _ -> failDB $ DBError UnknownError "Error: An error occured while trying to update a measurement."

---
delete :: Int -> DBAction ()
delete k =
  Action.delete k >+
  execute "DELETE FROM Measurement WHERE EntryMeasurement_entryKey = '?';" [SQLInt k]

entityIntf :: EntityIntf Measurement
entityIntf = EntityIntf {
  name_   = "measurement",
  key_    = key,
  ofJSON_ = ofJSON,
  toJSON_ = toJSON,
  insert_ = insert,
  read_   = read,
  update_ = update,
  delete_ = delete
}

--- Returns the set of measurements of the referenced entity (attribute).
--- @param measured - an entity ID
--- @return the measurements of the referenced entity.
getByMeasured :: Int -> DBAction [Measurement]
getByMeasured measured =
  select
    ("SELECT " ++
     "  Entry.Key, " ++
     "  Entry.Timestamp, " ++
     "  Event.Timestamp, " ++
     "  Action.EntryAction_subjectKey, " ++
     "  Measurement.Unit, " ++
     "  Measurement.Value, " ++
     "  Measurement.Precision " ++
     "FROM Entry " ++
     "INNER JOIN Event       ON Event.EntryEvent_entryKey       = Entry.Key " ++
     "INNER JOIN Action      ON Action.EntryAction_entryKey     = Entry.Key " ++
     "INNER JOIN Measurement ON Measurement.EntryMeasurement_entryKey = Entry.Key " ++
     "WHERE Measurement.EntryMeasurement_ofKey = '?' " ++
     "ORDER BY Event.Timestamp;")
    [SQLInt measured] [SQLTypeInt, SQLTypeDate, SQLTypeDate, SQLTypeInt, SQLTypeString, SQLTypeFloat, SQLTypeFloat] >+= from
  where  
    from :: [[SQLValue]] -> DBAction [Measurement]
    from = returnDB . Right . mapMaybe parseRes

    parseRes :: [SQLValue] -> Maybe Measurement
    parseRes res =
      case res of
        [SQLInt k, SQLDate created, SQLDate timestamp, SQLInt measurer, SQLString unit, SQLFloat value, SQLFloat precision] ->
          Just $ Measurement (Just k) created timestamp measurer measured value unit precision
        _  -> Nothing

--- Handles non-CRUD requests.
--- @param args - the URL arguments
--- @param env  - the execution environment
--- @return handles the given requests
extHandler :: [String] -> String -> Env -> IO ()
extHandler args req =
  let json = parseJSON req
    in case (args, json, json >>- ofJSON) of
      (["get-by-measured", measured], _, _) ->
        run (runInTransaction $ getByMeasured (Prelude.read measured :: Int))
          ("Error: An error occured while trying to retrieve a measurement using the ID of the measured entity. " ++)
          (\x -> Env.reply $ ppJSON $
            JArray $ map (\e -> JObject [("measurement", toJSON e)]) x)
      _ -> EntityIntf.defaultHandler args req

--- 
handler :: [String] -> Env -> IO ()
handler = EntityIntf.handler entityIntf $ extHandler
