--- This module defines my nutrition tracker utility.

module Nutrition where

import Float
import IO
import Maybe
import JSON.Data
import JSON.Parser
import JSON.Pretty
import Time

import Database.CDBI.ER
import Database.CDBI.Criteria
import Database.CDBI.Connection
import Database.CDBI.Description

import arf
import Base
import Env
import JSONExt
import EntityIntf
import qualified Attribute
import qualified Entity
import qualified Event
import qualified Measurement

---
data Serving = Serving {
  servingType :: String,
  amount      :: Int}

---
servingOfJSON :: JValue -> Maybe Serving
servingOfJSON json =
  case json of
    (JObject [("type", JString servingType), ("amount", JNumber amount)]) ->
      Just $ Serving servingType (truncate amount)
    _ -> Nothing

---
data Meal = Meal {
  key         :: Maybe Int,
  created     :: ClockTime,
  timestamp   :: ClockTime,
  description :: String,
  calories    :: Float,
  servings    :: [Serving]}

---
mealOfJSON :: JValue -> Maybe Meal
mealOfJSON json =
  case json of
    (JObject [
      ("key",         k),
      ("created",     JNumber created),
      ("timestamp",   JNumber timestamp),
      ("description", JString description),
      ("calories",    JNumber calories),
      ("servings",    JArray xs)]) ->
      maybeIntOfJSON k >>- \x -> Just $ Meal x
        (clockTimeOfNum created)
        (clockTimeOfNum timestamp)
        description calories
        (mapMaybe servingOfJSON xs)
    _ -> Nothing

---
servingToJSON :: Serving -> JValue
servingToJSON (Serving servingType amount) =
  JObject [
    ("type",   JString servingType),
    ("amount", JNumber $ i2f $ amount)]

---
mealToJSON :: Meal -> JValue
mealToJSON (Meal k created timestamp description calories servings) =
  JObject [
    ("key",         maybeIntToJSON k),
    ("created",     clockTimeToJSON created),
    ("timestamp",   clockTimeToJSON timestamp),
    ("description", JString description),
    ("calories",    JNumber calories),
    ("servings",    JArray $ map servingToJSON servings)]

---
insertServing :: Int -> Serving -> DBAction ()
insertServing mealID (Serving servingType amount) =
  arf.newServingWithEntryServing_mealKey servingType amount (arf.EntryID mealID) >+
  returnDB (Right ())

---
insertServings :: Int -> [Serving] -> DBAction ()
insertServings mealID servings =
  foldr
    (\serving acc -> acc >+ insertServing mealID serving >+ (returnDB $ Right ()))
    (returnDB $ Right ())
    servings

---
insertMeal :: Meal -> DBAction Meal
insertMeal x@(Meal k created timestamp description calories servings)
  | isJust k = failDB $ DBError UnknownError "Error: An error occured while trying to insert a meal into the database. Cannot insert an instantiated meal."
  | otherwise =
    Event.insert (Event.Event Nothing created timestamp) >+=
    (\(Event.Event (Just j) _ _) ->
      arf.newMealWithEntryMeal_entryKey calories description (arf.EntryID j) >+
      insertServings j servings >+
      (returnDB $ Right $ x {key = Just j}))

---
deleteServings :: Int -> DBAction ()
deleteServings k =
  execute "DELETE FROM Serving WHERE Serving.EntryServing_mealKey = '?';" [SQLInt k]

---
deleteMeal :: Int -> DBAction ()
deleteMeal k =
  execute "DELETE FROM Entry WHERE Key = '?';" [SQLInt k] >+
  execute "DELETE FROM Event WHERE EntryEvent_entryKey = '?';" [SQLInt k] >+
  execute "DELETE FROM Meal  WHERE EntryMeal_entryKey = '?';" [SQLInt k] >+
  deleteServings k

---
readServings :: Int -> DBAction [Serving]
readServings mealID =
  select
    ("SELECT " ++
     "  Serving.ServingType, " ++
     "  Serving.Amount " ++
     "FROM Serving " ++
     "WHERE Serving.EntryServing_mealKey = '?';")
    [SQLInt mealID] [SQLTypeString, SQLTypeInt] >+= from
  where
    from :: [[SQLValue]] -> DBAction [Serving]
    from res = returnDB $ Right $
      map
        (\[SQLString servingType, SQLInt amount] -> Serving servingType amount)
        res

---
readMeal :: Int -> DBAction (Maybe Meal)
readMeal k =
  readServings k >+=
  (\servings ->   
    select
      ("SELECT " ++
       "  Entry.Timestamp, " ++
       "  Event.Timestamp, " ++
       "  Meal.Calories, " ++
       "  Meal.Description " ++ 
       "FROM Entry " ++
       "INNER JOIN Event On Event.EntryEvent_entryKey = Entry.Key " ++
       "INNER JOIN Meal On Meal.EntryMeal_entryKey = Entry.Key " ++
       "WHERE Entry.Key = '?';")
      [SQLInt k] [SQLTypeDate, SQLTypeDate, SQLTypeFloat, SQLTypeString] >+= from servings)
  where
    from :: [Serving] -> [[SQLValue]] -> DBAction (Maybe Meal)
    from servings res =
      case res of
        [[SQLDate created, SQLDate timestamp, SQLFloat calories, SQLString description]] ->
          returnDB $ Right $ Just $ Meal (Just k) created timestamp description calories servings
        [] -> returnDB $ Right Nothing
        _  -> failDB $ DBError UnknownError "Error: An error occured while trying to read a meal from the database."

---
update :: Meal -> DBAction ()
update meal =
  case meal of
    Meal (Just k) created timestamp description calories servings ->
      deleteServings k >+
      insertServings k servings >+
      execute "UPDATE Entry SET Timestamp = '?' WHERE Key = '?';" [SQLDate created, SQLInt k] >+
      execute "UPDATE Event SET Timestamp = '?' WHERE EntryEvent_entryKey = '?';" [SQLDate timestamp, SQLInt k] >+
      execute "UPDATE Meal  SET (Calories, Description) = ('?', '?') WHERE EntryMeal_entryKey = '?';" [SQLFloat calories, SQLString description, SQLInt k]
    _ -> failDB $ DBError UnknownError "Error: An error occured while trying to update a meal."

entityIntf :: EntityIntf Meal
entityIntf = EntityIntf {
  name_   = "meal",
  key_    = key,
  ofJSON_ = mealOfJSON,
  toJSON_ = mealToJSON,
  insert_ = insertMeal,
  read_   = readMeal,
  update_ = update,
  delete_ = deleteMeal
}

--- 
handler :: [String] -> Env -> IO ()
handler = EntityIntf.handler entityIntf $ EntityIntf.defaultHandler
