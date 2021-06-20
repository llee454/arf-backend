--- This module defines my nutrition tracker utility.

module Nutrition where

import Text.CSV
import Float
import IO
import Maybe
import JSON.Data
import JSON.Parser
import JSON.Pretty
import LocalTime

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
  created     :: Int,
  timestamp   :: Int,
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
        (truncate created)
        (truncate timestamp)
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
    ("created",     JNumber $ i2f $ created),
    ("timestamp",   JNumber $ i2f $ timestamp),
    ("description", JString description),
    ("calories",    JNumber calories),
    ("servings",    JArray $ map servingToJSON servings)]

---
mealToCSV :: Meal -> [String]
mealToCSV (Meal Nothing _ _ _ _ _) = error $ "[mealToCSV] Error: an error occured while trying to convert meals into CSVs."
mealToCSV (Meal (Just k) created timestamp description calories servings) = [
    (show k), (show created), (show timestamp), description, (show calories),
    show (JArray (map servingToJSON servings))
  ]

---
mealsToCSV :: [Meal] -> [[String]]
mealsToCSV = map mealToCSV

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
      [SQLInt k] [SQLTypeInt, SQLTypeInt, SQLTypeFloat, SQLTypeString] >+= from servings)
  where
    from :: [Serving] -> [[SQLValue]] -> DBAction (Maybe Meal)
    from servings res =
      case res of
        [[SQLInt created, SQLInt timestamp, SQLFloat calories, SQLString description]] ->
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
      execute "UPDATE Entry SET Timestamp = '?' WHERE Key = '?';" [SQLInt created, SQLInt k] >+
      execute "UPDATE Event SET Timestamp = '?' WHERE EntryEvent_entryKey = '?';" [SQLInt timestamp, SQLInt k] >+
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

--- Accepts an ISO8601 UTC date time and returns the IDs of meals that
--- occured after the given date time.
readMealsAfter :: Int -> DBAction [Meal]
readMealsAfter timestamp =
  select
    ("SELECT Entry.Key " ++
     "FROM Entry " ++
     "INNER JOIN Event On Event.EntryEvent_entryKey = Entry.Key " ++
     "INNER JOIN Meal On Meal.EntryMeal_entryKey = Entry.Key " ++
     "WHERE Event.Timestamp >= '?';")
    [SQLInt timestamp] [SQLTypeInt] >+= from
  where
    from :: [[SQLValue]] -> DBAction [Meal]
    from res =
      foldr
        (\[SQLInt k] acc -> acc >+= \meals -> readMeal k >+= \(Just meal) -> returnDB $ Right (meal : meals))
        (returnDB $ Right [])
        res

--- Return the set of meals eaten after midnight New York local time.
readMealsToday :: IO (DBAction [Meal])
readMealsToday = getMidnightPosix >>= return . readMealsAfter

--- The recommended daily calorie limit when trying to lose weight.
dailyCalorieLimit :: Float
dailyCalorieLimit = 1900

--- Returns the number of calories that I should aim to consume each hour
--- when trying to lose weight.
---
--- Note: this function assumes that I eat all of my meals between
--- 6:00 AM and 9:00 PM (local time).
calorieConsumptionRate :: Float
calorieConsumptionRate = dailyCalorieLimit /. (12 +. 9 -. 6)

--- Returns the number of calories that I should aim to consume by the
--- current time.
calorieLimit :: IO Float
calorieLimit = do
  t <- getCurrPosixTime
  s <- getMidnightPosix
  return $ calorieConsumptionRate *. ((i2f t) -. (i2f s) -. (6 *. 3600)) /. 3600

--- Returns the number of calories remaining to be consumed.
---
--- Note: this function is used to help pace my calorie consumption so
--- that throughout a day I can limit my consumption to my target daily
--- intake.
remainingCalories :: (Float -> Env -> IO ()) -> Env -> IO ()
remainingCalories f env = do
  query <- readMealsToday
  targetCals <- calorieLimit
  run (runInTransaction query)
    ("Error: An error occured while trying to calculate the number of calories remaining today. " ++)
    (\meals ->
      let cals = foldr (\meal acc -> acc -. (calories meal)) targetCals meals
        in f cals)
    env

--- Returns the number of calories eaten today.
caloriesToday :: (Float -> Env -> IO ()) -> Env -> IO ()
caloriesToday f env = do
  query <- readMealsToday
  run (runInTransaction query)
    ("Error: An error occured while trying to calculate the number of calories eaten today. " ++)
    (\meals ->
      let cals = foldr (\meal acc -> acc +. (calories meal)) 0 meals
        in f cals)
    env

--- Returns my recommended meal size given three meals a day and 2 snacks
--- of 200 calories, +Cals.
mealSize :: Float
mealSize = (dailyCalorieLimit -. 400) /. 3

--- Returns the number of hours until I can have my next meal if calorie
--- pacing.
numHoursTillNextMeal :: (Float -> Env -> IO ()) -> Env -> IO ()
numHoursTillNextMeal f =
  remainingCalories $
    \cals ->
      let hours = (mealSize -. cals) /. calorieConsumptionRate
        in f hours

--- Returns a record of every meal.
readMeals :: DBAction [Meal]
readMeals =
  select
    ("SELECT Entry.Key " ++
     "FROM Entry " ++
     "INNER JOIN Event On Event.EntryEvent_entryKey = Entry.Key " ++
     "INNER JOIN Meal On Meal.EntryMeal_entryKey = Entry.Key;")
    [] [SQLTypeInt] >+= from
  where
    from :: [[SQLValue]] -> DBAction [Meal]
    from res =
      foldr
        (\[SQLInt k] acc -> acc >+= \meals -> readMeal k >+= \(Just meal) -> returnDB $ Right (meal : meals))
        (returnDB $ Right [])
        res

--- 
handler :: [String] -> Env -> IO ()
handler args env = do
  case args of
    ["meals-csv.csv"] ->
      let query = readMeals
      in run (runInTransaction query)
        ("Error: An error occured while trying to read all of the meal entries in the database. " ++)
        (\meals env -> Env.reply (showCSV $ mealsToCSV meals) env)
        env
    ["cals-today"] -> caloriesToday (\cals -> Env.reply (show cals)) env
    ["meals-today"] -> do
      query <- readMealsToday
      run (runInTransaction query)
        ("Error: An error occured while trying to read the meals eaten today. " ++)
        (Env.reply . ppJSON . JArray . map mealToJSON)
        env
    ["remaining-cals"] -> remainingCalories (\cals -> Env.reply (show cals)) env
    ["hours-till-next-meal"] -> numHoursTillNextMeal (\hours -> Env.reply (show hours)) env
    _ -> EntityIntf.handler entityIntf EntityIntf.defaultHandler args env
        
