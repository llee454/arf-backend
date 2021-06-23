--- This module defines my weight tracker utility.
--- This module defines the request handler for adding weight records
--- and a function for initializing the database to measure my weight.

module Health where

import Text.CSV
import LocalTime
import Float
import IO
import Maybe
import JSON.Data
import JSON.Parser
import JSON.Pretty

import Database.CDBI.Connection

import Base
import Env
import JSONExt
import qualified Entity
import qualified EntityIntf
import qualified Attribute
import qualified Measurement as M

myName :: String
myName = "Larry Darryl. Lee Jr."

--- Represents my blood pressure
--- @cons systolic  - my systolic pressure (in. Hg.)
--- @cons diastolic - my diastolic pressure (in. Hg.)
--- @cons puls      - my pule (BPM)
data BloodPressure = BloodPressure {
  systolic  :: Int,
  diastolic :: Int,
  pulse     :: Int
}

--- Represents my standard body measurements
--- @cons waist - my waist circumference as measured through my navel (in.)
--- @cons biceps - my bicep circumference as measured through the
---   middle of my right upper arm (in.)
--- @cons weight - my weight (lbs.)
data BodyMeasurements = BodyMeasurements {
  waist  :: Float,
  biceps :: Float,
  weight :: Float
}

--- Initializes the database by defining an entry that represents me,
--- and attributes about my health.
init :: IO (DBAction ())
init = do
  currTime <- getCurrPosixTime
  return $
    Entity.insert (Entity.Entity Nothing currTime myName) >+=
    \(Entity.Entity (Just k) _ _) ->
      let createAttrib name = Attribute.insert (Attribute.Attribute Nothing currTime name k)
        in createAttrib "Health.BloodPressure.systolic"  >+
           createAttrib "Health.BloodPressure.diastolic" >+
           createAttrib "Health.BloodPressure.pulse"     >+
           createAttrib "Health.BodyMeasurement.waist"   >+
           createAttrib "Health.BodyMeasurement.biceps"  >+
           createAttrib "Health.BodyMeasurement.weight"  >+
           (returnDB $ Right ())

-- TODO: Rewrite using getEntryByNameErr fns.
--- Accepts a list of measurements of attributes about me and inserts them into the database.
--- @param measurementTimestamp - the date on which the measurement was taken
--- @param measurement - a list of tuples of the form (attribName, value, unit, prec)
--- @return records the measurements for the named attributes
insertMeasurements :: [(String, Float, String, Float)] -> Int -> IO (DBAction ())
insertMeasurements measurements measurementTimestamp = do
  currTime <- getCurrPosixTime
  return $
    Entity.getByName myName >+=
    \(Just (Entity.Entity (Just k) _ _)) ->
      let
        record attribName value unit prec =
          Attribute.getByEntryAndName k attribName >+=
          \(Just (Attribute.Attribute (Just j) _ _ _)) ->
            M.insert (M.Measurement
              Nothing currTime measurementTimestamp k j value unit prec) >+
            (returnDB $ Right ())
      in
        foldr
          (\(attribName, value, unit, prec) acc ->
            acc >+ record attribName value unit prec)
          (returnDB $ Right ())
          measurements

--- Records a blood pressure measurement.
--- @param measurements - the measurements.
--- @param measurementTimestamp - the date on which the measurement was taken.
insertBloodPressure :: BloodPressure -> Int -> IO (DBAction ())
insertBloodPressure (BloodPressure systolic diastolic pulse) =
  insertMeasurements
    [("Health.BloodPressure.systolic",  i2f systolic,  "in. Hg.", 10.0),
     ("Health.BloodPressure.diastolic", i2f diastolic, "in. Hg.", 10.0),
     ("Health.BloodPressure.pulse",     i2f pulse,     "bpm",     10.0)]

--- Records a set of body measurements.
--- @param measurements - the measurements.
--- @param measurementTimestamp - the date on which the measurement was taken.
insertBodyMeasurements :: BodyMeasurements -> Int -> IO (DBAction ())
insertBodyMeasurements (BodyMeasurements waist biceps weight) =
  insertMeasurements
    [("Health.BodyMeasurement.waist",  waist,  "in.", 0.5),
     ("Health.BodyMeasurement.biceps", biceps, "in.", 0.0625),
     ("Health.BodyMeasurement.weight", weight, "lbs.", 0.2)]

--- Accepts a JSON object that represents a blood pressure measurement
--- and returns the measurements and the date on which the measurments
--- were taken.
bloodPressureOfJSON :: JValue -> Maybe (Int, BloodPressure)
bloodPressureOfJSON json =
  case json of
    (JObject [
      ("measurementTimestamp", JNumber measurementTimestamp),
      ("systolic",             JNumber systolic),
      ("diastolic",            JNumber diastolic),
      ("pulse",                JNumber pulse)]) ->
      Just (truncate measurementTimestamp,
        BloodPressure (truncate systolic) (truncate diastolic) (truncate pulse))
    _ -> Nothing

--- Accepts a JSON object that represents body measurements and returns
--- the measurements and the date on which the measurements were taken.
bodyMeasurementsOfJSON :: JValue -> Maybe (Int, BodyMeasurements)
bodyMeasurementsOfJSON json =
  case json of
    (JObject [
      ("measurementTimestamp", JNumber measurementTimestamp),
      ("waist",                JNumber waist),
      ("biceps",               JNumber biceps),
      ("weight",               JNumber weight)]) ->
      Just (truncate measurementTimestamp, BodyMeasurements waist biceps weight)
    _ -> Nothing

--- Accepts an attribute name and returns the set of measurements of
--- that attribute on me.
readAttribMeasurements :: String -> DBAction [M.Measurement]
readAttribMeasurements attribName =
  Entity.getByNameErr myName >+=
  \me ->
    Attribute.getByEntryAndNameErr (fromJust $ Entity.key me) attribName >+=
    \attrib ->
      M.getByMeasured (fromJust $ Attribute.key attrib)

readWeightMeasurements :: DBAction [M.Measurement]
readWeightMeasurements = readAttribMeasurements "Health.BodyMeasurement.weight"

--- Accepts a health measurement concerning me and returns the equivalent CSV row.
measurementToCSV :: M.Measurement -> [String]
measurementToCSV (M.Measurement k _ timestamp _ _ value unit precision) = [
    (show k),
    (toISO8601 $ fromPosix timestamp),
    (show value),
    (show unit),
    (show precision)
  ]

measurementsToCSV :: [M.Measurement] -> [[String]]
measurementsToCSV = map measurementToCSV

--- 
handler :: [String] -> Env -> IO ()
handler args env = do
  req <- getContents
  let json = parseJSON req
    in case (args, json >>- bloodPressureOfJSON, json >>- bodyMeasurementsOfJSON) of
      (["weight-csv"], _, _) ->
        run (runInTransaction readWeightMeasurements) 
          ("Error: An error occured while trying to read my weight measurements. " ++)
          (\measurements -> Env.reply (showCSV $ measurementsToCSV measurements))
          env
      (["record-blood-pressure"], Just (measurementTimestamp, bloodPressure), _) -> do
        query <- insertBloodPressure bloodPressure measurementTimestamp
        run (runInTransaction query)
          ("Error: An error occured while trying to record a blood pressure measurement. " ++)
          (\_ -> Env.reply "success")
          env
      (["record-body-measurements"], _, Just (measurementTimestamp, bodyMeasurements)) -> do
        query <- insertBodyMeasurements bodyMeasurements measurementTimestamp
        run (runInTransaction query)
          ("Error: An error occured while trying to record body measurements. " ++)
          (\_ -> Env.reply "success")
          env
      _ -> EntityIntf.defaultHandler args req env
