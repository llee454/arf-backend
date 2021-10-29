--- Defines the crud operations for the Anxiety Measurement data types.

module Anxiety where

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

--- Represents my anxiety measurement
--- @cons key - the entry key
--- @cons created - the date on which the measurement was recorded
--- @cons timestamp - the date on which the measurement was made
--- @cons measurer - the entity that made the measurement
--- @cons measured - the entity that was measured (an attribute entry)
--- @cons value - the value of the measurement
--- @cons unit - the units associated with the measurement (e.g. "lbs")
--- @cons precision - the error margin associated with the measurement (e.g. 2.0)
--- @cons symptoms - anxiety symptoms observed during the measurement (e.g. hyperventilation)
--- @cons interventions - actions taken to reduce anxiety (e.g. 100 mg. 5-HTP)
data AnxietyMeasurement = AnxietyMeasurement {
  key           :: Maybe Int,
  created       :: Int,
  timestamp     :: Int,
  measurer      :: Int,
  measured      :: Int,
  value         :: Float,
  unit          :: String,
  precision     :: Float
  symptoms      :: [String],
  interventions :: [String]
}

--
ofJSON :: JValue -> Maybe AnxietyMeasurement
ofJSON json =
  case json of
    (JObject [
      ("key",           k),
      ("created",       JNumber created),
      ("timestamp",     JNumber timestamp),
      ("measurer",      JNumber measurer),
      ("measured",      JNumber measured),
      ("value",         JNumber value),
      ("unit",          JString unit),
      ("precision",     JNumber precision),
      ("symptoms",      JArray symptoms),
      ("interventions", JArray interventions)]) ->
      maybeIntOfJSON k >>- \x ->
      mapMMaybe stringOfJSON symptoms >>- \ss ->
      mapMMaybe stringOfJSON interventions >>- \is ->
      Just $ AnxietyMeasurement x
        (truncate created)
        (truncate timestamp)
        (truncate measurer)
        (truncate measured)
        value unit precision ss is
    _ -> Nothing

--
toJSON :: AnxietyMeasurement -> JValue
toJSON (AnxietyMeasurement k created timestamp measurer measured value unit precision symptoms interventions)
  JObject [
    ("key",           maybeIntToJSON k),
    ("created",       JNumber $ i2f $ created),
    ("timestamp",     JNumber $ i2f $ timestamp),
    ("measurer",      JNumber $ i2f $ measurer),
    ("measured",      JNumber $ i2f $ measured),
    ("value",         JNumber value),
    ("unit",          JString unit),
    ("precision",     JNumber precision),
    ("symptoms",      JArray (map JString symptoms)),
    ("interventions", JArray (map JString interventions))]
        
--
insert :: AnxietyMeasurement -> DBAction AnxietyMeasurement
insert x@(AnxietyMeasurement k created timestamp measurer measured value unit precision symptoms interventions) 
  | isJust k = failDB $ DBError UnknownError "Error: An error occured while trying to insert an anxiety measurement into the database. Cannot insert an instantiated measurement."
  | otherwise =
    Measurement.insert (Measurement.Measurement Nothing created timestamp measurer measured value unit precision) >+=
    \(Measurement.Measurement (Just j) _ _ _ _ _ _ _) ->
     arf.


--- Initializes the database by defining my anxiety attribute.
init :: IO (DBAction ())
init = do
  currTime <- getCurrPosixTime
  return $
    Entity.getByNameErr Health.myName >+=
    \(Entity.Entity (Just k) _ _) ->
      Attribute.insert (Attribute.Attribute Nothing currTime "Health.MentalHealth.anxiety" k) >+
      (returnDB $ Right ())

insertAnxietyMeasurement
