--- This module defines auxiliary functions for converting to and from JSON.

module JSONExt where

import IO
import Float
import Time
import Maybe
import JSON.Data
import JSON.Parser
import JSON.Pretty

--- Accepts a string that represents a unix timestamp and attempts to convert
--- this into a clock timAccepts a string that represents a unix timestamp and
--- attempts to convert this into a clock time.
--- Note: this throws an exception when the conversion fails.
clockTimeOfString :: String -> ClockTime
clockTimeOfString s = read $ "(CTime " ++ s ++ ")"

--- Converts a JNumber value into a clocktime.
clockTimeOfNum :: Float -> ClockTime
clockTimeOfNum = clockTimeOfString . show . truncate

--- Converts a clocktime into a format that can be included in a JSON object.
clockTimeToNum :: ClockTime -> Float
clockTimeToNum = i2f . clockTimeToInt

--- Converts a clocktime into a JSON value.
clockTimeToJSON :: ClockTime -> JValue
clockTimeToJSON = JNumber . clockTimeToNum

---
bindMaybe :: b -> (a -> b) -> Maybe a -> b
bindMaybe def _ Nothing  = def
bindMaybe def f (Just x) = f x

---
maybeIntToJSON :: Maybe Int -> JValue
maybeIntToJSON = maybeToJSON (JNumber . i2f)

---
maybeIntOfJSON :: JValue -> Maybe (Maybe Int)
maybeIntOfJSON json =
  case json of
    JNumber n -> Just $ Just $ truncate n
    JNull     -> Just Nothing
    _         -> Nothing

---
maybeToJSON :: (a -> JValue) -> Maybe a -> JValue
maybeToJSON = bindMaybe JNull
