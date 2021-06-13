--- Interfaces with the Haskell Data.Time.LocalTime library to provide
--- functions for converting between local time and UTC.

module LocalTime where

import Time

data TimeZone = TimeZone {
  timeZoneMinutes :: Int,
  timeZoneSummerOnly :: Bool,
  timeZoneName :: String} deriving (Show)

--- Accepts a string such as "EST" and returns the corresponding time zone.
readTimeZone :: String -> TimeZone
readTimeZone external

data TimeOfDay = TimeOfDay {
  todHour :: Int,
  todMin :: Int,
  todSec :: Int} deriving (Show) -- Pico in Haskell, we convert to int using toInteger.

--- Convert a time of day in UTC to a time of day in some timezone,
--- together with a day adjustment.
utcToLocalTimeOfDay :: TimeZone -> TimeOfDay -> (Int, TimeOfDay) 
utcToLocalTimeOfDay external

data DateTime = DateTime {
  day  :: Int, -- Number of days since Nov. 17, 1858 
  time :: TimeOfDay} deriving (Show)

--- Accepts a Unix/Posix timestamp (the number of seconds since
--- 1970-01-01 00:00 UTC) and returns the corresponding UTC DateTime.
fromPosix :: Int -> DateTime
fromPosix external

toTimeZone :: TimeZone -> DateTime -> DateTime
toTimeZone tz (DateTime d t) =
  let (n, u) = utcToLocalTimeOfDay tz t in
  DateTime (n + d) u

--- Accepts a DateTime in UTC and returns an equivalent DateTime in EST.
toEST :: DateTime -> DateTime 
toEST = toTimeZone $ readTimeZone "EST"

--- Accepts a DateTime in UTC and returns an equivalent DateTime in EDT
--- (Eastern Daylight Time).
toEDT :: DateTime -> DateTime 
toEDT = toTimeZone $ readTimeZone "EDT"

--- Accepts two arguments: path, a file path that references a binary
--- Olson timezone file (usually /etc/localtime); dateTime, the current
--- time; and returns a timezone.
getTimeZonePath :: String -> DateTime -> TimeZone
getTimeZonePath external

getTimeZone :: DateTime -> TimeZone
getTimeZone = getTimeZonePath "/etc/localtime"

toLocalTime :: DateTime -> DateTime
toLocalTime d = toTimeZone (getTimeZone d) d

toISO8601 :: DateTime -> String
toISO8601 external

main :: IO ()
main = do
  ts <- getClockTime >>= return . clockTimeToInt
  putStrLn $ "posix timestamp: " ++ show (fromPosix ts)
  putStrLn $ "Current UTC Time: " ++ toISO8601 (fromPosix $ ts)
  putStrLn $ "Current EST Time: " ++ toISO8601 (toEST $ toLocalTime $ fromPosix ts)
