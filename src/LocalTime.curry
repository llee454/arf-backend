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

--- Accepts a timezone description and transforms the given UTC time
--- into the local time for the timezone.
toTimeZone :: TimeZone -> DateTime -> DateTime
toTimeZone tz (DateTime d t) =
  let (n, u) = utcToLocalTimeOfDay tz t in
  DateTime (n + d) u

--- Accepts two arguments: path, a file path that references a binary
--- Olson timezone file (usually /etc/localtime); dateTime, the current
--- time; and returns a timezone.
getTimeZonePath :: String -> DateTime -> IO TimeZone
getTimeZonePath external

--- Accepts a date time and returns an equivalent time string in
--- ISO8601 format.
toISO8601 :: DateTime -> String
toISO8601 external

--- The default path to the Olson timezone file for the
--- America/New_York timezone.
newYorkTimeZonePath = "/usr/share/zoneinfo/America/New_York"

--- Accepts a Unit/Posix timestamp and returns a string giving the
--- equivalent local time in New York.
getNewYorkTime :: DateTime -> IO String
getNewYorkTime t = do
  tz <- getTimeZonePath newYorkTimeZonePath t
  return $ toISO8601 $ toTimeZone tz t

--- Returns the current Posix time.
getCurrPosixTime :: IO Int
getCurrPosixTime = getClockTime >>= return . clockTimeToInt

main :: IO ()
main = do
  t <- getCurrPosixTime >>= return . fromPosix
  lt <- getNewYorkTime t
  putStrLn $ "posix timestamp: " ++ show t
  putStrLn $ "Current Local Time (New York): " ++ lt
