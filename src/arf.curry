--- This file has been generated from
--- 
---     /home/ubuntu/arf-backend/src/arf.erdterm
--- 
--- and contains definitions for all entities and relations
--- specified in this model.

module arf where

import qualified Time
import qualified Database.CDBI.ER
import qualified Database.CDBI.Criteria
import qualified Database.CDBI.Connection
import qualified Database.CDBI.Description

data Entry = Entry EntryID Time.ClockTime
 deriving (Eq,Show,Read)

data EntryID = EntryID Int
 deriving (Eq,Show,Read)

data Entity = Entity EntityID String EntryID
 deriving (Eq,Show,Read)

data EntityID = EntityID Int
 deriving (Eq,Show,Read)

data Event = Event EventID Time.ClockTime EntryID
 deriving (Eq,Show,Read)

data EventID = EventID Int
 deriving (Eq,Show,Read)

data Attrib = Attrib AttribID EntryID EntryID
 deriving (Eq,Show,Read)

data AttribID = AttribID Int
 deriving (Eq,Show,Read)

data Action = Action ActionID EntryID EntryID
 deriving (Eq,Show,Read)

data ActionID = ActionID Int
 deriving (Eq,Show,Read)

data Activity = Activity ActivityID Time.ClockTime EntryID
 deriving (Eq,Show,Read)

data ActivityID = ActivityID Int
 deriving (Eq,Show,Read)

data Measurement = Measurement MeasurementID String Float Float EntryID EntryID
 deriving (Eq,Show,Read)

data MeasurementID = MeasurementID Int
 deriving (Eq,Show,Read)

data Duration = Duration DurationID EntryID
 deriving (Eq,Show,Read)

data DurationID = DurationID Int
 deriving (Eq,Show,Read)

data Weight = Weight WeightID EntryID
 deriving (Eq,Show,Read)

data WeightID = WeightID Int
 deriving (Eq,Show,Read)

data Circumference = Circumference CircumferenceID EntryID
 deriving (Eq,Show,Read)

data CircumferenceID = CircumferenceID Int
 deriving (Eq,Show,Read)

--- The name of the SQLite database file.
sqliteDBFile :: String
sqliteDBFile = "data/Arf.sqlite"

--- The ER description of the `Entry` entity.
entry_CDBI_Description :: Database.CDBI.Description.EntityDescription Entry
entry_CDBI_Description =
  Database.CDBI.Description.ED "Entry"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeDate]
   (\(Entry (EntryID key) timestamp) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLDate timestamp])
   (\(Entry _ timestamp) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLDate timestamp])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLDate timestamp] ->
     Entry (EntryID key) timestamp)

--- The database table of the `Entry` entity.
entryTable :: Database.CDBI.Description.Table
entryTable = "Entry"

--- The database column `Key` of the `Entry` entity.
entryColumnKey :: Database.CDBI.Description.Column EntryID
entryColumnKey = Database.CDBI.Description.Column "\"Key\"" "\"Entry\".\"Key\""

--- The database column `Timestamp` of the `Entry` entity.
entryColumnTimestamp :: Database.CDBI.Description.Column Time.ClockTime
entryColumnTimestamp =
  Database.CDBI.Description.Column "\"Timestamp\"" "\"Entry\".\"Timestamp\""

--- The description of the database column `Key` of the `Entry` entity.
entryKeyColDesc :: Database.CDBI.Description.ColumnDescription EntryID
entryKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Entry\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntryID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> EntryID key)

--- The description of the database column `Timestamp` of the `Entry` entity.
entryTimestampColDesc
  :: Database.CDBI.Description.ColumnDescription Time.ClockTime
entryTimestampColDesc =
  Database.CDBI.Description.ColDesc "\"Entry\".\"Timestamp\""
   Database.CDBI.Connection.SQLTypeDate
   (\timestamp -> Database.CDBI.Connection.SQLDate timestamp)
   (\(Database.CDBI.Connection.SQLDate timestamp) -> timestamp)

--- Gets the attribute `Key` of the `Entry` entity.
entryKey :: Entry -> EntryID
entryKey (Entry a _) = a

--- Gets the attribute `Timestamp` of the `Entry` entity.
entryTimestamp :: Entry -> Time.ClockTime
entryTimestamp (Entry _ a) = a

--- Sets the attribute `Key` of the `Entry` entity.
setEntryKey :: Entry -> EntryID -> Entry
setEntryKey (Entry _ b1) a = Entry a b1

--- Sets the attribute `Timestamp` of the `Entry` entity.
setEntryTimestamp :: Entry -> Time.ClockTime -> Entry
setEntryTimestamp (Entry a2 _) a = Entry a2 a

--- id-to-value function for entity `Entry`.
entryID :: EntryID -> Database.CDBI.Criteria.Value EntryID
entryID (EntryID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Entry`.
entryKeyToInt :: EntryID -> Int
entryKeyToInt (EntryID key) = key

--- Shows the key of a `Entry` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showEntryKey :: Entry -> String
showEntryKey entry =
  Database.CDBI.ER.showDatabaseKey "Entry" entryKeyToInt (entryKey entry)

--- Transforms a string into a key of a `Entry` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readEntryKey :: String -> Maybe EntryID
readEntryKey = Database.CDBI.ER.readDatabaseKey "Entry" EntryID

--- Gets all `Entry` entities.
queryAllEntrys :: Database.CDBI.Connection.DBAction [Entry]
queryAllEntrys = Database.CDBI.ER.getAllEntries entry_CDBI_Description

--- Gets all `Entry` entities satisfying a given predicate.
queryCondEntry :: (Entry -> Bool) -> Database.CDBI.Connection.DBAction [Entry]
queryCondEntry = Database.CDBI.ER.getCondEntries entry_CDBI_Description

--- Gets a `Entry` entry by a given key.
getEntry :: EntryID -> Database.CDBI.Connection.DBAction Entry
getEntry =
  Database.CDBI.ER.getEntryWithKey entry_CDBI_Description entryColumnKey entryID

--- Inserts a new `Entry` entity.
newEntry :: Time.ClockTime -> Database.CDBI.Connection.DBAction Entry
newEntry timestamp_p =
  Database.CDBI.ER.insertNewEntry entry_CDBI_Description setEntryKey EntryID
   (Entry (EntryID 0) timestamp_p)

--- Deletes an existing `Entry` entry by its key.
deleteEntry :: Entry -> Database.CDBI.Connection.DBAction ()
deleteEntry =
  Database.CDBI.ER.deleteEntry entry_CDBI_Description entryColumnKey
   (entryID . entryKey)

--- Updates an existing `Entry` entry by its key.
updateEntry :: Entry -> Database.CDBI.Connection.DBAction ()
updateEntry = Database.CDBI.ER.updateEntry entry_CDBI_Description

--- The ER description of the `Entity` entity.
entity_CDBI_Description :: Database.CDBI.Description.EntityDescription Entity
entity_CDBI_Description =
  Database.CDBI.Description.ED "Entity"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(Entity (EntityID key) name (EntryID entryEntity_entryKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLInt entryEntity_entryKey])
   (\(Entity _ name (EntryID entryEntity_entryKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLInt entryEntity_entryKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLInt entryEntity_entryKey] ->
     Entity (EntityID key) name (EntryID entryEntity_entryKey))

--- The database table of the `Entity` entity.
entityTable :: Database.CDBI.Description.Table
entityTable = "Entity"

--- The database column `Key` of the `Entity` entity.
entityColumnKey :: Database.CDBI.Description.Column EntityID
entityColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Entity\".\"Key\""

--- The database column `Name` of the `Entity` entity.
entityColumnName :: Database.CDBI.Description.Column String
entityColumnName =
  Database.CDBI.Description.Column "\"Name\"" "\"Entity\".\"Name\""

--- The database column `EntryEntity_entryKey` of the `Entity` entity.
entityColumnEntryEntity_entryKey :: Database.CDBI.Description.Column EntryID
entityColumnEntryEntity_entryKey =
  Database.CDBI.Description.Column "\"EntryEntity_entryKey\""
   "\"Entity\".\"EntryEntity_entryKey\""

--- The description of the database column `Key` of the `Entity` entity.
entityKeyColDesc :: Database.CDBI.Description.ColumnDescription EntityID
entityKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Entity\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntityID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> EntityID key)

--- The description of the database column `Name` of the `Entity` entity.
entityNameColDesc :: Database.CDBI.Description.ColumnDescription String
entityNameColDesc =
  Database.CDBI.Description.ColDesc "\"Entity\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- The description of the database column `EntryEntity_entryKey` of the `Entity` entity.
entityEntryEntity_entryKeyColDesc
  :: Database.CDBI.Description.ColumnDescription EntryID
entityEntryEntity_entryKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Entity\".\"EntryEntity_entryKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntryID entryEntity_entryKey) ->
     Database.CDBI.Connection.SQLInt entryEntity_entryKey)
   (\(Database.CDBI.Connection.SQLInt entryEntity_entryKey) ->
     EntryID entryEntity_entryKey)

--- Gets the attribute `Key` of the `Entity` entity.
entityKey :: Entity -> EntityID
entityKey (Entity a _ _) = a

--- Gets the attribute `Name` of the `Entity` entity.
entityName :: Entity -> String
entityName (Entity _ a _) = a

--- Gets the attribute `EntryEntity_entryKey` of the `Entity` entity.
entityEntryEntity_entryKey :: Entity -> EntryID
entityEntryEntity_entryKey (Entity _ _ a) = a

--- Sets the attribute `Key` of the `Entity` entity.
setEntityKey :: Entity -> EntityID -> Entity
setEntityKey (Entity _ b2 b1) a = Entity a b2 b1

--- Sets the attribute `Name` of the `Entity` entity.
setEntityName :: Entity -> String -> Entity
setEntityName (Entity a2 _ b1) a = Entity a2 a b1

--- Sets the attribute `EntryEntity_entryKey` of the `Entity` entity.
setEntityEntryEntity_entryKey :: Entity -> EntryID -> Entity
setEntityEntryEntity_entryKey (Entity a3 a2 _) a = Entity a3 a2 a

--- id-to-value function for entity `Entity`.
entityID :: EntityID -> Database.CDBI.Criteria.Value EntityID
entityID (EntityID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Entity`.
entityKeyToInt :: EntityID -> Int
entityKeyToInt (EntityID key) = key

--- Shows the key of a `Entity` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showEntityKey :: Entity -> String
showEntityKey entry =
  Database.CDBI.ER.showDatabaseKey "Entity" entityKeyToInt (entityKey entry)

--- Transforms a string into a key of a `Entity` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readEntityKey :: String -> Maybe EntityID
readEntityKey = Database.CDBI.ER.readDatabaseKey "Entity" EntityID

--- Gets all `Entity` entities.
queryAllEntitys :: Database.CDBI.Connection.DBAction [Entity]
queryAllEntitys = Database.CDBI.ER.getAllEntries entity_CDBI_Description

--- Gets all `Entity` entities satisfying a given predicate.
queryCondEntity
  :: (Entity -> Bool) -> Database.CDBI.Connection.DBAction [Entity]
queryCondEntity = Database.CDBI.ER.getCondEntries entity_CDBI_Description

--- Gets a `Entity` entry by a given key.
getEntity :: EntityID -> Database.CDBI.Connection.DBAction Entity
getEntity =
  Database.CDBI.ER.getEntryWithKey entity_CDBI_Description entityColumnKey
   entityID

--- Inserts a new `Entity` entity.
newEntityWithEntryEntity_entryKey
  :: String -> EntryID -> Database.CDBI.Connection.DBAction Entity
newEntityWithEntryEntity_entryKey name_p entryEntity_entryKey_p =
  Database.CDBI.ER.insertNewEntry entity_CDBI_Description setEntityKey EntityID
   (Entity (EntityID 0) name_p entryEntity_entryKey_p)

--- Deletes an existing `Entity` entry by its key.
deleteEntity :: Entity -> Database.CDBI.Connection.DBAction ()
deleteEntity =
  Database.CDBI.ER.deleteEntry entity_CDBI_Description entityColumnKey
   (entityID . entityKey)

--- Updates an existing `Entity` entry by its key.
updateEntity :: Entity -> Database.CDBI.Connection.DBAction ()
updateEntity = Database.CDBI.ER.updateEntry entity_CDBI_Description

--- The ER description of the `Event` entity.
event_CDBI_Description :: Database.CDBI.Description.EntityDescription Event
event_CDBI_Description =
  Database.CDBI.Description.ED "Event"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeDate
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(Event (EventID key) timestamp (EntryID entryEvent_entryKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLDate timestamp
     ,Database.CDBI.Connection.SQLInt entryEvent_entryKey])
   (\(Event _ timestamp (EntryID entryEvent_entryKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLDate timestamp
     ,Database.CDBI.Connection.SQLInt entryEvent_entryKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLDate timestamp
     ,Database.CDBI.Connection.SQLInt entryEvent_entryKey] ->
     Event (EventID key) timestamp (EntryID entryEvent_entryKey))

--- The database table of the `Event` entity.
eventTable :: Database.CDBI.Description.Table
eventTable = "Event"

--- The database column `Key` of the `Event` entity.
eventColumnKey :: Database.CDBI.Description.Column EventID
eventColumnKey = Database.CDBI.Description.Column "\"Key\"" "\"Event\".\"Key\""

--- The database column `Timestamp` of the `Event` entity.
eventColumnTimestamp :: Database.CDBI.Description.Column Time.ClockTime
eventColumnTimestamp =
  Database.CDBI.Description.Column "\"Timestamp\"" "\"Event\".\"Timestamp\""

--- The database column `EntryEvent_entryKey` of the `Event` entity.
eventColumnEntryEvent_entryKey :: Database.CDBI.Description.Column EntryID
eventColumnEntryEvent_entryKey =
  Database.CDBI.Description.Column "\"EntryEvent_entryKey\""
   "\"Event\".\"EntryEvent_entryKey\""

--- The description of the database column `Key` of the `Event` entity.
eventKeyColDesc :: Database.CDBI.Description.ColumnDescription EventID
eventKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Event\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EventID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> EventID key)

--- The description of the database column `Timestamp` of the `Event` entity.
eventTimestampColDesc
  :: Database.CDBI.Description.ColumnDescription Time.ClockTime
eventTimestampColDesc =
  Database.CDBI.Description.ColDesc "\"Event\".\"Timestamp\""
   Database.CDBI.Connection.SQLTypeDate
   (\timestamp -> Database.CDBI.Connection.SQLDate timestamp)
   (\(Database.CDBI.Connection.SQLDate timestamp) -> timestamp)

--- The description of the database column `EntryEvent_entryKey` of the `Event` entity.
eventEntryEvent_entryKeyColDesc
  :: Database.CDBI.Description.ColumnDescription EntryID
eventEntryEvent_entryKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Event\".\"EntryEvent_entryKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntryID entryEvent_entryKey) ->
     Database.CDBI.Connection.SQLInt entryEvent_entryKey)
   (\(Database.CDBI.Connection.SQLInt entryEvent_entryKey) ->
     EntryID entryEvent_entryKey)

--- Gets the attribute `Key` of the `Event` entity.
eventKey :: Event -> EventID
eventKey (Event a _ _) = a

--- Gets the attribute `Timestamp` of the `Event` entity.
eventTimestamp :: Event -> Time.ClockTime
eventTimestamp (Event _ a _) = a

--- Gets the attribute `EntryEvent_entryKey` of the `Event` entity.
eventEntryEvent_entryKey :: Event -> EntryID
eventEntryEvent_entryKey (Event _ _ a) = a

--- Sets the attribute `Key` of the `Event` entity.
setEventKey :: Event -> EventID -> Event
setEventKey (Event _ b2 b1) a = Event a b2 b1

--- Sets the attribute `Timestamp` of the `Event` entity.
setEventTimestamp :: Event -> Time.ClockTime -> Event
setEventTimestamp (Event a2 _ b1) a = Event a2 a b1

--- Sets the attribute `EntryEvent_entryKey` of the `Event` entity.
setEventEntryEvent_entryKey :: Event -> EntryID -> Event
setEventEntryEvent_entryKey (Event a3 a2 _) a = Event a3 a2 a

--- id-to-value function for entity `Event`.
eventID :: EventID -> Database.CDBI.Criteria.Value EventID
eventID (EventID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Event`.
eventKeyToInt :: EventID -> Int
eventKeyToInt (EventID key) = key

--- Shows the key of a `Event` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showEventKey :: Event -> String
showEventKey entry =
  Database.CDBI.ER.showDatabaseKey "Event" eventKeyToInt (eventKey entry)

--- Transforms a string into a key of a `Event` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readEventKey :: String -> Maybe EventID
readEventKey = Database.CDBI.ER.readDatabaseKey "Event" EventID

--- Gets all `Event` entities.
queryAllEvents :: Database.CDBI.Connection.DBAction [Event]
queryAllEvents = Database.CDBI.ER.getAllEntries event_CDBI_Description

--- Gets all `Event` entities satisfying a given predicate.
queryCondEvent :: (Event -> Bool) -> Database.CDBI.Connection.DBAction [Event]
queryCondEvent = Database.CDBI.ER.getCondEntries event_CDBI_Description

--- Gets a `Event` entry by a given key.
getEvent :: EventID -> Database.CDBI.Connection.DBAction Event
getEvent =
  Database.CDBI.ER.getEntryWithKey event_CDBI_Description eventColumnKey eventID

--- Inserts a new `Event` entity.
newEventWithEntryEvent_entryKey
  :: Time.ClockTime -> EntryID -> Database.CDBI.Connection.DBAction Event
newEventWithEntryEvent_entryKey timestamp_p entryEvent_entryKey_p =
  Database.CDBI.ER.insertNewEntry event_CDBI_Description setEventKey EventID
   (Event (EventID 0) timestamp_p entryEvent_entryKey_p)

--- Deletes an existing `Event` entry by its key.
deleteEvent :: Event -> Database.CDBI.Connection.DBAction ()
deleteEvent =
  Database.CDBI.ER.deleteEntry event_CDBI_Description eventColumnKey
   (eventID . eventKey)

--- Updates an existing `Event` entry by its key.
updateEvent :: Event -> Database.CDBI.Connection.DBAction ()
updateEvent = Database.CDBI.ER.updateEntry event_CDBI_Description

--- The ER description of the `Attrib` entity.
attrib_CDBI_Description :: Database.CDBI.Description.EntityDescription Attrib
attrib_CDBI_Description =
  Database.CDBI.Description.ED "Attrib"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(Attrib
       (AttribID key)
       (EntryID entryAttrib_entryKey)
       (EntryID entryAttrib_subjectKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt entryAttrib_entryKey
     ,Database.CDBI.Connection.SQLInt entryAttrib_subjectKey])
   (\(Attrib
       _ (EntryID entryAttrib_entryKey) (EntryID entryAttrib_subjectKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLInt entryAttrib_entryKey
     ,Database.CDBI.Connection.SQLInt entryAttrib_subjectKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt entryAttrib_entryKey
     ,Database.CDBI.Connection.SQLInt entryAttrib_subjectKey] ->
     Attrib (AttribID key) (EntryID entryAttrib_entryKey)
      (EntryID entryAttrib_subjectKey))

--- The database table of the `Attrib` entity.
attribTable :: Database.CDBI.Description.Table
attribTable = "Attrib"

--- The database column `Key` of the `Attrib` entity.
attribColumnKey :: Database.CDBI.Description.Column AttribID
attribColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Attrib\".\"Key\""

--- The database column `EntryAttrib_entryKey` of the `Attrib` entity.
attribColumnEntryAttrib_entryKey :: Database.CDBI.Description.Column EntryID
attribColumnEntryAttrib_entryKey =
  Database.CDBI.Description.Column "\"EntryAttrib_entryKey\""
   "\"Attrib\".\"EntryAttrib_entryKey\""

--- The database column `EntryAttrib_subjectKey` of the `Attrib` entity.
attribColumnEntryAttrib_subjectKey :: Database.CDBI.Description.Column EntryID
attribColumnEntryAttrib_subjectKey =
  Database.CDBI.Description.Column "\"EntryAttrib_subjectKey\""
   "\"Attrib\".\"EntryAttrib_subjectKey\""

--- The description of the database column `Key` of the `Attrib` entity.
attribKeyColDesc :: Database.CDBI.Description.ColumnDescription AttribID
attribKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Attrib\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(AttribID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> AttribID key)

--- The description of the database column `EntryAttrib_entryKey` of the `Attrib` entity.
attribEntryAttrib_entryKeyColDesc
  :: Database.CDBI.Description.ColumnDescription EntryID
attribEntryAttrib_entryKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Attrib\".\"EntryAttrib_entryKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntryID entryAttrib_entryKey) ->
     Database.CDBI.Connection.SQLInt entryAttrib_entryKey)
   (\(Database.CDBI.Connection.SQLInt entryAttrib_entryKey) ->
     EntryID entryAttrib_entryKey)

--- The description of the database column `EntryAttrib_subjectKey` of the `Attrib` entity.
attribEntryAttrib_subjectKeyColDesc
  :: Database.CDBI.Description.ColumnDescription EntryID
attribEntryAttrib_subjectKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Attrib\".\"EntryAttrib_subjectKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntryID entryAttrib_subjectKey) ->
     Database.CDBI.Connection.SQLInt entryAttrib_subjectKey)
   (\(Database.CDBI.Connection.SQLInt entryAttrib_subjectKey) ->
     EntryID entryAttrib_subjectKey)

--- Gets the attribute `Key` of the `Attrib` entity.
attribKey :: Attrib -> AttribID
attribKey (Attrib a _ _) = a

--- Gets the attribute `EntryAttrib_entryKey` of the `Attrib` entity.
attribEntryAttrib_entryKey :: Attrib -> EntryID
attribEntryAttrib_entryKey (Attrib _ a _) = a

--- Gets the attribute `EntryAttrib_subjectKey` of the `Attrib` entity.
attribEntryAttrib_subjectKey :: Attrib -> EntryID
attribEntryAttrib_subjectKey (Attrib _ _ a) = a

--- Sets the attribute `Key` of the `Attrib` entity.
setAttribKey :: Attrib -> AttribID -> Attrib
setAttribKey (Attrib _ b2 b1) a = Attrib a b2 b1

--- Sets the attribute `EntryAttrib_entryKey` of the `Attrib` entity.
setAttribEntryAttrib_entryKey :: Attrib -> EntryID -> Attrib
setAttribEntryAttrib_entryKey (Attrib a2 _ b1) a = Attrib a2 a b1

--- Sets the attribute `EntryAttrib_subjectKey` of the `Attrib` entity.
setAttribEntryAttrib_subjectKey :: Attrib -> EntryID -> Attrib
setAttribEntryAttrib_subjectKey (Attrib a3 a2 _) a = Attrib a3 a2 a

--- id-to-value function for entity `Attrib`.
attribID :: AttribID -> Database.CDBI.Criteria.Value AttribID
attribID (AttribID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Attrib`.
attribKeyToInt :: AttribID -> Int
attribKeyToInt (AttribID key) = key

--- Shows the key of a `Attrib` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showAttribKey :: Attrib -> String
showAttribKey entry =
  Database.CDBI.ER.showDatabaseKey "Attrib" attribKeyToInt (attribKey entry)

--- Transforms a string into a key of a `Attrib` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readAttribKey :: String -> Maybe AttribID
readAttribKey = Database.CDBI.ER.readDatabaseKey "Attrib" AttribID

--- Gets all `Attrib` entities.
queryAllAttribs :: Database.CDBI.Connection.DBAction [Attrib]
queryAllAttribs = Database.CDBI.ER.getAllEntries attrib_CDBI_Description

--- Gets all `Attrib` entities satisfying a given predicate.
queryCondAttrib
  :: (Attrib -> Bool) -> Database.CDBI.Connection.DBAction [Attrib]
queryCondAttrib = Database.CDBI.ER.getCondEntries attrib_CDBI_Description

--- Gets a `Attrib` entry by a given key.
getAttrib :: AttribID -> Database.CDBI.Connection.DBAction Attrib
getAttrib =
  Database.CDBI.ER.getEntryWithKey attrib_CDBI_Description attribColumnKey
   attribID

--- Inserts a new `Attrib` entity.
newAttribWithEntryAttrib_entryKeyWithEntryAttrib_subjectKey
  :: EntryID -> EntryID -> Database.CDBI.Connection.DBAction Attrib
newAttribWithEntryAttrib_entryKeyWithEntryAttrib_subjectKey
    entryAttrib_entryKey_p entryAttrib_subjectKey_p =
  Database.CDBI.ER.insertNewEntry attrib_CDBI_Description setAttribKey AttribID
   (Attrib (AttribID 0) entryAttrib_entryKey_p entryAttrib_subjectKey_p)

--- Deletes an existing `Attrib` entry by its key.
deleteAttrib :: Attrib -> Database.CDBI.Connection.DBAction ()
deleteAttrib =
  Database.CDBI.ER.deleteEntry attrib_CDBI_Description attribColumnKey
   (attribID . attribKey)

--- Updates an existing `Attrib` entry by its key.
updateAttrib :: Attrib -> Database.CDBI.Connection.DBAction ()
updateAttrib = Database.CDBI.ER.updateEntry attrib_CDBI_Description

--- The ER description of the `Action` entity.
action_CDBI_Description :: Database.CDBI.Description.EntityDescription Action
action_CDBI_Description =
  Database.CDBI.Description.ED "Action"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(Action
       (ActionID key)
       (EntryID entryAction_entryKey)
       (EntryID entryAction_subjectKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt entryAction_entryKey
     ,Database.CDBI.Connection.SQLInt entryAction_subjectKey])
   (\(Action
       _ (EntryID entryAction_entryKey) (EntryID entryAction_subjectKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLInt entryAction_entryKey
     ,Database.CDBI.Connection.SQLInt entryAction_subjectKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt entryAction_entryKey
     ,Database.CDBI.Connection.SQLInt entryAction_subjectKey] ->
     Action (ActionID key) (EntryID entryAction_entryKey)
      (EntryID entryAction_subjectKey))

--- The database table of the `Action` entity.
actionTable :: Database.CDBI.Description.Table
actionTable = "Action"

--- The database column `Key` of the `Action` entity.
actionColumnKey :: Database.CDBI.Description.Column ActionID
actionColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Action\".\"Key\""

--- The database column `EntryAction_entryKey` of the `Action` entity.
actionColumnEntryAction_entryKey :: Database.CDBI.Description.Column EntryID
actionColumnEntryAction_entryKey =
  Database.CDBI.Description.Column "\"EntryAction_entryKey\""
   "\"Action\".\"EntryAction_entryKey\""

--- The database column `EntryAction_subjectKey` of the `Action` entity.
actionColumnEntryAction_subjectKey :: Database.CDBI.Description.Column EntryID
actionColumnEntryAction_subjectKey =
  Database.CDBI.Description.Column "\"EntryAction_subjectKey\""
   "\"Action\".\"EntryAction_subjectKey\""

--- The description of the database column `Key` of the `Action` entity.
actionKeyColDesc :: Database.CDBI.Description.ColumnDescription ActionID
actionKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Action\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(ActionID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> ActionID key)

--- The description of the database column `EntryAction_entryKey` of the `Action` entity.
actionEntryAction_entryKeyColDesc
  :: Database.CDBI.Description.ColumnDescription EntryID
actionEntryAction_entryKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Action\".\"EntryAction_entryKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntryID entryAction_entryKey) ->
     Database.CDBI.Connection.SQLInt entryAction_entryKey)
   (\(Database.CDBI.Connection.SQLInt entryAction_entryKey) ->
     EntryID entryAction_entryKey)

--- The description of the database column `EntryAction_subjectKey` of the `Action` entity.
actionEntryAction_subjectKeyColDesc
  :: Database.CDBI.Description.ColumnDescription EntryID
actionEntryAction_subjectKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Action\".\"EntryAction_subjectKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntryID entryAction_subjectKey) ->
     Database.CDBI.Connection.SQLInt entryAction_subjectKey)
   (\(Database.CDBI.Connection.SQLInt entryAction_subjectKey) ->
     EntryID entryAction_subjectKey)

--- Gets the attribute `Key` of the `Action` entity.
actionKey :: Action -> ActionID
actionKey (Action a _ _) = a

--- Gets the attribute `EntryAction_entryKey` of the `Action` entity.
actionEntryAction_entryKey :: Action -> EntryID
actionEntryAction_entryKey (Action _ a _) = a

--- Gets the attribute `EntryAction_subjectKey` of the `Action` entity.
actionEntryAction_subjectKey :: Action -> EntryID
actionEntryAction_subjectKey (Action _ _ a) = a

--- Sets the attribute `Key` of the `Action` entity.
setActionKey :: Action -> ActionID -> Action
setActionKey (Action _ b2 b1) a = Action a b2 b1

--- Sets the attribute `EntryAction_entryKey` of the `Action` entity.
setActionEntryAction_entryKey :: Action -> EntryID -> Action
setActionEntryAction_entryKey (Action a2 _ b1) a = Action a2 a b1

--- Sets the attribute `EntryAction_subjectKey` of the `Action` entity.
setActionEntryAction_subjectKey :: Action -> EntryID -> Action
setActionEntryAction_subjectKey (Action a3 a2 _) a = Action a3 a2 a

--- id-to-value function for entity `Action`.
actionID :: ActionID -> Database.CDBI.Criteria.Value ActionID
actionID (ActionID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Action`.
actionKeyToInt :: ActionID -> Int
actionKeyToInt (ActionID key) = key

--- Shows the key of a `Action` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showActionKey :: Action -> String
showActionKey entry =
  Database.CDBI.ER.showDatabaseKey "Action" actionKeyToInt (actionKey entry)

--- Transforms a string into a key of a `Action` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readActionKey :: String -> Maybe ActionID
readActionKey = Database.CDBI.ER.readDatabaseKey "Action" ActionID

--- Gets all `Action` entities.
queryAllActions :: Database.CDBI.Connection.DBAction [Action]
queryAllActions = Database.CDBI.ER.getAllEntries action_CDBI_Description

--- Gets all `Action` entities satisfying a given predicate.
queryCondAction
  :: (Action -> Bool) -> Database.CDBI.Connection.DBAction [Action]
queryCondAction = Database.CDBI.ER.getCondEntries action_CDBI_Description

--- Gets a `Action` entry by a given key.
getAction :: ActionID -> Database.CDBI.Connection.DBAction Action
getAction =
  Database.CDBI.ER.getEntryWithKey action_CDBI_Description actionColumnKey
   actionID

--- Inserts a new `Action` entity.
newActionWithEntryAction_entryKeyWithEntryAction_subjectKey
  :: EntryID -> EntryID -> Database.CDBI.Connection.DBAction Action
newActionWithEntryAction_entryKeyWithEntryAction_subjectKey
    entryAction_entryKey_p entryAction_subjectKey_p =
  Database.CDBI.ER.insertNewEntry action_CDBI_Description setActionKey ActionID
   (Action (ActionID 0) entryAction_entryKey_p entryAction_subjectKey_p)

--- Deletes an existing `Action` entry by its key.
deleteAction :: Action -> Database.CDBI.Connection.DBAction ()
deleteAction =
  Database.CDBI.ER.deleteEntry action_CDBI_Description actionColumnKey
   (actionID . actionKey)

--- Updates an existing `Action` entry by its key.
updateAction :: Action -> Database.CDBI.Connection.DBAction ()
updateAction = Database.CDBI.ER.updateEntry action_CDBI_Description

--- The ER description of the `Activity` entity.
activity_CDBI_Description
  :: Database.CDBI.Description.EntityDescription Activity
activity_CDBI_Description =
  Database.CDBI.Description.ED "Activity"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeDate
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(Activity (ActivityID key) duration (EntryID entryActivity_entryKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLDate duration
     ,Database.CDBI.Connection.SQLInt entryActivity_entryKey])
   (\(Activity _ duration (EntryID entryActivity_entryKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLDate duration
     ,Database.CDBI.Connection.SQLInt entryActivity_entryKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLDate duration
     ,Database.CDBI.Connection.SQLInt entryActivity_entryKey] ->
     Activity (ActivityID key) duration (EntryID entryActivity_entryKey))

--- The database table of the `Activity` entity.
activityTable :: Database.CDBI.Description.Table
activityTable = "Activity"

--- The database column `Key` of the `Activity` entity.
activityColumnKey :: Database.CDBI.Description.Column ActivityID
activityColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Activity\".\"Key\""

--- The database column `Duration` of the `Activity` entity.
activityColumnDuration :: Database.CDBI.Description.Column Time.ClockTime
activityColumnDuration =
  Database.CDBI.Description.Column "\"Duration\"" "\"Activity\".\"Duration\""

--- The database column `EntryActivity_entryKey` of the `Activity` entity.
activityColumnEntryActivity_entryKey :: Database.CDBI.Description.Column EntryID
activityColumnEntryActivity_entryKey =
  Database.CDBI.Description.Column "\"EntryActivity_entryKey\""
   "\"Activity\".\"EntryActivity_entryKey\""

--- The description of the database column `Key` of the `Activity` entity.
activityKeyColDesc :: Database.CDBI.Description.ColumnDescription ActivityID
activityKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Activity\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(ActivityID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> ActivityID key)

--- The description of the database column `Duration` of the `Activity` entity.
activityDurationColDesc
  :: Database.CDBI.Description.ColumnDescription Time.ClockTime
activityDurationColDesc =
  Database.CDBI.Description.ColDesc "\"Activity\".\"Duration\""
   Database.CDBI.Connection.SQLTypeDate
   (\duration -> Database.CDBI.Connection.SQLDate duration)
   (\(Database.CDBI.Connection.SQLDate duration) -> duration)

--- The description of the database column `EntryActivity_entryKey` of the `Activity` entity.
activityEntryActivity_entryKeyColDesc
  :: Database.CDBI.Description.ColumnDescription EntryID
activityEntryActivity_entryKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Activity\".\"EntryActivity_entryKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntryID entryActivity_entryKey) ->
     Database.CDBI.Connection.SQLInt entryActivity_entryKey)
   (\(Database.CDBI.Connection.SQLInt entryActivity_entryKey) ->
     EntryID entryActivity_entryKey)

--- Gets the attribute `Key` of the `Activity` entity.
activityKey :: Activity -> ActivityID
activityKey (Activity a _ _) = a

--- Gets the attribute `Duration` of the `Activity` entity.
activityDuration :: Activity -> Time.ClockTime
activityDuration (Activity _ a _) = a

--- Gets the attribute `EntryActivity_entryKey` of the `Activity` entity.
activityEntryActivity_entryKey :: Activity -> EntryID
activityEntryActivity_entryKey (Activity _ _ a) = a

--- Sets the attribute `Key` of the `Activity` entity.
setActivityKey :: Activity -> ActivityID -> Activity
setActivityKey (Activity _ b2 b1) a = Activity a b2 b1

--- Sets the attribute `Duration` of the `Activity` entity.
setActivityDuration :: Activity -> Time.ClockTime -> Activity
setActivityDuration (Activity a2 _ b1) a = Activity a2 a b1

--- Sets the attribute `EntryActivity_entryKey` of the `Activity` entity.
setActivityEntryActivity_entryKey :: Activity -> EntryID -> Activity
setActivityEntryActivity_entryKey (Activity a3 a2 _) a = Activity a3 a2 a

--- id-to-value function for entity `Activity`.
activityID :: ActivityID -> Database.CDBI.Criteria.Value ActivityID
activityID (ActivityID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Activity`.
activityKeyToInt :: ActivityID -> Int
activityKeyToInt (ActivityID key) = key

--- Shows the key of a `Activity` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showActivityKey :: Activity -> String
showActivityKey entry =
  Database.CDBI.ER.showDatabaseKey "Activity" activityKeyToInt
   (activityKey entry)

--- Transforms a string into a key of a `Activity` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readActivityKey :: String -> Maybe ActivityID
readActivityKey = Database.CDBI.ER.readDatabaseKey "Activity" ActivityID

--- Gets all `Activity` entities.
queryAllActivitys :: Database.CDBI.Connection.DBAction [Activity]
queryAllActivitys = Database.CDBI.ER.getAllEntries activity_CDBI_Description

--- Gets all `Activity` entities satisfying a given predicate.
queryCondActivity
  :: (Activity -> Bool) -> Database.CDBI.Connection.DBAction [Activity]
queryCondActivity = Database.CDBI.ER.getCondEntries activity_CDBI_Description

--- Gets a `Activity` entry by a given key.
getActivity :: ActivityID -> Database.CDBI.Connection.DBAction Activity
getActivity =
  Database.CDBI.ER.getEntryWithKey activity_CDBI_Description activityColumnKey
   activityID

--- Inserts a new `Activity` entity.
newActivityWithEntryActivity_entryKey
  :: Time.ClockTime -> EntryID -> Database.CDBI.Connection.DBAction Activity
newActivityWithEntryActivity_entryKey duration_p entryActivity_entryKey_p =
  Database.CDBI.ER.insertNewEntry activity_CDBI_Description setActivityKey
   ActivityID
   (Activity (ActivityID 0) duration_p entryActivity_entryKey_p)

--- Deletes an existing `Activity` entry by its key.
deleteActivity :: Activity -> Database.CDBI.Connection.DBAction ()
deleteActivity =
  Database.CDBI.ER.deleteEntry activity_CDBI_Description activityColumnKey
   (activityID . activityKey)

--- Updates an existing `Activity` entry by its key.
updateActivity :: Activity -> Database.CDBI.Connection.DBAction ()
updateActivity = Database.CDBI.ER.updateEntry activity_CDBI_Description

--- The ER description of the `Measurement` entity.
measurement_CDBI_Description
  :: Database.CDBI.Description.EntityDescription Measurement
measurement_CDBI_Description =
  Database.CDBI.Description.ED "Measurement"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeFloat
   ,Database.CDBI.Connection.SQLTypeFloat
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(Measurement
       (MeasurementID key)
       unit
       value
       precision
       (EntryID entryMeasurement_entryKey)
       (EntryID entryMeasurement_ofKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString unit
     ,Database.CDBI.Connection.SQLFloat value
     ,Database.CDBI.Connection.SQLFloat precision
     ,Database.CDBI.Connection.SQLInt entryMeasurement_entryKey
     ,Database.CDBI.Connection.SQLInt entryMeasurement_ofKey])
   (\(Measurement
       _
       unit
       value
       precision
       (EntryID entryMeasurement_entryKey)
       (EntryID entryMeasurement_ofKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString unit
     ,Database.CDBI.Connection.SQLFloat value
     ,Database.CDBI.Connection.SQLFloat precision
     ,Database.CDBI.Connection.SQLInt entryMeasurement_entryKey
     ,Database.CDBI.Connection.SQLInt entryMeasurement_ofKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString unit
     ,Database.CDBI.Connection.SQLFloat value
     ,Database.CDBI.Connection.SQLFloat precision
     ,Database.CDBI.Connection.SQLInt entryMeasurement_entryKey
     ,Database.CDBI.Connection.SQLInt entryMeasurement_ofKey] ->
     Measurement (MeasurementID key) unit value precision
      (EntryID entryMeasurement_entryKey)
      (EntryID entryMeasurement_ofKey))

--- The database table of the `Measurement` entity.
measurementTable :: Database.CDBI.Description.Table
measurementTable = "Measurement"

--- The database column `Key` of the `Measurement` entity.
measurementColumnKey :: Database.CDBI.Description.Column MeasurementID
measurementColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Measurement\".\"Key\""

--- The database column `Unit` of the `Measurement` entity.
measurementColumnUnit :: Database.CDBI.Description.Column String
measurementColumnUnit =
  Database.CDBI.Description.Column "\"Unit\"" "\"Measurement\".\"Unit\""

--- The database column `Value` of the `Measurement` entity.
measurementColumnValue :: Database.CDBI.Description.Column Float
measurementColumnValue =
  Database.CDBI.Description.Column "\"Value\"" "\"Measurement\".\"Value\""

--- The database column `Precision` of the `Measurement` entity.
measurementColumnPrecision :: Database.CDBI.Description.Column Float
measurementColumnPrecision =
  Database.CDBI.Description.Column "\"Precision\""
   "\"Measurement\".\"Precision\""

--- The database column `EntryMeasurement_entryKey` of the `Measurement` entity.
measurementColumnEntryMeasurement_entryKey
  :: Database.CDBI.Description.Column EntryID
measurementColumnEntryMeasurement_entryKey =
  Database.CDBI.Description.Column "\"EntryMeasurement_entryKey\""
   "\"Measurement\".\"EntryMeasurement_entryKey\""

--- The database column `EntryMeasurement_ofKey` of the `Measurement` entity.
measurementColumnEntryMeasurement_ofKey
  :: Database.CDBI.Description.Column EntryID
measurementColumnEntryMeasurement_ofKey =
  Database.CDBI.Description.Column "\"EntryMeasurement_ofKey\""
   "\"Measurement\".\"EntryMeasurement_ofKey\""

--- The description of the database column `Key` of the `Measurement` entity.
measurementKeyColDesc
  :: Database.CDBI.Description.ColumnDescription MeasurementID
measurementKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Measurement\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(MeasurementID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> MeasurementID key)

--- The description of the database column `Unit` of the `Measurement` entity.
measurementUnitColDesc :: Database.CDBI.Description.ColumnDescription String
measurementUnitColDesc =
  Database.CDBI.Description.ColDesc "\"Measurement\".\"Unit\""
   Database.CDBI.Connection.SQLTypeString
   (\unit -> Database.CDBI.Connection.SQLString unit)
   (\(Database.CDBI.Connection.SQLString unit) -> unit)

--- The description of the database column `Value` of the `Measurement` entity.
measurementValueColDesc :: Database.CDBI.Description.ColumnDescription Float
measurementValueColDesc =
  Database.CDBI.Description.ColDesc "\"Measurement\".\"Value\""
   Database.CDBI.Connection.SQLTypeFloat
   (\value -> Database.CDBI.Connection.SQLFloat value)
   (\(Database.CDBI.Connection.SQLFloat value) -> value)

--- The description of the database column `Precision` of the `Measurement` entity.
measurementPrecisionColDesc :: Database.CDBI.Description.ColumnDescription Float
measurementPrecisionColDesc =
  Database.CDBI.Description.ColDesc "\"Measurement\".\"Precision\""
   Database.CDBI.Connection.SQLTypeFloat
   (\precision -> Database.CDBI.Connection.SQLFloat precision)
   (\(Database.CDBI.Connection.SQLFloat precision) -> precision)

--- The description of the database column `EntryMeasurement_entryKey` of the `Measurement` entity.
measurementEntryMeasurement_entryKeyColDesc
  :: Database.CDBI.Description.ColumnDescription EntryID
measurementEntryMeasurement_entryKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"Measurement\".\"EntryMeasurement_entryKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntryID entryMeasurement_entryKey) ->
     Database.CDBI.Connection.SQLInt entryMeasurement_entryKey)
   (\(Database.CDBI.Connection.SQLInt entryMeasurement_entryKey) ->
     EntryID entryMeasurement_entryKey)

--- The description of the database column `EntryMeasurement_ofKey` of the `Measurement` entity.
measurementEntryMeasurement_ofKeyColDesc
  :: Database.CDBI.Description.ColumnDescription EntryID
measurementEntryMeasurement_ofKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Measurement\".\"EntryMeasurement_ofKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntryID entryMeasurement_ofKey) ->
     Database.CDBI.Connection.SQLInt entryMeasurement_ofKey)
   (\(Database.CDBI.Connection.SQLInt entryMeasurement_ofKey) ->
     EntryID entryMeasurement_ofKey)

--- Gets the attribute `Key` of the `Measurement` entity.
measurementKey :: Measurement -> MeasurementID
measurementKey (Measurement a _ _ _ _ _) = a

--- Gets the attribute `Unit` of the `Measurement` entity.
measurementUnit :: Measurement -> String
measurementUnit (Measurement _ a _ _ _ _) = a

--- Gets the attribute `Value` of the `Measurement` entity.
measurementValue :: Measurement -> Float
measurementValue (Measurement _ _ a _ _ _) = a

--- Gets the attribute `Precision` of the `Measurement` entity.
measurementPrecision :: Measurement -> Float
measurementPrecision (Measurement _ _ _ a _ _) = a

--- Gets the attribute `EntryMeasurement_entryKey` of the `Measurement` entity.
measurementEntryMeasurement_entryKey :: Measurement -> EntryID
measurementEntryMeasurement_entryKey (Measurement _ _ _ _ a _) = a

--- Gets the attribute `EntryMeasurement_ofKey` of the `Measurement` entity.
measurementEntryMeasurement_ofKey :: Measurement -> EntryID
measurementEntryMeasurement_ofKey (Measurement _ _ _ _ _ a) = a

--- Sets the attribute `Key` of the `Measurement` entity.
setMeasurementKey :: Measurement -> MeasurementID -> Measurement
setMeasurementKey (Measurement _ b5 b4 b3 b2 b1) a =
  Measurement a b5 b4 b3 b2 b1

--- Sets the attribute `Unit` of the `Measurement` entity.
setMeasurementUnit :: Measurement -> String -> Measurement
setMeasurementUnit (Measurement a2 _ b4 b3 b2 b1) a =
  Measurement a2 a b4 b3 b2 b1

--- Sets the attribute `Value` of the `Measurement` entity.
setMeasurementValue :: Measurement -> Float -> Measurement
setMeasurementValue (Measurement a3 a2 _ b3 b2 b1) a =
  Measurement a3 a2 a b3 b2 b1

--- Sets the attribute `Precision` of the `Measurement` entity.
setMeasurementPrecision :: Measurement -> Float -> Measurement
setMeasurementPrecision (Measurement a4 a3 a2 _ b2 b1) a =
  Measurement a4 a3 a2 a b2 b1

--- Sets the attribute `EntryMeasurement_entryKey` of the `Measurement` entity.
setMeasurementEntryMeasurement_entryKey :: Measurement -> EntryID -> Measurement
setMeasurementEntryMeasurement_entryKey (Measurement a5 a4 a3 a2 _ b1) a =
  Measurement a5 a4 a3 a2 a b1

--- Sets the attribute `EntryMeasurement_ofKey` of the `Measurement` entity.
setMeasurementEntryMeasurement_ofKey :: Measurement -> EntryID -> Measurement
setMeasurementEntryMeasurement_ofKey (Measurement a6 a5 a4 a3 a2 _) a =
  Measurement a6 a5 a4 a3 a2 a

--- id-to-value function for entity `Measurement`.
measurementID :: MeasurementID -> Database.CDBI.Criteria.Value MeasurementID
measurementID (MeasurementID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Measurement`.
measurementKeyToInt :: MeasurementID -> Int
measurementKeyToInt (MeasurementID key) = key

--- Shows the key of a `Measurement` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showMeasurementKey :: Measurement -> String
showMeasurementKey entry =
  Database.CDBI.ER.showDatabaseKey "Measurement" measurementKeyToInt
   (measurementKey entry)

--- Transforms a string into a key of a `Measurement` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readMeasurementKey :: String -> Maybe MeasurementID
readMeasurementKey =
  Database.CDBI.ER.readDatabaseKey "Measurement" MeasurementID

--- Gets all `Measurement` entities.
queryAllMeasurements :: Database.CDBI.Connection.DBAction [Measurement]
queryAllMeasurements =
  Database.CDBI.ER.getAllEntries measurement_CDBI_Description

--- Gets all `Measurement` entities satisfying a given predicate.
queryCondMeasurement
  :: (Measurement -> Bool) -> Database.CDBI.Connection.DBAction [Measurement]
queryCondMeasurement =
  Database.CDBI.ER.getCondEntries measurement_CDBI_Description

--- Gets a `Measurement` entry by a given key.
getMeasurement :: MeasurementID -> Database.CDBI.Connection.DBAction Measurement
getMeasurement =
  Database.CDBI.ER.getEntryWithKey measurement_CDBI_Description
   measurementColumnKey
   measurementID

--- Inserts a new `Measurement` entity.
newMeasurementWithEntryMeasurement_entryKeyWithEntryMeasurement_ofKey
  :: String
  -> Float
  -> Float
  -> EntryID -> EntryID -> Database.CDBI.Connection.DBAction Measurement
newMeasurementWithEntryMeasurement_entryKeyWithEntryMeasurement_ofKey
    unit_p
    value_p
    precision_p
    entryMeasurement_entryKey_p
    entryMeasurement_ofKey_p =
  Database.CDBI.ER.insertNewEntry measurement_CDBI_Description setMeasurementKey
   MeasurementID
   (Measurement (MeasurementID 0) unit_p value_p precision_p
     entryMeasurement_entryKey_p
     entryMeasurement_ofKey_p)

--- Deletes an existing `Measurement` entry by its key.
deleteMeasurement :: Measurement -> Database.CDBI.Connection.DBAction ()
deleteMeasurement =
  Database.CDBI.ER.deleteEntry measurement_CDBI_Description measurementColumnKey
   (measurementID . measurementKey)

--- Updates an existing `Measurement` entry by its key.
updateMeasurement :: Measurement -> Database.CDBI.Connection.DBAction ()
updateMeasurement = Database.CDBI.ER.updateEntry measurement_CDBI_Description

--- The ER description of the `Duration` entity.
duration_CDBI_Description
  :: Database.CDBI.Description.EntityDescription Duration
duration_CDBI_Description =
  Database.CDBI.Description.ED "Duration"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeInt]
   (\(Duration (DurationID key) (EntryID entryDuration_entryKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt entryDuration_entryKey])
   (\(Duration _ (EntryID entryDuration_entryKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLInt entryDuration_entryKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt entryDuration_entryKey] ->
     Duration (DurationID key) (EntryID entryDuration_entryKey))

--- The database table of the `Duration` entity.
durationTable :: Database.CDBI.Description.Table
durationTable = "Duration"

--- The database column `Key` of the `Duration` entity.
durationColumnKey :: Database.CDBI.Description.Column DurationID
durationColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Duration\".\"Key\""

--- The database column `EntryDuration_entryKey` of the `Duration` entity.
durationColumnEntryDuration_entryKey :: Database.CDBI.Description.Column EntryID
durationColumnEntryDuration_entryKey =
  Database.CDBI.Description.Column "\"EntryDuration_entryKey\""
   "\"Duration\".\"EntryDuration_entryKey\""

--- The description of the database column `Key` of the `Duration` entity.
durationKeyColDesc :: Database.CDBI.Description.ColumnDescription DurationID
durationKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Duration\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(DurationID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> DurationID key)

--- The description of the database column `EntryDuration_entryKey` of the `Duration` entity.
durationEntryDuration_entryKeyColDesc
  :: Database.CDBI.Description.ColumnDescription EntryID
durationEntryDuration_entryKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Duration\".\"EntryDuration_entryKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntryID entryDuration_entryKey) ->
     Database.CDBI.Connection.SQLInt entryDuration_entryKey)
   (\(Database.CDBI.Connection.SQLInt entryDuration_entryKey) ->
     EntryID entryDuration_entryKey)

--- Gets the attribute `Key` of the `Duration` entity.
durationKey :: Duration -> DurationID
durationKey (Duration a _) = a

--- Gets the attribute `EntryDuration_entryKey` of the `Duration` entity.
durationEntryDuration_entryKey :: Duration -> EntryID
durationEntryDuration_entryKey (Duration _ a) = a

--- Sets the attribute `Key` of the `Duration` entity.
setDurationKey :: Duration -> DurationID -> Duration
setDurationKey (Duration _ b1) a = Duration a b1

--- Sets the attribute `EntryDuration_entryKey` of the `Duration` entity.
setDurationEntryDuration_entryKey :: Duration -> EntryID -> Duration
setDurationEntryDuration_entryKey (Duration a2 _) a = Duration a2 a

--- id-to-value function for entity `Duration`.
durationID :: DurationID -> Database.CDBI.Criteria.Value DurationID
durationID (DurationID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Duration`.
durationKeyToInt :: DurationID -> Int
durationKeyToInt (DurationID key) = key

--- Shows the key of a `Duration` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showDurationKey :: Duration -> String
showDurationKey entry =
  Database.CDBI.ER.showDatabaseKey "Duration" durationKeyToInt
   (durationKey entry)

--- Transforms a string into a key of a `Duration` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readDurationKey :: String -> Maybe DurationID
readDurationKey = Database.CDBI.ER.readDatabaseKey "Duration" DurationID

--- Gets all `Duration` entities.
queryAllDurations :: Database.CDBI.Connection.DBAction [Duration]
queryAllDurations = Database.CDBI.ER.getAllEntries duration_CDBI_Description

--- Gets all `Duration` entities satisfying a given predicate.
queryCondDuration
  :: (Duration -> Bool) -> Database.CDBI.Connection.DBAction [Duration]
queryCondDuration = Database.CDBI.ER.getCondEntries duration_CDBI_Description

--- Gets a `Duration` entry by a given key.
getDuration :: DurationID -> Database.CDBI.Connection.DBAction Duration
getDuration =
  Database.CDBI.ER.getEntryWithKey duration_CDBI_Description durationColumnKey
   durationID

--- Inserts a new `Duration` entity.
newDurationWithEntryDuration_entryKey
  :: EntryID -> Database.CDBI.Connection.DBAction Duration
newDurationWithEntryDuration_entryKey entryDuration_entryKey_p =
  Database.CDBI.ER.insertNewEntry duration_CDBI_Description setDurationKey
   DurationID
   (Duration (DurationID 0) entryDuration_entryKey_p)

--- Deletes an existing `Duration` entry by its key.
deleteDuration :: Duration -> Database.CDBI.Connection.DBAction ()
deleteDuration =
  Database.CDBI.ER.deleteEntry duration_CDBI_Description durationColumnKey
   (durationID . durationKey)

--- Updates an existing `Duration` entry by its key.
updateDuration :: Duration -> Database.CDBI.Connection.DBAction ()
updateDuration = Database.CDBI.ER.updateEntry duration_CDBI_Description

--- The ER description of the `Weight` entity.
weight_CDBI_Description :: Database.CDBI.Description.EntityDescription Weight
weight_CDBI_Description =
  Database.CDBI.Description.ED "Weight"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeInt]
   (\(Weight (WeightID key) (EntryID entryWeight_entryKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt entryWeight_entryKey])
   (\(Weight _ (EntryID entryWeight_entryKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLInt entryWeight_entryKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt entryWeight_entryKey] ->
     Weight (WeightID key) (EntryID entryWeight_entryKey))

--- The database table of the `Weight` entity.
weightTable :: Database.CDBI.Description.Table
weightTable = "Weight"

--- The database column `Key` of the `Weight` entity.
weightColumnKey :: Database.CDBI.Description.Column WeightID
weightColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Weight\".\"Key\""

--- The database column `EntryWeight_entryKey` of the `Weight` entity.
weightColumnEntryWeight_entryKey :: Database.CDBI.Description.Column EntryID
weightColumnEntryWeight_entryKey =
  Database.CDBI.Description.Column "\"EntryWeight_entryKey\""
   "\"Weight\".\"EntryWeight_entryKey\""

--- The description of the database column `Key` of the `Weight` entity.
weightKeyColDesc :: Database.CDBI.Description.ColumnDescription WeightID
weightKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Weight\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(WeightID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> WeightID key)

--- The description of the database column `EntryWeight_entryKey` of the `Weight` entity.
weightEntryWeight_entryKeyColDesc
  :: Database.CDBI.Description.ColumnDescription EntryID
weightEntryWeight_entryKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Weight\".\"EntryWeight_entryKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntryID entryWeight_entryKey) ->
     Database.CDBI.Connection.SQLInt entryWeight_entryKey)
   (\(Database.CDBI.Connection.SQLInt entryWeight_entryKey) ->
     EntryID entryWeight_entryKey)

--- Gets the attribute `Key` of the `Weight` entity.
weightKey :: Weight -> WeightID
weightKey (Weight a _) = a

--- Gets the attribute `EntryWeight_entryKey` of the `Weight` entity.
weightEntryWeight_entryKey :: Weight -> EntryID
weightEntryWeight_entryKey (Weight _ a) = a

--- Sets the attribute `Key` of the `Weight` entity.
setWeightKey :: Weight -> WeightID -> Weight
setWeightKey (Weight _ b1) a = Weight a b1

--- Sets the attribute `EntryWeight_entryKey` of the `Weight` entity.
setWeightEntryWeight_entryKey :: Weight -> EntryID -> Weight
setWeightEntryWeight_entryKey (Weight a2 _) a = Weight a2 a

--- id-to-value function for entity `Weight`.
weightID :: WeightID -> Database.CDBI.Criteria.Value WeightID
weightID (WeightID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Weight`.
weightKeyToInt :: WeightID -> Int
weightKeyToInt (WeightID key) = key

--- Shows the key of a `Weight` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showWeightKey :: Weight -> String
showWeightKey entry =
  Database.CDBI.ER.showDatabaseKey "Weight" weightKeyToInt (weightKey entry)

--- Transforms a string into a key of a `Weight` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readWeightKey :: String -> Maybe WeightID
readWeightKey = Database.CDBI.ER.readDatabaseKey "Weight" WeightID

--- Gets all `Weight` entities.
queryAllWeights :: Database.CDBI.Connection.DBAction [Weight]
queryAllWeights = Database.CDBI.ER.getAllEntries weight_CDBI_Description

--- Gets all `Weight` entities satisfying a given predicate.
queryCondWeight
  :: (Weight -> Bool) -> Database.CDBI.Connection.DBAction [Weight]
queryCondWeight = Database.CDBI.ER.getCondEntries weight_CDBI_Description

--- Gets a `Weight` entry by a given key.
getWeight :: WeightID -> Database.CDBI.Connection.DBAction Weight
getWeight =
  Database.CDBI.ER.getEntryWithKey weight_CDBI_Description weightColumnKey
   weightID

--- Inserts a new `Weight` entity.
newWeightWithEntryWeight_entryKey
  :: EntryID -> Database.CDBI.Connection.DBAction Weight
newWeightWithEntryWeight_entryKey entryWeight_entryKey_p =
  Database.CDBI.ER.insertNewEntry weight_CDBI_Description setWeightKey WeightID
   (Weight (WeightID 0) entryWeight_entryKey_p)

--- Deletes an existing `Weight` entry by its key.
deleteWeight :: Weight -> Database.CDBI.Connection.DBAction ()
deleteWeight =
  Database.CDBI.ER.deleteEntry weight_CDBI_Description weightColumnKey
   (weightID . weightKey)

--- Updates an existing `Weight` entry by its key.
updateWeight :: Weight -> Database.CDBI.Connection.DBAction ()
updateWeight = Database.CDBI.ER.updateEntry weight_CDBI_Description

--- The ER description of the `Circumference` entity.
circumference_CDBI_Description
  :: Database.CDBI.Description.EntityDescription Circumference
circumference_CDBI_Description =
  Database.CDBI.Description.ED "Circumference"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeInt]
   (\(Circumference
       (CircumferenceID key) (EntryID entryCircumference_entryKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt entryCircumference_entryKey])
   (\(Circumference _ (EntryID entryCircumference_entryKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLInt entryCircumference_entryKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt entryCircumference_entryKey] ->
     Circumference (CircumferenceID key) (EntryID entryCircumference_entryKey))

--- The database table of the `Circumference` entity.
circumferenceTable :: Database.CDBI.Description.Table
circumferenceTable = "Circumference"

--- The database column `Key` of the `Circumference` entity.
circumferenceColumnKey :: Database.CDBI.Description.Column CircumferenceID
circumferenceColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Circumference\".\"Key\""

--- The database column `EntryCircumference_entryKey` of the `Circumference` entity.
circumferenceColumnEntryCircumference_entryKey
  :: Database.CDBI.Description.Column EntryID
circumferenceColumnEntryCircumference_entryKey =
  Database.CDBI.Description.Column "\"EntryCircumference_entryKey\""
   "\"Circumference\".\"EntryCircumference_entryKey\""

--- The description of the database column `Key` of the `Circumference` entity.
circumferenceKeyColDesc
  :: Database.CDBI.Description.ColumnDescription CircumferenceID
circumferenceKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Circumference\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(CircumferenceID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> CircumferenceID key)

--- The description of the database column `EntryCircumference_entryKey` of the `Circumference` entity.
circumferenceEntryCircumference_entryKeyColDesc
  :: Database.CDBI.Description.ColumnDescription EntryID
circumferenceEntryCircumference_entryKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"Circumference\".\"EntryCircumference_entryKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntryID entryCircumference_entryKey) ->
     Database.CDBI.Connection.SQLInt entryCircumference_entryKey)
   (\(Database.CDBI.Connection.SQLInt entryCircumference_entryKey) ->
     EntryID entryCircumference_entryKey)

--- Gets the attribute `Key` of the `Circumference` entity.
circumferenceKey :: Circumference -> CircumferenceID
circumferenceKey (Circumference a _) = a

--- Gets the attribute `EntryCircumference_entryKey` of the `Circumference` entity.
circumferenceEntryCircumference_entryKey :: Circumference -> EntryID
circumferenceEntryCircumference_entryKey (Circumference _ a) = a

--- Sets the attribute `Key` of the `Circumference` entity.
setCircumferenceKey :: Circumference -> CircumferenceID -> Circumference
setCircumferenceKey (Circumference _ b1) a = Circumference a b1

--- Sets the attribute `EntryCircumference_entryKey` of the `Circumference` entity.
setCircumferenceEntryCircumference_entryKey
  :: Circumference -> EntryID -> Circumference
setCircumferenceEntryCircumference_entryKey (Circumference a2 _) a =
  Circumference a2 a

--- id-to-value function for entity `Circumference`.
circumferenceID
  :: CircumferenceID -> Database.CDBI.Criteria.Value CircumferenceID
circumferenceID (CircumferenceID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Circumference`.
circumferenceKeyToInt :: CircumferenceID -> Int
circumferenceKeyToInt (CircumferenceID key) = key

--- Shows the key of a `Circumference` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showCircumferenceKey :: Circumference -> String
showCircumferenceKey entry =
  Database.CDBI.ER.showDatabaseKey "Circumference" circumferenceKeyToInt
   (circumferenceKey entry)

--- Transforms a string into a key of a `Circumference` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readCircumferenceKey :: String -> Maybe CircumferenceID
readCircumferenceKey =
  Database.CDBI.ER.readDatabaseKey "Circumference" CircumferenceID

--- Gets all `Circumference` entities.
queryAllCircumferences :: Database.CDBI.Connection.DBAction [Circumference]
queryAllCircumferences =
  Database.CDBI.ER.getAllEntries circumference_CDBI_Description

--- Gets all `Circumference` entities satisfying a given predicate.
queryCondCircumference
  :: (Circumference -> Bool)
  -> Database.CDBI.Connection.DBAction [Circumference]
queryCondCircumference =
  Database.CDBI.ER.getCondEntries circumference_CDBI_Description

--- Gets a `Circumference` entry by a given key.
getCircumference
  :: CircumferenceID -> Database.CDBI.Connection.DBAction Circumference
getCircumference =
  Database.CDBI.ER.getEntryWithKey circumference_CDBI_Description
   circumferenceColumnKey
   circumferenceID

--- Inserts a new `Circumference` entity.
newCircumferenceWithEntryCircumference_entryKey
  :: EntryID -> Database.CDBI.Connection.DBAction Circumference
newCircumferenceWithEntryCircumference_entryKey entryCircumference_entryKey_p =
  Database.CDBI.ER.insertNewEntry circumference_CDBI_Description
   setCircumferenceKey
   CircumferenceID
   (Circumference (CircumferenceID 0) entryCircumference_entryKey_p)

--- Deletes an existing `Circumference` entry by its key.
deleteCircumference :: Circumference -> Database.CDBI.Connection.DBAction ()
deleteCircumference =
  Database.CDBI.ER.deleteEntry circumference_CDBI_Description
   circumferenceColumnKey
   (circumferenceID . circumferenceKey)

--- Updates an existing `Circumference` entry by its key.
updateCircumference :: Circumference -> Database.CDBI.Connection.DBAction ()
updateCircumference =
  Database.CDBI.ER.updateEntry circumference_CDBI_Description

--- Generates a new database (name provided as the parameter) and
--- creates its schema.
createNewDB :: String -> IO ()
createNewDB dbfile =
  do conn <- Database.CDBI.Connection.connectSQLite dbfile
     Database.CDBI.Connection.writeConnection cstr conn
     Database.CDBI.Connection.disconnect conn
  where
    cstr =
      unlines
       ["create table 'Entry'('Key' integer primary key ,'Timestamp' string not null);"
       ,"create table 'Entity'('Key' integer primary key ,'Name' string not null ,'EntryEntity_entryKey' int REFERENCES 'Entry'(Key) unique not null);"
       ,"create table 'Event'('Key' integer primary key ,'Timestamp' string not null ,'EntryEvent_entryKey' int REFERENCES 'Entry'(Key) unique not null);"
       ,"create table 'Attrib'('Key' integer primary key ,'EntryAttrib_entryKey' int REFERENCES 'Entry'(Key) unique not null ,'EntryAttrib_subjectKey' int REFERENCES 'Entry'(Key) not null);"
       ,"create table 'Action'('Key' integer primary key ,'EntryAction_entryKey' int REFERENCES 'Entry'(Key) unique not null ,'EntryAction_subjectKey' int REFERENCES 'Entry'(Key) not null);"
       ,"create table 'Activity'('Key' integer primary key ,'Duration' string not null ,'EntryActivity_entryKey' int REFERENCES 'Entry'(Key) unique not null);"
       ,"create table 'Measurement'('Key' integer primary key ,'Unit' string not null ,'Value' float not null ,'Precision' float not null ,'EntryMeasurement_entryKey' int REFERENCES 'Entry'(Key) unique not null ,'EntryMeasurement_ofKey' int REFERENCES 'Entry'(Key) unique not null);"
       ,"create table 'Duration'('Key' integer primary key ,'EntryDuration_entryKey' int REFERENCES 'Entry'(Key) unique not null);"
       ,"create table 'Weight'('Key' integer primary key ,'EntryWeight_entryKey' int REFERENCES 'Entry'(Key) unique not null);"
       ,"create table 'Circumference'('Key' integer primary key ,'EntryCircumference_entryKey' int REFERENCES 'Entry'(Key) unique not null);"]

--- Saves complete database as term files into an existing directory
--- provided as a parameter.
saveDBTo :: String -> IO ()
saveDBTo dir =
  do Database.CDBI.ER.saveDBTerms entry_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms entity_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms event_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms attrib_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms action_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms activity_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms measurement_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms duration_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms weight_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms circumference_CDBI_Description sqliteDBFile
      dir

--- Restores complete database from term files which are stored
--- in a directory provided as a parameter.
restoreDBFrom :: String -> IO ()
restoreDBFrom dir =
  do Database.CDBI.ER.restoreDBTerms entry_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms entity_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms event_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms attrib_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms action_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms activity_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms measurement_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.restoreDBTerms duration_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms weight_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms circumference_CDBI_Description sqliteDBFile
      dir

--- Runs a DB action (typically a query).
runQ :: Database.CDBI.Connection.DBAction a -> IO a
runQ = Database.CDBI.ER.runQueryOnDB sqliteDBFile

--- Runs a DB action as a transaction.
runT
  :: Database.CDBI.Connection.DBAction a
  -> IO (Database.CDBI.Connection.SQLResult a)
runT = Database.CDBI.ER.runTransactionOnDB sqliteDBFile

--- Runs a DB action as a transaction. Emits an error in case of failure.
runJustT :: Database.CDBI.Connection.DBAction a -> IO a
runJustT = Database.CDBI.ER.runJustTransactionOnDB sqliteDBFile
