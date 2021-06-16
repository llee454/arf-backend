--- This file has been generated from
--- 
---     /arf-backend/src/arf.erdterm
--- 
--- and contains definitions for all entities and relations
--- specified in this model.

module arf where

import qualified Time
import qualified Database.CDBI.ER
import qualified Database.CDBI.Criteria
import qualified Database.CDBI.Connection
import qualified Database.CDBI.Description

data Entry = Entry EntryID Int
 deriving (Eq,Show,Read)

data EntryID = EntryID Int
 deriving (Eq,Show,Read)

data Entity = Entity EntityID String EntryID
 deriving (Eq,Show,Read)

data EntityID = EntityID Int
 deriving (Eq,Show,Read)

data Event = Event EventID Int EntryID
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

data Activity = Activity ActivityID Int EntryID
 deriving (Eq,Show,Read)

data ActivityID = ActivityID Int
 deriving (Eq,Show,Read)

data Measurement = Measurement MeasurementID String Float Float EntryID EntryID
 deriving (Eq,Show,Read)

data MeasurementID = MeasurementID Int
 deriving (Eq,Show,Read)

data Meal = Meal MealID Float String EntryID
 deriving (Eq,Show,Read)

data MealID = MealID Int
 deriving (Eq,Show,Read)

data Serving = Serving ServingID String Int EntryID
 deriving (Eq,Show,Read)

data ServingID = ServingID Int
 deriving (Eq,Show,Read)

--- The name of the SQLite database file.
sqliteDBFile :: String
sqliteDBFile = "data/Arf.sqlite"

--- The ER description of the `Entry` entity.
entry_CDBI_Description :: Database.CDBI.Description.EntityDescription Entry
entry_CDBI_Description =
  Database.CDBI.Description.ED "Entry"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeInt]
   (\(Entry (EntryID key) timestamp) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt timestamp])
   (\(Entry _ timestamp) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLInt timestamp])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt timestamp] ->
     Entry (EntryID key) timestamp)

--- The database table of the `Entry` entity.
entryTable :: Database.CDBI.Description.Table
entryTable = "Entry"

--- The database column `Key` of the `Entry` entity.
entryColumnKey :: Database.CDBI.Description.Column EntryID
entryColumnKey = Database.CDBI.Description.Column "\"Key\"" "\"Entry\".\"Key\""

--- The database column `Timestamp` of the `Entry` entity.
entryColumnTimestamp :: Database.CDBI.Description.Column Int
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
entryTimestampColDesc :: Database.CDBI.Description.ColumnDescription Int
entryTimestampColDesc =
  Database.CDBI.Description.ColDesc "\"Entry\".\"Timestamp\""
   Database.CDBI.Connection.SQLTypeInt
   (\timestamp -> Database.CDBI.Connection.SQLInt timestamp)
   (\(Database.CDBI.Connection.SQLInt timestamp) -> timestamp)

--- Gets the attribute `Key` of the `Entry` entity.
entryKey :: Entry -> EntryID
entryKey (Entry a _) = a

--- Gets the attribute `Timestamp` of the `Entry` entity.
entryTimestamp :: Entry -> Int
entryTimestamp (Entry _ a) = a

--- Sets the attribute `Key` of the `Entry` entity.
setEntryKey :: Entry -> EntryID -> Entry
setEntryKey (Entry _ b1) a = Entry a b1

--- Sets the attribute `Timestamp` of the `Entry` entity.
setEntryTimestamp :: Entry -> Int -> Entry
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
newEntry :: Int -> Database.CDBI.Connection.DBAction Entry
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
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(Event (EventID key) timestamp (EntryID entryEvent_entryKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt timestamp
     ,Database.CDBI.Connection.SQLInt entryEvent_entryKey])
   (\(Event _ timestamp (EntryID entryEvent_entryKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLInt timestamp
     ,Database.CDBI.Connection.SQLInt entryEvent_entryKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt timestamp
     ,Database.CDBI.Connection.SQLInt entryEvent_entryKey] ->
     Event (EventID key) timestamp (EntryID entryEvent_entryKey))

--- The database table of the `Event` entity.
eventTable :: Database.CDBI.Description.Table
eventTable = "Event"

--- The database column `Key` of the `Event` entity.
eventColumnKey :: Database.CDBI.Description.Column EventID
eventColumnKey = Database.CDBI.Description.Column "\"Key\"" "\"Event\".\"Key\""

--- The database column `Timestamp` of the `Event` entity.
eventColumnTimestamp :: Database.CDBI.Description.Column Int
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
eventTimestampColDesc :: Database.CDBI.Description.ColumnDescription Int
eventTimestampColDesc =
  Database.CDBI.Description.ColDesc "\"Event\".\"Timestamp\""
   Database.CDBI.Connection.SQLTypeInt
   (\timestamp -> Database.CDBI.Connection.SQLInt timestamp)
   (\(Database.CDBI.Connection.SQLInt timestamp) -> timestamp)

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
eventTimestamp :: Event -> Int
eventTimestamp (Event _ a _) = a

--- Gets the attribute `EntryEvent_entryKey` of the `Event` entity.
eventEntryEvent_entryKey :: Event -> EntryID
eventEntryEvent_entryKey (Event _ _ a) = a

--- Sets the attribute `Key` of the `Event` entity.
setEventKey :: Event -> EventID -> Event
setEventKey (Event _ b2 b1) a = Event a b2 b1

--- Sets the attribute `Timestamp` of the `Event` entity.
setEventTimestamp :: Event -> Int -> Event
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
  :: Int -> EntryID -> Database.CDBI.Connection.DBAction Event
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
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(Activity (ActivityID key) duration (EntryID entryActivity_entryKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt duration
     ,Database.CDBI.Connection.SQLInt entryActivity_entryKey])
   (\(Activity _ duration (EntryID entryActivity_entryKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLInt duration
     ,Database.CDBI.Connection.SQLInt entryActivity_entryKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLInt duration
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
activityColumnDuration :: Database.CDBI.Description.Column Int
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
activityDurationColDesc :: Database.CDBI.Description.ColumnDescription Int
activityDurationColDesc =
  Database.CDBI.Description.ColDesc "\"Activity\".\"Duration\""
   Database.CDBI.Connection.SQLTypeInt
   (\duration -> Database.CDBI.Connection.SQLInt duration)
   (\(Database.CDBI.Connection.SQLInt duration) -> duration)

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
activityDuration :: Activity -> Int
activityDuration (Activity _ a _) = a

--- Gets the attribute `EntryActivity_entryKey` of the `Activity` entity.
activityEntryActivity_entryKey :: Activity -> EntryID
activityEntryActivity_entryKey (Activity _ _ a) = a

--- Sets the attribute `Key` of the `Activity` entity.
setActivityKey :: Activity -> ActivityID -> Activity
setActivityKey (Activity _ b2 b1) a = Activity a b2 b1

--- Sets the attribute `Duration` of the `Activity` entity.
setActivityDuration :: Activity -> Int -> Activity
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
  :: Int -> EntryID -> Database.CDBI.Connection.DBAction Activity
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

--- The ER description of the `Meal` entity.
meal_CDBI_Description :: Database.CDBI.Description.EntityDescription Meal
meal_CDBI_Description =
  Database.CDBI.Description.ED "Meal"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeFloat
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(Meal (MealID key) calories description (EntryID entryMeal_entryKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLFloat calories
     ,Database.CDBI.Connection.SQLString description
     ,Database.CDBI.Connection.SQLInt entryMeal_entryKey])
   (\(Meal _ calories description (EntryID entryMeal_entryKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLFloat calories
     ,Database.CDBI.Connection.SQLString description
     ,Database.CDBI.Connection.SQLInt entryMeal_entryKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLFloat calories
     ,Database.CDBI.Connection.SQLString description
     ,Database.CDBI.Connection.SQLInt entryMeal_entryKey] ->
     Meal (MealID key) calories description (EntryID entryMeal_entryKey))

--- The database table of the `Meal` entity.
mealTable :: Database.CDBI.Description.Table
mealTable = "Meal"

--- The database column `Key` of the `Meal` entity.
mealColumnKey :: Database.CDBI.Description.Column MealID
mealColumnKey = Database.CDBI.Description.Column "\"Key\"" "\"Meal\".\"Key\""

--- The database column `Calories` of the `Meal` entity.
mealColumnCalories :: Database.CDBI.Description.Column Float
mealColumnCalories =
  Database.CDBI.Description.Column "\"Calories\"" "\"Meal\".\"Calories\""

--- The database column `Description` of the `Meal` entity.
mealColumnDescription :: Database.CDBI.Description.Column String
mealColumnDescription =
  Database.CDBI.Description.Column "\"Description\"" "\"Meal\".\"Description\""

--- The database column `EntryMeal_entryKey` of the `Meal` entity.
mealColumnEntryMeal_entryKey :: Database.CDBI.Description.Column EntryID
mealColumnEntryMeal_entryKey =
  Database.CDBI.Description.Column "\"EntryMeal_entryKey\""
   "\"Meal\".\"EntryMeal_entryKey\""

--- The description of the database column `Key` of the `Meal` entity.
mealKeyColDesc :: Database.CDBI.Description.ColumnDescription MealID
mealKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Meal\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(MealID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> MealID key)

--- The description of the database column `Calories` of the `Meal` entity.
mealCaloriesColDesc :: Database.CDBI.Description.ColumnDescription Float
mealCaloriesColDesc =
  Database.CDBI.Description.ColDesc "\"Meal\".\"Calories\""
   Database.CDBI.Connection.SQLTypeFloat
   (\calories -> Database.CDBI.Connection.SQLFloat calories)
   (\(Database.CDBI.Connection.SQLFloat calories) -> calories)

--- The description of the database column `Description` of the `Meal` entity.
mealDescriptionColDesc :: Database.CDBI.Description.ColumnDescription String
mealDescriptionColDesc =
  Database.CDBI.Description.ColDesc "\"Meal\".\"Description\""
   Database.CDBI.Connection.SQLTypeString
   (\description -> Database.CDBI.Connection.SQLString description)
   (\(Database.CDBI.Connection.SQLString description) -> description)

--- The description of the database column `EntryMeal_entryKey` of the `Meal` entity.
mealEntryMeal_entryKeyColDesc
  :: Database.CDBI.Description.ColumnDescription EntryID
mealEntryMeal_entryKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Meal\".\"EntryMeal_entryKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntryID entryMeal_entryKey) ->
     Database.CDBI.Connection.SQLInt entryMeal_entryKey)
   (\(Database.CDBI.Connection.SQLInt entryMeal_entryKey) ->
     EntryID entryMeal_entryKey)

--- Gets the attribute `Key` of the `Meal` entity.
mealKey :: Meal -> MealID
mealKey (Meal a _ _ _) = a

--- Gets the attribute `Calories` of the `Meal` entity.
mealCalories :: Meal -> Float
mealCalories (Meal _ a _ _) = a

--- Gets the attribute `Description` of the `Meal` entity.
mealDescription :: Meal -> String
mealDescription (Meal _ _ a _) = a

--- Gets the attribute `EntryMeal_entryKey` of the `Meal` entity.
mealEntryMeal_entryKey :: Meal -> EntryID
mealEntryMeal_entryKey (Meal _ _ _ a) = a

--- Sets the attribute `Key` of the `Meal` entity.
setMealKey :: Meal -> MealID -> Meal
setMealKey (Meal _ b3 b2 b1) a = Meal a b3 b2 b1

--- Sets the attribute `Calories` of the `Meal` entity.
setMealCalories :: Meal -> Float -> Meal
setMealCalories (Meal a2 _ b2 b1) a = Meal a2 a b2 b1

--- Sets the attribute `Description` of the `Meal` entity.
setMealDescription :: Meal -> String -> Meal
setMealDescription (Meal a3 a2 _ b1) a = Meal a3 a2 a b1

--- Sets the attribute `EntryMeal_entryKey` of the `Meal` entity.
setMealEntryMeal_entryKey :: Meal -> EntryID -> Meal
setMealEntryMeal_entryKey (Meal a4 a3 a2 _) a = Meal a4 a3 a2 a

--- id-to-value function for entity `Meal`.
mealID :: MealID -> Database.CDBI.Criteria.Value MealID
mealID (MealID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Meal`.
mealKeyToInt :: MealID -> Int
mealKeyToInt (MealID key) = key

--- Shows the key of a `Meal` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showMealKey :: Meal -> String
showMealKey entry =
  Database.CDBI.ER.showDatabaseKey "Meal" mealKeyToInt (mealKey entry)

--- Transforms a string into a key of a `Meal` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readMealKey :: String -> Maybe MealID
readMealKey = Database.CDBI.ER.readDatabaseKey "Meal" MealID

--- Gets all `Meal` entities.
queryAllMeals :: Database.CDBI.Connection.DBAction [Meal]
queryAllMeals = Database.CDBI.ER.getAllEntries meal_CDBI_Description

--- Gets all `Meal` entities satisfying a given predicate.
queryCondMeal :: (Meal -> Bool) -> Database.CDBI.Connection.DBAction [Meal]
queryCondMeal = Database.CDBI.ER.getCondEntries meal_CDBI_Description

--- Gets a `Meal` entry by a given key.
getMeal :: MealID -> Database.CDBI.Connection.DBAction Meal
getMeal =
  Database.CDBI.ER.getEntryWithKey meal_CDBI_Description mealColumnKey mealID

--- Inserts a new `Meal` entity.
newMealWithEntryMeal_entryKey
  :: Float -> String -> EntryID -> Database.CDBI.Connection.DBAction Meal
newMealWithEntryMeal_entryKey calories_p description_p entryMeal_entryKey_p =
  Database.CDBI.ER.insertNewEntry meal_CDBI_Description setMealKey MealID
   (Meal (MealID 0) calories_p description_p entryMeal_entryKey_p)

--- Deletes an existing `Meal` entry by its key.
deleteMeal :: Meal -> Database.CDBI.Connection.DBAction ()
deleteMeal =
  Database.CDBI.ER.deleteEntry meal_CDBI_Description mealColumnKey
   (mealID . mealKey)

--- Updates an existing `Meal` entry by its key.
updateMeal :: Meal -> Database.CDBI.Connection.DBAction ()
updateMeal = Database.CDBI.ER.updateEntry meal_CDBI_Description

--- The ER description of the `Serving` entity.
serving_CDBI_Description :: Database.CDBI.Description.EntityDescription Serving
serving_CDBI_Description =
  Database.CDBI.Description.ED "Serving"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(Serving
       (ServingID key) servingType amount (EntryID entryServing_mealKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString servingType
     ,Database.CDBI.Connection.SQLInt amount
     ,Database.CDBI.Connection.SQLInt entryServing_mealKey])
   (\(Serving _ servingType amount (EntryID entryServing_mealKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString servingType
     ,Database.CDBI.Connection.SQLInt amount
     ,Database.CDBI.Connection.SQLInt entryServing_mealKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString servingType
     ,Database.CDBI.Connection.SQLInt amount
     ,Database.CDBI.Connection.SQLInt entryServing_mealKey] ->
     Serving (ServingID key) servingType amount (EntryID entryServing_mealKey))

--- The database table of the `Serving` entity.
servingTable :: Database.CDBI.Description.Table
servingTable = "Serving"

--- The database column `Key` of the `Serving` entity.
servingColumnKey :: Database.CDBI.Description.Column ServingID
servingColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Serving\".\"Key\""

--- The database column `ServingType` of the `Serving` entity.
servingColumnServingType :: Database.CDBI.Description.Column String
servingColumnServingType =
  Database.CDBI.Description.Column "\"ServingType\""
   "\"Serving\".\"ServingType\""

--- The database column `Amount` of the `Serving` entity.
servingColumnAmount :: Database.CDBI.Description.Column Int
servingColumnAmount =
  Database.CDBI.Description.Column "\"Amount\"" "\"Serving\".\"Amount\""

--- The database column `EntryServing_mealKey` of the `Serving` entity.
servingColumnEntryServing_mealKey :: Database.CDBI.Description.Column EntryID
servingColumnEntryServing_mealKey =
  Database.CDBI.Description.Column "\"EntryServing_mealKey\""
   "\"Serving\".\"EntryServing_mealKey\""

--- The description of the database column `Key` of the `Serving` entity.
servingKeyColDesc :: Database.CDBI.Description.ColumnDescription ServingID
servingKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Serving\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(ServingID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> ServingID key)

--- The description of the database column `ServingType` of the `Serving` entity.
servingServingTypeColDesc :: Database.CDBI.Description.ColumnDescription String
servingServingTypeColDesc =
  Database.CDBI.Description.ColDesc "\"Serving\".\"ServingType\""
   Database.CDBI.Connection.SQLTypeString
   (\servingType -> Database.CDBI.Connection.SQLString servingType)
   (\(Database.CDBI.Connection.SQLString servingType) -> servingType)

--- The description of the database column `Amount` of the `Serving` entity.
servingAmountColDesc :: Database.CDBI.Description.ColumnDescription Int
servingAmountColDesc =
  Database.CDBI.Description.ColDesc "\"Serving\".\"Amount\""
   Database.CDBI.Connection.SQLTypeInt
   (\amount -> Database.CDBI.Connection.SQLInt amount)
   (\(Database.CDBI.Connection.SQLInt amount) -> amount)

--- The description of the database column `EntryServing_mealKey` of the `Serving` entity.
servingEntryServing_mealKeyColDesc
  :: Database.CDBI.Description.ColumnDescription EntryID
servingEntryServing_mealKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Serving\".\"EntryServing_mealKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(EntryID entryServing_mealKey) ->
     Database.CDBI.Connection.SQLInt entryServing_mealKey)
   (\(Database.CDBI.Connection.SQLInt entryServing_mealKey) ->
     EntryID entryServing_mealKey)

--- Gets the attribute `Key` of the `Serving` entity.
servingKey :: Serving -> ServingID
servingKey (Serving a _ _ _) = a

--- Gets the attribute `ServingType` of the `Serving` entity.
servingServingType :: Serving -> String
servingServingType (Serving _ a _ _) = a

--- Gets the attribute `Amount` of the `Serving` entity.
servingAmount :: Serving -> Int
servingAmount (Serving _ _ a _) = a

--- Gets the attribute `EntryServing_mealKey` of the `Serving` entity.
servingEntryServing_mealKey :: Serving -> EntryID
servingEntryServing_mealKey (Serving _ _ _ a) = a

--- Sets the attribute `Key` of the `Serving` entity.
setServingKey :: Serving -> ServingID -> Serving
setServingKey (Serving _ b3 b2 b1) a = Serving a b3 b2 b1

--- Sets the attribute `ServingType` of the `Serving` entity.
setServingServingType :: Serving -> String -> Serving
setServingServingType (Serving a2 _ b2 b1) a = Serving a2 a b2 b1

--- Sets the attribute `Amount` of the `Serving` entity.
setServingAmount :: Serving -> Int -> Serving
setServingAmount (Serving a3 a2 _ b1) a = Serving a3 a2 a b1

--- Sets the attribute `EntryServing_mealKey` of the `Serving` entity.
setServingEntryServing_mealKey :: Serving -> EntryID -> Serving
setServingEntryServing_mealKey (Serving a4 a3 a2 _) a = Serving a4 a3 a2 a

--- id-to-value function for entity `Serving`.
servingID :: ServingID -> Database.CDBI.Criteria.Value ServingID
servingID (ServingID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Serving`.
servingKeyToInt :: ServingID -> Int
servingKeyToInt (ServingID key) = key

--- Shows the key of a `Serving` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showServingKey :: Serving -> String
showServingKey entry =
  Database.CDBI.ER.showDatabaseKey "Serving" servingKeyToInt (servingKey entry)

--- Transforms a string into a key of a `Serving` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readServingKey :: String -> Maybe ServingID
readServingKey = Database.CDBI.ER.readDatabaseKey "Serving" ServingID

--- Gets all `Serving` entities.
queryAllServings :: Database.CDBI.Connection.DBAction [Serving]
queryAllServings = Database.CDBI.ER.getAllEntries serving_CDBI_Description

--- Gets all `Serving` entities satisfying a given predicate.
queryCondServing
  :: (Serving -> Bool) -> Database.CDBI.Connection.DBAction [Serving]
queryCondServing = Database.CDBI.ER.getCondEntries serving_CDBI_Description

--- Gets a `Serving` entry by a given key.
getServing :: ServingID -> Database.CDBI.Connection.DBAction Serving
getServing =
  Database.CDBI.ER.getEntryWithKey serving_CDBI_Description servingColumnKey
   servingID

--- Inserts a new `Serving` entity.
newServingWithEntryServing_mealKey
  :: String -> Int -> EntryID -> Database.CDBI.Connection.DBAction Serving
newServingWithEntryServing_mealKey
    servingType_p amount_p entryServing_mealKey_p =
  Database.CDBI.ER.insertNewEntry serving_CDBI_Description setServingKey
   ServingID
   (Serving (ServingID 0) servingType_p amount_p entryServing_mealKey_p)

--- Deletes an existing `Serving` entry by its key.
deleteServing :: Serving -> Database.CDBI.Connection.DBAction ()
deleteServing =
  Database.CDBI.ER.deleteEntry serving_CDBI_Description servingColumnKey
   (servingID . servingKey)

--- Updates an existing `Serving` entry by its key.
updateServing :: Serving -> Database.CDBI.Connection.DBAction ()
updateServing = Database.CDBI.ER.updateEntry serving_CDBI_Description

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
       ["create table 'Entry'('Key' integer primary key ,'Timestamp' not null);"
       ,"create table 'Entity'('Key' integer primary key ,'Name' string not null ,'EntryEntity_entryKey' int REFERENCES 'Entry'(Key) unique not null);"
       ,"create table 'Event'('Key' integer primary key ,'Timestamp' not null ,'EntryEvent_entryKey' int REFERENCES 'Entry'(Key) unique not null);"
       ,"create table 'Attrib'('Key' integer primary key ,'EntryAttrib_entryKey' int REFERENCES 'Entry'(Key) unique not null ,'EntryAttrib_subjectKey' int REFERENCES 'Entry'(Key) not null);"
       ,"create table 'Action'('Key' integer primary key ,'EntryAction_entryKey' int REFERENCES 'Entry'(Key) unique not null ,'EntryAction_subjectKey' int REFERENCES 'Entry'(Key) not null);"
       ,"create table 'Activity'('Key' integer primary key ,'Duration' not null ,'EntryActivity_entryKey' int REFERENCES 'Entry'(Key) unique not null);"
       ,"create table 'Measurement'('Key' integer primary key ,'Unit' string not null ,'Value' float not null ,'Precision' float not null ,'EntryMeasurement_entryKey' int REFERENCES 'Entry'(Key) unique not null ,'EntryMeasurement_ofKey' int REFERENCES 'Entry'(Key) not null);"
       ,"create table 'Meal'('Key' integer primary key ,'Calories' float not null ,'Description' string not null ,'EntryMeal_entryKey' int REFERENCES 'Entry'(Key) unique not null);"
       ,"create table 'Serving'('Key' integer primary key ,'ServingType' string not null ,'Amount' not null ,'EntryServing_mealKey' int REFERENCES 'Entry'(Key) not null);"]

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
     Database.CDBI.ER.saveDBTerms meal_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms serving_CDBI_Description sqliteDBFile dir

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
     Database.CDBI.ER.restoreDBTerms meal_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms serving_CDBI_Description sqliteDBFile dir

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
