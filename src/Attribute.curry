--- Defines the insert, update, and delete operations for the Attribute database types.

module Attribute where

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

import PredicateIntf
import EntityIntf
import JSONExt
import Base
import Env
import arf

--- Represents attributes
--- @cons key - the entry key
--- @cons created - the date on which the entry was created
--- @cons subject - the entry that the attribute is associated with
data Attribute = Attribute { key :: Maybe Int, created :: ClockTime, subject :: Int }

predicateIntf :: PredicateIntf Attribute arf.Attrib
predicateIntf = PredicateIntf {
  predicateName_   = "attrib",
  cons_            = Attribute,
  predicateKey_    = key,
  created_         = created,
  subject_         = subject,
  tblName_         = "Attrib",
  predicateInsert_ = arf.newAttribWithEntryAttrib_entryKeyWithEntryAttrib_subjectKey}

entityIntf :: EntityIntf Attribute
entityIntf = PredicateIntf.entityIntf predicateIntf

handler :: [String] -> Env -> IO ()
handler = EntityIntf.handler entityIntf
