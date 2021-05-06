-- Entity Relationship Description for the Applied Rationality Framework Tools.

import Database.ERD

arf =
  ERD "arf"
   [
     Entity "Entry" [Attribute "Timestamp" (DateDom Nothing) NoKey False],
     Entity "Entity" [Attribute "Name" (StringDom Nothing) NoKey False],
     Entity "Event" [Attribute "Timestamp" (DateDom Nothing) NoKey False],
     Entity "Attrib" [],
     Entity "Action" [],
     Entity "Activity" [Attribute "Duration" (DateDom Nothing) NoKey False],
     Entity "Measurement" [
       Attribute "Unit" (StringDom Nothing) NoKey False,
       Attribute "Value" (FloatDom Nothing) NoKey False,
       Attribute "Precision" (FloatDom Nothing) NoKey False],
     Entity "Duration" [],
     Entity "Weight" [],
     Entity "Circumference" []
   ]
   [
     Relationship "Entity_entry" [
       REnd "Entity" "entry" (Between 0 (Max 1)),
       REnd "Entry" "entity" (Exactly 1)],
     Relationship "Event_entry" [
       REnd "Event" "entry" (Between 0 (Max 1)),
       REnd "Entry" "event" (Exactly 1)],
     Relationship "Attrib_entry" [
       REnd "Attrib" "entry" (Between 0 (Max 1)),
       REnd "Entry" "attrib" (Exactly 1)],
     Relationship "Attrib_subject" [
       REnd "Attrib" "subject" (Between 0 Infinite),
       REnd "Entry" "attrib_ref" (Exactly 1)],
     Relationship "Action_entry" [
       REnd "Action" "entry" (Between 0 (Max 1)),
       REnd "Entry" "action" (Exactly 1)],
     Relationship "Action_subject" [
       REnd "Action" "subject" (Between 0 Infinite),
       REnd "Entry" "action_ref" (Exactly 1)],
     Relationship "Activity_entry" [
       REnd "Activity" "entry" (Between 0 (Max 1)),
       REnd "Entry" "activity" (Exactly 1)],
     Relationship "Measurement_entry" [
       REnd "Measurement" "entry" (Between 0 (Max 1)),
       REnd "Entry" "is_measurement" (Exactly 1)],
     Relationship "Measurement_of" [
       REnd "Measurement" "of" (Between 0 (Max 1)),
       REnd "Entry" "measurement" (Exactly 1)],
     Relationship "Duration_entry" [
       REnd "Duration" "entry" (Between 0 (Max 1)),
       REnd "Entry" "duration" (Exactly 1)],
     Relationship "Weight_entry" [
       REnd "Weight" "entry" (Between 0 (Max 1)),
       REnd "Entry" "weight" (Exactly 1)],
     Relationship "Circumference_entry" [
       REnd "Circumference" "entry" (Between 0 (Max 1)),
       REnd "Entry" "circumference" (Exactly 1)]
   ]
