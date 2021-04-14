-- Entity Relationship Description for the Applied Rationality Framework Tools.

import Database.ERD

arf =
  ERD "arf"
   [
     Entity "Entry" [Attribute "Timestamp" (DateDom Nothing) NoKey False],
     Entity "Entity" [Attribute "Name" (StringDom Nothing) NoKey False],
     Entity "Event" [Attribute "Timestamp" (StringDom Nothing) NoKey False],
     Entity "Attribute" [],
     Entity "Action" [],
     Entity "Activity" [],
     Entity "Measurement" [
       Attribute "Unit" (StringDom Nothing) NoKey False,
       Attribute "Value" (FloatDom Nothing) NoKey False,
       Attribute "Precision" (StringDom Nothing) NoKey False],
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
     Relationship "Attribute_entry" [
       REnd "Attribute" "entry" (Between 0 (Max 1)),
       REnd "Entry" "attribute" (Exactly 1)],
     Relationship "Action_entry" [
       REnd "Action" "entry" (Between 0 (Max 1)),
       REnd "Entry" "action" (Exactly 1)],
     Relationship "Activity_entry" [
       REnd "Activity" "entry" (Between 0 (Max 1)),
       REnd "Entry" "activity" (Exactly 1)],
     Relationship "Measurement_action_entry" [
       REnd "Measurement" "entry" (Between 0 (Max 1)),
       REnd "Entry" "action" (Exactly 1)],
     Relationship "Measurement_of_entry" [
       REnd "Measurement" "entry" (Between 0 (Max 1)),
       REnd "Entry" "of" (Exactly 1)],
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
