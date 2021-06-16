-- Entity Relationship Description for the Applied Rationality Framework Tools.

import Database.ERD

arf =
  ERD "arf"
   [
     Entity "Entry" [Attribute "Timestamp" (IntDom Nothing) NoKey False],
     Entity "Entity" [Attribute "Name" (StringDom Nothing) NoKey False],
     Entity "Event" [Attribute "Timestamp" (IntDom Nothing) NoKey False],
     Entity "Attrib" [],
     Entity "Action" [],
     Entity "Activity" [Attribute "Duration" (IntDom Nothing) NoKey False],
     Entity "Measurement" [
       Attribute "Unit" (StringDom Nothing) NoKey False,
       Attribute "Value" (FloatDom Nothing) NoKey False,
       Attribute "Precision" (FloatDom Nothing) NoKey False],
     -- Nutrition Utility Entities
     Entity "Meal" [
       Attribute "Calories" (FloatDom Nothing) NoKey False,
       Attribute "Description" (StringDom Nothing) NoKey False],
     Entity "Serving" [
       Attribute "ServingType" (StringDom Nothing) NoKey False,
       Attribute "Amount" (IntDom Nothing) NoKey False]
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
       REnd "Measurement" "of" (Between 0 Infinite),
       REnd "Entry" "measurement" (Exactly 1)],
     -- Nutrition Utility Entity Relations
     Relationship "Meal_entry" [
       REnd "Meal" "entry" (Between 0 (Max 1)),
       REnd "Entry" "meal" (Exactly 1)],
     Relationship "Serving_meal" [
       REnd "Serving" "meal" (Between 0 Infinite),
       REnd "Entry" "serving" (Exactly 1)]
   ]
