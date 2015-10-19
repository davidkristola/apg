with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.locations;
with kv.apg.writer;

package kv.apg.incidents is

   type Severity_Type is (Debug, Detail, Information, Warning, Error);

   type Incident_Class(Severity : Severity_Type) is tagged private;

   procedure Initialize
      (Self     : in out Incident_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String_Type);

   function Image(Self : Incident_Class) return String_Type;




   type Report_Class is interface;
   procedure Note
      (Self     : in     Report_Class;
       Incident : in     Incident_Class'CLASS) is abstract;



   type Writer_Report_Class is new Report_Class with private;
   not overriding procedure Initialize
      (Self   : in out Writer_Report_Class;
       Writer : not null access kv.apg.writer.Writer_Class'CLASS);
   not overriding procedure Set_Filter_Level
      (Self        : in out Writer_Report_Class;
       Let_Through : in     Severity_Type);
   overriding procedure Note
      (Self     : in     Writer_Report_Class;
       Incident : in     Incident_Class'CLASS);


private

   type Incident_Class(Severity : Severity_Type) is tagged
      record
         Location : kv.apg.locations.File_Location_Type;
         Citation : String_Type;
         Reason   : String_Type;
      end record;

   type Writer_Report_Class is new Report_Class with
      record
         Writer : access kv.apg.writer.Writer_Class'CLASS;
         Level  : Severity_Type := Debug;
      end record;

end kv.apg.incidents;
