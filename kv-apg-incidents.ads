with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.locations;

package kv.apg.incidents is

   type Severity_Type is (Information, Warning, Error);

   type Incident_Class(Severity : Severity_Type) is tagged private;

   procedure Initialize
      (Self     : in out Incident_Class;
       Location : in     kv.apg.locations.Location_Type;
       Citation : in     String_Type;
       Reason   : in     String_Type);

   function Image(Self : Incident_Class) return String_Type;

private

   type Incident_Class(Severity : Severity_Type) is tagged
      record
         Location : kv.apg.locations.Location_Type;
         Citation : String_Type;
         Reason   : String_Type;
      end record;

end kv.apg.incidents;
