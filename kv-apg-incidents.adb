with Ada.Strings.Wide_Wide_Unbounded;

package body kv.apg.incidents is

   use Ada.Strings.Wide_Wide_Unbounded; -- &
   use kv.apg.locations;

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self     : in out Incident_Class;
       Location : in     kv.apg.locations.Location_Type;
       Citation : in     String_Type;
       Reason   : in     String_Type) is
   begin
      Self.Location := Location;
      Self.Citation := Citation;
      Self.Reason   := Reason;
   end Initialize;

   ----------------------------------------------------------------------------
   function Image(Self : Incident_Class) return String_Type is
      Answer : String_Type := To_String_Type(Severity_Type'IMAGE(Self.Severity)&" ("&Image(Self.Location)&"): ");
   begin
      Answer := Answer & Self.Reason & To_String_Type(" (""") & Self.Citation & To_String_Type(""").");
      return Answer;
   end Image;

end kv.apg.incidents;
