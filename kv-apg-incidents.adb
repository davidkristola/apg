with Ada.Strings.Wide_Wide_Unbounded;

package body kv.apg.incidents is

   use Ada.Strings.Wide_Wide_Unbounded; -- &
   use kv.apg.locations;

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self     : in out Incident_Class;
       Location : in     kv.apg.locations.File_Location_Type;
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

   ----------------------------------------------------------------------------
   not overriding procedure Initialize
      (Self   : in out Writer_Report_Class;
       Writer : not null access kv.apg.writer.Writer_Class'CLASS) is
   begin
      Self.Writer := Writer;
   end Initialize;

   ----------------------------------------------------------------------------
   not overriding procedure Set_Filter_Level
      (Self        : in out Writer_Report_Class;
       Let_Through : in     Severity_Type) is
   begin
      Self.Level := Let_Through;
   end Set_Filter_Level;

   ----------------------------------------------------------------------------
   overriding procedure Note
      (Self     : in     Writer_Report_Class;
       Incident : in     Incident_Class'CLASS) is
   begin
      if Incident.Severity >= Self.Level then
         Self.Writer.Write_Line(Incident.Image);
      end if;
   end Note;

end kv.apg.incidents;
