package body kv.apg.logger.writer is

   ----------------------------------------------------------------------------
   not overriding procedure Initialize
      (Self   : in out Writer_Logger_Class;
       Writer : not null access kv.apg.writer.Writer_Class'CLASS;
       Level  : in     kv.apg.incidents.Severity_Type) is
   begin
      Self.Reporter.Initialize(Writer);
      Self.Reporter.Set_Filter_Level(Level);
   end Initialize;

   ----------------------------------------------------------------------------
   not overriding procedure Set_New_Level
      (Self  : in out Writer_Logger_Class;
       Level : in     kv.apg.incidents.Severity_Type) is
   begin
      Self.Reporter.Set_Filter_Level(Level);
   end Set_New_Level;

   ----------------------------------------------------------------------------
   overriding procedure Note_By_Severity
      (Self        : in out Writer_Logger_Class;
       Severity    : in     kv.apg.incidents.Severity_Type;
       Information : in     String) is
   begin
      Self.Reporter.Note(Severity, Information);
   end Note_By_Severity;

   ----------------------------------------------------------------------------
   overriding procedure Note_By_Severity
      (Self     : in out Writer_Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String;
       Severity : in     kv.apg.incidents.Severity_Type) is

      What : kv.apg.incidents.Incident_Class(Severity);

   begin
      What.Initialize(Location, Citation, To_String_Type(Reason));
      Self.Reporter.Note(What);
   end Note_By_Severity;

   ----------------------------------------------------------------------------
   overriding procedure Note_Debug
      (Self     : in out Writer_Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String) is
   begin
      Note_By_Severity(Self, Location, Citation, Reason, kv.apg.incidents.Debug);
   end Note_Debug;

   ----------------------------------------------------------------------------
   overriding procedure Note_Detail
      (Self     : in out Writer_Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String) is
   begin
      Note_By_Severity(Self, Location, Citation, Reason, kv.apg.incidents.Detail);
   end Note_Detail;

   ----------------------------------------------------------------------------
   overriding procedure Note_Info
      (Self     : in out Writer_Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String) is
   begin
      Note_By_Severity(Self, Location, Citation, Reason, kv.apg.incidents.Information);
   end Note_Info;

   ----------------------------------------------------------------------------
   overriding procedure Note_Warning
      (Self     : in out Writer_Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String) is
   begin
      Note_By_Severity(Self, Location, Citation, Reason, kv.apg.incidents.Warning);
   end Note_Warning;

   ----------------------------------------------------------------------------
   overriding procedure Note_Error
      (Self     : in out Writer_Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String) is
   begin
      Note_By_Severity(Self, Location, Citation, Reason, kv.apg.incidents.Error);
   end Note_Error;

end kv.apg.logger.writer;
