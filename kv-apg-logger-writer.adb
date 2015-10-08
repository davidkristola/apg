package body kv.apg.logger.writer is

   ----------------------------------------------------------------------------
   not overriding procedure Initialize
      (Self   : in out Writer_Logger_Class;
       File   : in     String;
       Writer : not null access kv.apg.writer.Writer_Class'CLASS;
       Level  : in     kv.apg.incidents.Severity_Type) is
   begin
      Self.Factory.Initialize(File);
      Self.Reporter.Initialize(Writer);
      Self.Reporter.Set_Filter_Level(Level);
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Note_Level
      (Self     : in out Writer_Logger_Class;
       Line     : in     Natural;
       Column   : in     Natural;
       Citation : in     String_Type;
       Reason   : in     String;
       Level    : in     kv.apg.incidents.Severity_Type) is

      Where : kv.apg.locations.Location_Type;
      What  : kv.apg.incidents.Incident_Class(Level);

   begin
      Where := Self.Factory.New_Location(Line => Line, Column => Column);
      What.Initialize(Where, Citation, To_String_Type(Reason));
      Self.Reporter.Note(What);
   end Note_Level;

   ----------------------------------------------------------------------------
   overriding procedure Note_Debug
      (Self     : in out Writer_Logger_Class;
       Line     : in     Natural;
       Column   : in     Natural;
       Citation : in     String_Type;
       Reason   : in     String) is
   begin
      Note_Level(Self, Line, Column, Citation, Reason, kv.apg.incidents.Debug);
   end Note_Debug;

   ----------------------------------------------------------------------------
   overriding procedure Note_Detail
      (Self     : in out Writer_Logger_Class;
       Line     : in     Natural;
       Column   : in     Natural;
       Citation : in     String_Type;
       Reason   : in     String) is
   begin
      Note_Level(Self, Line, Column, Citation, Reason, kv.apg.incidents.Detail);
   end Note_Detail;

   ----------------------------------------------------------------------------
   overriding procedure Note_Info
      (Self     : in out Writer_Logger_Class;
       Line     : in     Natural;
       Column   : in     Natural;
       Citation : in     String_Type;
       Reason   : in     String) is
   begin
      Note_Level(Self, Line, Column, Citation, Reason, kv.apg.incidents.Information);
   end Note_Info;

   ----------------------------------------------------------------------------
   overriding procedure Note_Warning
      (Self     : in out Writer_Logger_Class;
       Line     : in     Natural;
       Column   : in     Natural;
       Citation : in     String_Type;
       Reason   : in     String) is
   begin
      Note_Level(Self, Line, Column, Citation, Reason, kv.apg.incidents.Warning);
   end Note_Warning;

   ----------------------------------------------------------------------------
   overriding procedure Note_Error
      (Self     : in out Writer_Logger_Class;
       Line     : in     Natural;
       Column   : in     Natural;
       Citation : in     String_Type;
       Reason   : in     String) is
   begin
      Note_Level(Self, Line, Column, Citation, Reason, kv.apg.incidents.Error);
   end Note_Error;

end kv.apg.logger.writer;