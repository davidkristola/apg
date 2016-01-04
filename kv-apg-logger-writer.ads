with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.writer;
with kv.apg.incidents;
private with kv.apg.locations.factories;

package kv.apg.logger.writer is

   type Writer_Logger_Class is new Logger_Class with private;
   not overriding procedure Initialize
      (Self   : in out Writer_Logger_Class;
       Writer : not null access kv.apg.writer.Writer_Class'CLASS;
       Level  : in     kv.apg.incidents.Severity_Type);
   overriding procedure Note_By_Severity
      (Self        : in out Writer_Logger_Class;
       Severity    : in     kv.apg.incidents.Severity_Type;
       Information : in     String);
   overriding procedure Note_By_Severity
      (Self     : in out Writer_Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String;
       Severity : in     kv.apg.incidents.Severity_Type);
   overriding procedure Note_Debug
      (Self     : in out Writer_Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String);
   overriding procedure Note_Detail
      (Self     : in out Writer_Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String);
   overriding procedure Note_Info
      (Self     : in out Writer_Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String);
   overriding procedure Note_Warning
      (Self     : in out Writer_Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String);
   overriding procedure Note_Error
      (Self     : in out Writer_Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String);

private

   type Writer_Logger_Class is new Logger_Class with
      record
         Reporter : kv.apg.incidents.Writer_Report_Class;
      end record;

end kv.apg.logger.writer;
