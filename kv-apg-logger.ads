with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.locations;
with kv.apg.incidents;

package kv.apg.logger is

   type Logger_Class is interface;
   type Logger_Pointer is access all Logger_Class'CLASS;
   type Safe_Logger_Pointer is not null access all Logger_Class'CLASS;
   procedure Note_By_Severity
      (Self        : in out Logger_Class;
       Severity    : in     kv.apg.incidents.Severity_Type;
       Information : in     String) is abstract;
   procedure Note_By_Severity
      (Self     : in out Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String;
       Severity : in     kv.apg.incidents.Severity_Type) is abstract;
   procedure Note_Debug
      (Self     : in out Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String) is abstract;
   procedure Note_Detail
      (Self     : in out Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String) is abstract;
   procedure Note_Info
      (Self     : in out Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String) is abstract;
   procedure Note_Warning
      (Self     : in out Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String) is abstract;
   procedure Note_Error
      (Self     : in out Logger_Class;
       Location : in     kv.apg.locations.File_Location_Type;
       Citation : in     String_Type;
       Reason   : in     String) is abstract;

end kv.apg.logger;
