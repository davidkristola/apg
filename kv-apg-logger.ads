with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.locations;

package kv.apg.logger is

   type Logger_Class is interface;
   type Logger_Pointer is access all Logger_Class'CLASS;
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
