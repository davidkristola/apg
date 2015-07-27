
with kv.core.wwstr; use kv.core.wwstr;

package kv.apg.writer is

   type Writer_Class is abstract tagged null record;

   procedure Write_Line
      (Self : in out Writer_Class;
       Line : in     String_Type) is abstract;
   procedure Write_Line
      (Self : in out Writer_Class;
       Line : in     String) is abstract;

   procedure Write_Some
      (Self : in out Writer_Class;
       Part : in     String_Type) is abstract;
   procedure Write_Some
      (Self : in out Writer_Class;
       Part : in     String) is abstract;

   procedure New_Line
      (Self  : in out Writer_Class;
       Count : in     Positive := 1) is abstract;

end kv.apg.writer;
