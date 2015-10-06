with kv.core.wwstr; use kv.core.wwstr;

package kv.apg.writer.console is

   type Console_Writer_Class is new Writer_Class with null record;

   overriding procedure Write_Line
      (Self : in out Console_Writer_Class;
       Line : in     String_Type);
   overriding procedure Write_Line
      (Self : in out Console_Writer_Class;
       Line : in     String);

   overriding procedure Write_Some
      (Self : in out Console_Writer_Class;
       Part : in     String_Type);
   overriding procedure Write_Some
      (Self : in out Console_Writer_Class;
       Part : in     String);

   overriding procedure New_Line
      (Self  : in out Console_Writer_Class;
       Count : in     Positive := 1);

end kv.apg.writer.console;
