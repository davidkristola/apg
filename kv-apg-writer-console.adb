with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Wide_Wide_Unbounded;

package body kv.apg.writer.console is

   use Ada.Strings.Wide_Wide_Unbounded;

   ----------------------------------------------------------------------------
   overriding procedure Write_Line
      (Self : in out Console_Writer_Class;
       Line : in     String_Type) is
   begin
      Put_Line(To_UTF(Line));
   end Write_Line;

   ----------------------------------------------------------------------------
   overriding procedure Write_Line
      (Self : in out Console_Writer_Class;
       Line : in     String) is
   begin
      Put_Line(Line);
   end Write_Line;

   ----------------------------------------------------------------------------
   overriding procedure Write_Some
      (Self : in out Console_Writer_Class;
       Part : in     String_Type) is
   begin
      Put(To_UTF(Part));
   end Write_Some;

   ----------------------------------------------------------------------------
   overriding procedure Write_Some
      (Self : in out Console_Writer_Class;
       Part : in     String) is
   begin
      Put(Part);
   end Write_Some;

   ----------------------------------------------------------------------------
   overriding procedure New_Line
      (Self  : in out Console_Writer_Class;
       Count : in     Positive := 1) is
   begin
      New_Line(Positive_Count(Count));
   end New_Line;

end kv.apg.writer.console;
