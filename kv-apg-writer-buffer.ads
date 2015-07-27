private with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded;

with kv.core.wwstr; use kv.core.wwstr;

package kv.apg.writer.buffer is

   type Buffer_Writer_Class is new Writer_Class with private;

   overriding procedure Write_Line
      (Self : in out Buffer_Writer_Class;
       Line : in     String_Type);
   overriding procedure Write_Line
      (Self : in out Buffer_Writer_Class;
       Line : in     String);

   overriding procedure Write_Some
      (Self : in out Buffer_Writer_Class;
       Part : in     String_Type);
   overriding procedure Write_Some
      (Self : in out Buffer_Writer_Class;
       Part : in     String);

   overriding procedure New_Line
      (Self  : in out Buffer_Writer_Class;
       Count : in     Positive := 1);

   not overriding function Line_Count
      (Self : Buffer_Writer_Class) return Natural;

   not overriding function Get_Line
      (Self : Buffer_Writer_Class;
       Line : Positive) return String_Type;

private

   package String_Vector is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => String_Type,
       "=" => Ada.Strings.Wide_Wide_Unbounded."=");

   type Buffer_Writer_Class is new Writer_Class with
      record
         Unfinished : String_Type;
         Lines      : String_Vector.Vector;
      end record;

end kv.apg.writer.buffer;
