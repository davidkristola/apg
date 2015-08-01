private with Ada.Containers.Doubly_Linked_Lists;

with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.writer;

package kv.apg.enum is

   type Enumeration_Class is tagged private;

   procedure Initialize
      (Self : in out Enumeration_Class;
       Name : in     String_Type);

   procedure Append
      (Self : in out Enumeration_Class;
       Name : in     String_Type);

   function Get_Count(Self : Enumeration_Class) return Natural;

   procedure Write
      (Self   : in     Enumeration_Class;
       Writer : in out kv.apg.writer.Writer_Class'CLASS);

private

   type Value_Type is
      record
         Value : Integer;
         Name  : String_Type;
      end record;

   package Value_List is new Ada.Containers.Doubly_Linked_Lists(Value_Type);

   type Enumeration_Class is tagged
      record
         Name   : String_Type;
         Values : Value_List.List;
         First  : Integer;
         Last   : Integer;
      end record;

end kv.apg.enum;
