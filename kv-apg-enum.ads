private with Ada.Containers.Doubly_Linked_Lists;

with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.tokens;
with kv.apg.writer;

package kv.apg.enum is

   type Token_Associativity_Type is (Neither, Left, Right);
   type Token_Precedence_Type is new Natural;

   type Enumeration_Class is tagged private;

   procedure Initialize
      (Self : in out Enumeration_Class;
       Name : in     String_Type);

   procedure Append
      (Self : in out Enumeration_Class;
       Name : in     kv.apg.tokens.Token_Class);

   procedure Append
      (Self          : in out Enumeration_Class;
       Name          : in     kv.apg.tokens.Token_Class;
       Associativity : in     Token_Associativity_Type;
       Precedence    : in     Token_Precedence_Type);

   function Get_Count(Self : Enumeration_Class) return Natural;

   procedure Write
      (Self   : in     Enumeration_Class;
       Writer : in out kv.apg.writer.Writer_Class'CLASS);

   function Get(Self : Enumeration_Class; Index : Positive) return String_Type;
   function Get(Self : Enumeration_Class; Index : Positive) return kv.apg.tokens.Token_Class;
   function Get(Self : Enumeration_Class; Name : String_Type) return Integer;
   Not_Found_Error : constant Integer := -1;

   function Get_Associativity(Self : Enumeration_Class; Index : Positive) return Token_Associativity_Type;
   function Get_Precedence(Self : Enumeration_Class; Index : Positive) return Token_Precedence_Type;

private

   type Value_Type is
      record
         Value : Integer;
         Name  : kv.apg.tokens.Token_Class;
         Associativity : Token_Associativity_Type := Neither;
         Precedence : Token_Precedence_Type := 0;
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
