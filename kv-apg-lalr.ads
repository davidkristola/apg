with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings;
with Ada.Strings.Hash;
with Ada.Strings.Wide_Wide_Unbounded;

with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.tokens;
with kv.apg.fast;
with kv.apg.enum;
with kv.apg.logger;

package kv.apg.lalr is

   use kv.apg.tokens;
   use Ada.Strings.Wide_Wide_Unbounded;


   Unresolved_Error : exception;
   Rule_Not_Found_Error : exception;
   Production_Not_Found_Error : exception;
   Dot_Position_Error : exception;
   Terminal_Expected_Error : exception;
   Non_Following_Terminal_Error : exception;


   End_Of_File : constant := -1;
   Epsilon     : constant := 0;
   Terminal_1  : constant := 1;

   type Terminal_Index_Type is range End_Of_File .. Integer'LAST;
   type Non_Terminal_Index_Type is new Positive; -- Rule index

   function Img(Arg : Terminal_Index_Type) return String;

   package Terminal_Sets is new Ada.Containers.Ordered_Sets(Terminal_Index_Type);



   type Symbol_Class is abstract tagged private;
   type Symbol_Pointer is access all Symbol_Class'CLASS;
   type Constant_Symbol_Pointer is access constant Symbol_Class'CLASS;

   function Name(Self : Symbol_Class) return String_Type;

   function Is_Terminal(Self : Symbol_Class) return Boolean is abstract;
   function Is_Same_As(Self : Symbol_Class; Other : Symbol_Class'CLASS) return Boolean is abstract;
   function Get_Number(Self : Symbol_Class) return Terminal_Index_Type is abstract;
   function Get_Index(Self : Symbol_Class) return Integer is abstract;



   type Terminal_Class is new Symbol_Class with private;

   function End_Of_File_Terminal return Terminal_Class;
   function New_End_Of_File_Terminal return Constant_Symbol_Pointer;
   function Epsilon_Terminal return Terminal_Class;

   function Is_Terminal(Self : Terminal_Class) return Boolean is (True);
   function Is_Same_As(Self : Terminal_Class; Other : Symbol_Class'CLASS) return Boolean;
   function Get_Number(Self : Terminal_Class) return Terminal_Index_Type;
   function Get_Index(Self : Terminal_Class) return Integer;


   type Non_Terminal_Class is new Symbol_Class with private;

   function Is_Terminal(Self : Non_Terminal_Class) return Boolean is (False);
   function Is_Same_As(Self : Non_Terminal_Class; Other : Symbol_Class'CLASS) return Boolean;
   function Get_Number(Self : Non_Terminal_Class) return Terminal_Index_Type; -- will raise Terminal_Expected_Error
   function Get_Index(Self : Non_Terminal_Class) return Integer;

   function New_Non_Terminal_Symbol(Token : kv.apg.tokens.Token_Class; Rule : Non_Terminal_Index_Type) return Constant_Symbol_Pointer;


   type Pre_Symbol_Class is new Symbol_Class with private;

   function New_Pre_Symbol
      (Token : in     kv.apg.tokens.Token_Class) return Constant_Symbol_Pointer;

   procedure Free
      (Symbol : in out Constant_Symbol_Pointer);

   -- These will all raise Unresolved_Error
   function Is_Terminal(Self : Pre_Symbol_Class) return Boolean;
   function Is_Same_As(Self : Pre_Symbol_Class; Other : Symbol_Class'CLASS) return Boolean;
   function Get_Number(Self : Pre_Symbol_Class) return Terminal_Index_Type;
   function Get_Index(Self : Pre_Symbol_Class) return Integer;






   function Equal(L, R : Constant_Symbol_Pointer) return Boolean;

   package Symbol_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Constant_Symbol_Pointer,
       "=" => Equal);


private

   type Symbol_Class is abstract tagged
      record
         Token : kv.apg.tokens.Token_Class;
      end record;

   type Terminal_Class is new Symbol_Class with
      record
         Key : kv.apg.fast.Key_Type;
      end record;

   type Non_Terminal_Class is new Symbol_Class with
      record
         Rule_Number : Non_Terminal_Index_Type;
      end record;

   type Pre_Symbol_Class is new Symbol_Class with null record;

end kv.apg.lalr;
