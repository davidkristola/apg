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

package kv.apg.rules is

   use kv.apg.tokens;
   use Ada.Strings.Wide_Wide_Unbounded;

   type Rule_Class;
   type Rule_Pointer is access all Rule_Class;

   type Element_Class is tagged
      record
         Token : kv.apg.tokens.Token_Class;
      end record;
   type Terminal_Class is new Element_Class with
      record
         Key : kv.apg.fast.Key_Type;
      end record;
   type Non_Terminal_Class is new Element_Class with
      record
         Rule : Rule_Pointer;
      end record;
   type Pre_Element_Class is new Element_Class with null record;

   type Element_Pointer is access all Element_Class'CLASS;


   function New_Pre_Element
      (Token : in     kv.apg.tokens.Token_Class) return Element_Pointer;

   procedure Free
      (Element : in out Element_Pointer);

   package Element_Vector is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Element_Pointer);


   type Production_Class is tagged
      record
         Elements : Element_Vector.Vector; -- predicates?
         Code     : String_Type;
      end record;

   procedure Append
      (Self    : in out Production_Class;
       Element : in     Element_Pointer);

   function Image(Self : in out Production_Class) return String_Type;

   procedure Clear
      (Self : in out Production_Class);


   package Production_Vector is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Production_Class);




   type Rule_Class is tagged
      record
         Name_Token  : kv.apg.tokens.Token_Class;
         Productions : Production_Vector.Vector;
         Start_Rule  : Boolean := False;
      end record;

   procedure Initialize
      (Self        : in out Rule_Class;
       Name        : in     kv.apg.tokens.Token_Class;
       Productions : in     Production_Vector.Vector);

   procedure Set_Is_Start
      (Self     : in out Rule_Class;
       Is_Start : in     Boolean);

   function Is_Start(Self : Rule_Class) return Boolean;

   function Get_Name(Self : Rule_Class) return String_Type;



   package Rule_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Rule_Pointer,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");


   type Grammar_Class is tagged
      record
         Tokens : kv.apg.enum.Enumeration_Class;
         Rules  : Rule_Maps.Map;
         Start  : String_Type;
      end record;

   procedure Initialize
      (Self   : in out Grammar_Class;
       Tokens : in     kv.apg.enum.Enumeration_Class);

   procedure Add_Rule
      (Self : in out Grammar_Class;
       Rule : in     Rule_Pointer);

   procedure Validate
      (Self   : in out Grammar_Class;
       Logger : in out kv.apg.logger.Safe_Logger_Pointer);

   function Find(Self : Grammar_Class; Name : String_Type) return Rule_Pointer;

end kv.apg.rules;
