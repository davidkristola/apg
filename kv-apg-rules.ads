with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings;
with Ada.Strings.Hash;

with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.tokens;
with kv.apg.fast;

package kv.apg.rules is

   use kv.apg.tokens;

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


   package Element_Vector is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Element_Pointer);


   type Production_Class is tagged
      record
         Elements : Element_Vector.Vector;
         Code     : String_Type;
      end record;

   procedure Append
      (Self    : in out Production_Class;
       Element : in     Element_Pointer);


   package Production_Vector is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Production_Class);




   type Rule_Class is tagged
      record
         Name_Token  : kv.apg.tokens.Token_Class;
         Productions : Production_Vector.Vector;
      end record;

   procedure Initialize
      (Self        : in out Rule_Class;
       Name        : in     kv.apg.tokens.Token_Class;
       Productions : in     Production_Vector.Vector);


   package Rule_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type => String,
       Element_Type => Rule_Class,
       Hash => Ada.Strings.Hash,
       Equivalent_Keys => "=");


end kv.apg.rules;
