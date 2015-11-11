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

   type Element_Pointer is access all Element_Class'CLASS;

   package Element_Vector is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Element_Pointer);


   type Production_Type is
      record
         Elements : Element_Vector.Vector;
         Code     : String_Type;
      end record;


   package Production_Vector is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Production_Type);




   type Rule_Class is tagged
      record
         Name_Token  : kv.apg.tokens.Token_Class;
         Productions : Production_Vector.Vector;
      end record;


   package Rule_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type => String,
       Element_Type => Rule_Class,
       Hash => Ada.Strings.Hash,
       Equivalent_Keys => "=");

procedure doit;

end kv.apg.rules;
