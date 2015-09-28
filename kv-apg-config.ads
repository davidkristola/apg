private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash;

with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.parse;

package kv.apg.config is

   type Key_Value_Map_Class is tagged private;

   procedure Initialize
      (Self   : in out Key_Value_Map_Class;
       Parser : in     kv.apg.parse.Parser_Pointer_Type);

   function Has_Key(Self : Key_Value_Map_Class; Key : String) return Boolean;

   function Get_Value(Self : Key_Value_Map_Class; Key : String) return String;

private

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type => String,
       Element_Type => String,
       Hash => Ada.Strings.Hash,
       Equivalent_Keys => "=");

   type Key_Value_Map_Class is tagged
      record
         Store : aliased String_Maps.Map;
      end record;

end kv.apg.config;
