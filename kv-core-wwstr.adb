with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body kv.core.wwstr is

   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Ada.Characters.Conversions;


   ----------------------------------------------------------------------------
   function To_String_Type(S : String) return String_Type is
      WS : constant Wide_Wide_String := To_Wide_Wide_String(S);
   begin
      return +WS;
   end To_String_Type;

   ----------------------------------------------------------------------------
   function "+"(S : Wide_Wide_String) return String_Type renames Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String;

   ----------------------------------------------------------------------------
   function "+"(U : String_Type) return Wide_Wide_String renames Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String;


   ----------------------------------------------------------------------------
   function To_WWS(S : Ada.Strings.UTF_Encoding.UTF_8_String) return Wide_Wide_String renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode;

   ----------------------------------------------------------------------------
   -- Can't use a rename.
   function To_UTF(S : Wide_Wide_String) return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      return Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode(S);
   end To_UTF;

end kv.core.wwstr;
