with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.UTF_Encoding;

package kv.core.wwstr is

   subtype String_Type is Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

   function To_String_Type(S : String) return String_Type;
   function "+"(S : Wide_Wide_String) return String_Type;
   function "+"(U : String_Type) return Wide_Wide_String;
   function To_String(S : String_Type) return String;

   function To_WWS(S : Ada.Strings.UTF_Encoding.UTF_8_String) return Wide_Wide_String;
   function To_UTF(S : Wide_Wide_String) return Ada.Strings.UTF_Encoding.UTF_8_String;
   function To_UTF(S : String_Type) return Ada.Strings.UTF_Encoding.UTF_8_String;

end kv.core.wwstr;
