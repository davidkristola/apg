with Ada.Strings.Wide_Wide_Unbounded;

package kv.core.wwstr is

   subtype String_Type is Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

   function To_String_Type(S : String) return String_Type;
   function "+"(S : Wide_Wide_String) return String_Type;
   function "+"(U : String_Type) return Wide_Wide_String;

end kv.core.wwstr;
