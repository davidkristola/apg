-- This file is machine generated. Do not edit.

with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.UTF_Encoding;

with «package_name».States;
with «package_name».Nfa;

package «package_name».Lexer is

   use «package_name».States;
   use «package_name».Nfa;

   type Nfa_State_Class is tagged
      record
         Nfa      : Nfa_Pointer_Type;
         Previous : Active_State_List_Pointer_Type;
         Next     : Active_State_List_Pointer_Type;
      end record;

   procedure Initialize
      (Self : in out Nfa_State_Class);

   procedure Reset
      (Self : in out Nfa_State_Class);

   procedure Ingest
      (Self  : in out Nfa_State_Class;
       Input : in     Wide_Wide_Character);

   function Is_Accepting(Self : Nfa_State_Class) return Boolean;
   function Is_Terminal(Self : Nfa_State_Class) return Boolean;
   function Is_Failed(Self : Nfa_State_Class) return Boolean;
   function Get_Token(Self : Nfa_State_Class) return Token_Type;


end «package_name».Lexer;
