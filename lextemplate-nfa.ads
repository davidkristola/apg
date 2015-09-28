-- This file is machine generated. Do not edit.

with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.UTF_Encoding;

with «package_name».States;

package «package_name».Nfa is

   use «package_name».States;

   type Nfa_Class is tagged
      record
         Start  : State_Id_Type := State_Id_Type'FIRST;
         States : State_List_Pointer_Type;
      end record;
   type Nfa_Pointer_Type is access constant Nfa_Class;

   procedure Initialize
      (Self     : in out Nfa_Class;
       Existing : in     State_List_Pointer_Type);
   procedure Mark_Transitions
      (Self  : in     Nfa_Class;
       These : in     Active_State_List_Pointer_Type;
       Next  : in     Active_State_List_Pointer_Type;
       Value : in     Wide_Wide_Character);

   function Get_State_Count(Self : Nfa_Class) return Natural;
   function Get_Start_State(Self : Nfa_Class) return State_Id_Type;
   function Is_Accepting(Self : Nfa_Class; State : State_Id_Type) return Boolean;
   function Is_Terminal(Self : Nfa_Class; State : State_Id_Type) return Boolean;
   function Transition_Count(Self : Nfa_Class; State : State_Id_Type) return Natural;
   function Get_Token(Self : Nfa_Class; State : State_Id_Type) return Token_Type;


end «package_name».Nfa;

