-- This file is machine generated. Do not edit.

with Ada.Characters.Conversions;

package body «package_name».Nfa is

   procedure Initialize
      (Self     : in out Nfa_Class;
       Existing : in     State_List_Pointer_Type) is
   begin
      Self.States := Existing;
   end Initialize;

   procedure Mark_Transitions
      (Self  : in     Nfa_Class;
       These : in     Active_State_List_Pointer_Type;
       Next  : in     Active_State_List_Pointer_Type;
       Value : in     Wide_Wide_Character) is
      Count : Natural := 0;
   begin
      for I in These.all'RANGE loop
         if These(I) then
            Mark_Transitions(Self.States(I), Value, Next, Count);
         end if;
      end loop;
   end Mark_Transitions;

   ----------------------------------------------------------------------------
   function Get_State_Count(Self : Nfa_Class) return Natural is
   begin
      if Self.States = null then
         return 0;
      else
         return Self.States'LENGTH;
      end if;
   end Get_State_Count;

   ----------------------------------------------------------------------------
   function Get_Start_State(Self : Nfa_Class) return State_Id_Type is
   begin
      return Self.Start;
   end Get_Start_State;

   function Is_Accepting(Self : Nfa_Class; State : State_Id_Type) return Boolean is
   begin
      return Is_Accepting(Self.States(State));
   end Is_Accepting;

   function Transition_Count(Self : Nfa_Class; State : State_Id_Type) return Natural is
   begin
      return Get_Transition_Count(Self.States(State));
   end Transition_Count;

   function Is_Terminal(Self : Nfa_Class; State : State_Id_Type) return Boolean is
   begin
      return Get_Transition_Count(Self.States(State)) = 0; -- Has no exit transitions
   end Is_Terminal;

   function Get_Token(Self : Nfa_Class; State : State_Id_Type) return Token_Type is
   begin
      return Get_Token(Self.States(State));
   end Get_Token;


end «package_name».Nfa;

