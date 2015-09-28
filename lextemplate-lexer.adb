-- This file is machine generated. Do not edit.

with Ada.Characters.Conversions;

with «package_name».Static_Nfa_Definition;

package body «package_name».Lexer is

   use «package_name».Static_Nfa_Definition;

   ----------------------------------------------------------------------------
   procedure Clear_State_List
      (List : in     Active_State_List_Pointer_Type) is
   begin
      for I in List.all'RANGE loop
         List(I) := False;
      end loop;
   end Clear_State_List;

   ----------------------------------------------------------------------------
   procedure Activate
      (List  : in     Active_State_List_Pointer_Type;
       State : in     State_Id_Type) is
   begin
      List(State) := True;
   end Activate;



   ----------------------------------------------------------------------------
   type State_Check_Function_Pointer_Type is access function(Nfa : Nfa_Pointer_Type; State : State_Id_Type) return Boolean;

   ----------------------------------------------------------------------------
   function For_Each_Active_State_Count_If
      (Nfa    : in     Nfa_Pointer_Type;
       Active : in     Active_State_List_Pointer_Type;
       Check  : in     State_Check_Function_Pointer_Type) return Natural is
      Count : Natural := 0;
   begin
      for State in Active.all'RANGE loop
         if Active(State) then
            if Check(Nfa, State) then
               Count := Count + 1;
            end if;
         end if;
      end loop;
      return Count;
   end For_Each_Active_State_Count_If;


   ----------------------------------------------------------------------------
   function Count_Me(Nfa : Nfa_Pointer_Type; State : State_Id_Type) return Boolean is
   begin
      return True;
   end Count_Me;

   ----------------------------------------------------------------------------
   function Active_Count
      (List : in     Active_State_List_Pointer_Type) return Natural is
   begin
      return For_Each_Active_State_Count_If(null, List, Count_Me'ACCESS);
   end Active_Count;

   ----------------------------------------------------------------------------
   function Count_Accepting(Nfa : Nfa_Pointer_Type; State : State_Id_Type) return Boolean is
   begin
      return Nfa.Is_Accepting(State);
   end Count_Accepting;

   ----------------------------------------------------------------------------
   function Count_Terminal_Accepting(Nfa : Nfa_Pointer_Type; State : State_Id_Type) return Boolean is
   begin
      return Nfa.Is_Accepting(State) and then Nfa.Is_Terminal(State);
   end Count_Terminal_Accepting;

   ----------------------------------------------------------------------------
   function Accepting_Count
      (Nfa    : in     Nfa_Pointer_Type;
       Active : in     Active_State_List_Pointer_Type) return Natural is
   begin
      return For_Each_Active_State_Count_If(Nfa, Active, Count_Accepting'ACCESS);
   end Accepting_Count;

   ----------------------------------------------------------------------------
   function Transition_Count
      (Self : in     Nfa_State_Class;
       List : in     Active_State_List_Pointer_Type) return Natural is
      Count : Natural := 0;
   begin
      for I in List.all'RANGE loop
         if List(I) then
            Count := Count + Self.Nfa.Transition_Count(I);
         end if;
      end loop;
      return Count;
   end Transition_Count;




   ----------------------------------------------------------------------------
   procedure Reset
      (Self : in out Nfa_State_Class) is
   begin
      Clear_State_List(Self.Previous);
      Clear_State_List(Self.Next);
      Activate(Self.Previous, Self.Nfa.Get_Start_State);
      Activate(Self.Next, Self.Nfa.Get_Start_State);
   end Reset;

   ----------------------------------------------------------------------------
   procedure Swap_Current_And_Previous
      (Self  : in out Nfa_State_Class) is
      Temp : Active_State_List_Pointer_Type;
   begin
      Temp := Self.Previous;
      Self.Previous := Self.Next;
      Self.Next := Temp;
   end Swap_Current_And_Previous;

   ----------------------------------------------------------------------------
   procedure Ingest
      (Self  : in out Nfa_State_Class;
       Input : in     Wide_Wide_Character) is
   begin
      Swap_Current_And_Previous(Self);
      Clear_State_List(Self.Next);
      Self.Nfa.Mark_Transitions(Self.Previous, Self.Next, Input);
   end Ingest;

   ----------------------------------------------------------------------------
   function Is_Accepting(Self : Nfa_State_Class) return Boolean is
   begin
      return Accepting_Count(Self.Nfa, Self.Next) > 0;
   end;

   ----------------------------------------------------------------------------
   function Is_Terminal(Self : Nfa_State_Class) return Boolean is
   begin
      return For_Each_Active_State_Count_If(Self.Nfa, Self.Next, Count_Terminal_Accepting'ACCESS) > 0;
   end;

   ----------------------------------------------------------------------------
   function Is_Failed(Self : Nfa_State_Class) return Boolean is
   begin
      return Active_Count(Self.Next) = 0;
   end;

   ----------------------------------------------------------------------------
   function Active_State_Count(Self : Nfa_State_Class) return Natural is
   begin
      return Active_Count(Self.Next);
   end Active_State_Count;

   ----------------------------------------------------------------------------
   function Get_Token(Self : Nfa_State_Class) return Token_Type is
   begin
      for S in Self.Next'RANGE loop
         if Self.Next(S) then
            return Self.Nfa.Get_Token(S);
         end if;
      end loop;
      return Token_Type'FIRST;
   end Get_Token;




   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Nfa_State_Class) is
      State_Count : State_Universe_Type;
   begin
      Self.Nfa := Nfa_Definition'ACCESS;
      State_Count := State_Universe_Type(Self.Nfa.Get_State_Count);
      Self.Previous := new Active_State_List_Type(1..State_Count);
      Self.Next := new Active_State_List_Type(1..State_Count);
      Self.Reset;
   end Initialize;

end «package_name».Lexer;

