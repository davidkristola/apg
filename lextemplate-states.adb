-- This file is machine generated. Do not edit.

with Ada.Characters.Conversions;

package body «package_name».States is

   use Ada.Characters.Conversions;

   function Move(Self : in Transition_Type; Check : in Wide_Wide_Character) return State_Universe_Type is
   begin
      case Self.Criteria is
         when Any =>
            return Self.To_State;
         when Match =>
            if Check = Self.Value then
               return Self.To_State;
            else
               return Invalid_State;
            end if;
         when From_To =>
            if Check in Self.Lower .. Self.Upper then
               return Self.To_State;
            else
               return Invalid_State;
            end if;
      end case;
   end Move;

   procedure Mark_Transitions
      (Self  : in     State_Type;
       Value : in     Wide_Wide_Character;
       Next  : in     Active_State_List_Pointer_Type;
       Count :    out Natural) is
      Destination : State_Universe_Type;
   begin
      Count := 0;
      if Self.Transitions = null then
         return;
      end if;
      for I in Self.Transitions'RANGE loop
         Destination := Move(Self.Transitions(I), Value);
         if Destination /= Invalid_State then
            Count := Count + 1;
            Next(Destination) := True;
         end if;
      end loop;
   end Mark_Transitions;

   function Get_Id(Self : State_Type) return State_Universe_Type is
   begin
      return Self.Id;
   end Get_Id;

   function Is_Accepting(Self : State_Type) return Boolean is
   begin
      return Self.Accepting;
   end Is_Accepting;

   function Get_Token(Self : State_Type) return Token_Type is
   begin
      return Self.Accepted_Token;
   end Get_Token;

   function Get_Transition_Count(Self : State_Type) return Natural is
   begin
      if Self.Transitions = null then
         return 0;
      else
         return Self.Transitions'LENGTH;
      end if;
   end Get_Transition_Count;

   function Get_Transition(Self : State_Type; Index : Positive) return Transition_Type is
   begin
      return Self.Transitions(Index);
   end Get_Transition;




end «package_name».States;
