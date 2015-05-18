with Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

package body kv.apg.fa.dfa is

   Debug : Boolean := False;

   -------------------------------------------------------------------------
   procedure Set_Debug(Value : in Boolean) is
   begin
      Debug := Value;
      if Debug then Put_Line("kv.apg.dfa debug turned on"); end if;
   end Set_Debug;



   ----------------------------------------------------------------------------
   function Compute_Transitions
      (Self  : in     Dfa_Class;
       State : in     State_Id_Type;
       Value : in     Wide_Wide_Character) return State_Universe_Type is
   begin
      if Debug then Put_Line("Compute_Transitions on " & Img(Value)); end if;
      return Compute_Transitions(Self.States(State), Value);
   end Compute_Transitions;






   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Dfa_State_Class;
       Dfa  : in     Dfa_Pointer_Type) is
   begin
      Self.Dfa := Dfa;
      Self.Reset;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Reset
      (Self : in out Dfa_State_Class) is
   begin
      if Debug then Put_Line("Dfa_State_Class.Reset"); end if;
      Self.State := Self.Dfa.Get_Start_State;
      Self.Moves := 0;
      if Debug then Put_Line("Dfa_State_Class.Reset complete"); end if;
   end Reset;

   ----------------------------------------------------------------------------
   procedure Ingest
      (Self  : in out Dfa_State_Class;
       Input : in     Wide_Wide_Character) is
   begin
      if Debug then Put_Line("Dfa_State_Class.Ingest (state = "&State_Universe_Type'IMAGE(Self.State)&") input " & Img(Input)); end if;
      if Self.Is_Failed then
         return;
      end if;
      Self.State := Self.Dfa.Compute_Transitions(Self.State, Input);
      Self.Moves := Self.Moves + 1;
   end Ingest;

   ----------------------------------------------------------------------------
   function Is_Accepting(Self : Dfa_State_Class) return Boolean is
   begin
      if Self.Is_Failed then
         return False;
      end if;
      return Self.Dfa.Is_Accepting(Self.State);
   end Is_Accepting;

   ----------------------------------------------------------------------------
   function Is_Terminal(Self : Dfa_State_Class) return Boolean is
   begin
      if Self.Is_Failed then
         return True;
      end if;
      return Self.Dfa.Is_Terminal(Self.State);
   end Is_Terminal;

   ----------------------------------------------------------------------------
   function Is_Failed(Self : Dfa_State_Class) return Boolean is
   begin
      return Self.State = Invalid_State;
   end Is_Failed;

   ----------------------------------------------------------------------------
   function Move_Count(Self : Dfa_State_Class) return Natural is
   begin
      return Self.Moves;
   end Move_Count;

   ----------------------------------------------------------------------------
   function Get_Key(Self : Dfa_State_Class) return Key_Type is
   begin
      if Self.Is_Failed then
         return Key_Type'FIRST;
      end if;
      return Self.Dfa.Get_Key(Self.State);
   end Get_Key;



end kv.apg.fa.dfa;
