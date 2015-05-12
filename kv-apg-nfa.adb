with Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

package body kv.apg.nfa is

   Debug : Boolean := False;

   -------------------------------------------------------------------------
   procedure Set_Debug(Value : in Boolean) is
   begin
      Debug := Value;
   end Set_Debug;

   ----------------------------------------------------------------------------
   function Img(Char : Wide_Wide_Character) return String is
      Decimal_Image : constant String := Interfaces.Unsigned_32'IMAGE(Interfaces.Unsigned_32(Wide_Wide_Character'POS(Char)));
   begin
      return Decimal_Image(2..Decimal_Image'LAST);
   end Img;


   ----------------------------------------------------------------------------
   procedure Initialize
      (Self   : in out Nfa_Class;
       Alloc  : in     Positive;
       Preset : in     Boolean := False;
       Key    : in     Key_Type := 0) is
      Last : State_Id_Type := State_Id_Type(Alloc);
   begin
      Self.States := new State_List_Type(1..Last);
      if Preset then
         if Debug then Put_Line("Presetting " & Positive'IMAGE(Alloc) & " states with the last one accepting/key=" & Key_Type'IMAGE(Key)); end if;
         for I in 1..State_Universe_Type(Last) - 1 loop
            Set_Non_Accepting(Self.States(I), I, 0);
         end loop;
         Set_Accepting(Self.States(Last), Last, Key);
      end if;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self     : in out Nfa_Class;
       Existing : in     State_List_Pointer_Type) is
   begin
      Self.States := Existing;
   end Initialize;

   ----------------------------------------------------------------------------
   function Get_State_Count
      (Self : in     Nfa_Class) return Natural is
   begin
      return Self.States'LENGTH;
   end Get_State_Count;

   ----------------------------------------------------------------------------
   procedure Set_State_Accepting
      (Self  : in out Nfa_Class;
       State : in     State_Id_Type;
       Key   : in     Key_Type;
       Alloc : in     Natural := 0) is
   begin
      Set_Accepting(Self.States(State), State, Key, Alloc);
   end Set_State_Accepting;

   ----------------------------------------------------------------------------
   procedure Set_State_Non_Accepting
      (Self  : in out Nfa_Class;
       State : in     State_Id_Type;
       Alloc : in     Natural) is
   begin
      Set_Non_Accepting(Self.States(State), State, Alloc);
   end Set_State_Non_Accepting;

   ----------------------------------------------------------------------------
   procedure Set_State_Transition
      (Self  : in out Nfa_Class;
       State : in     State_Id_Type;
       Index : in     Positive;
       Trans : in     Transition_Type) is
   begin
      Set_Transition(Self.States(State), Index, Trans);
   end Set_State_Transition;

   ----------------------------------------------------------------------------
   procedure Append_State_Transition
      (Self  : in out Nfa_Class;
       State : in     State_Id_Type;
       Trans : in     Transition_Type) is
   begin
      Append_Transition(Self.States(State), Trans);
   end Append_State_Transition;

   ----------------------------------------------------------------------------
   function Recursive_Image(Self : Nfa_Class; Index : Natural) return String is
   begin
      if Index = 1 then
         return Image(Self.States(1));
      else
         return Recursive_Image(Self, Index-1) & "/" & Image(Self.States(State_Id_Type(Index)));
      end if;
   end Recursive_Image;

   ----------------------------------------------------------------------------
   function Get_Start_State(Self : Nfa_Class) return State_Id_Type is
   begin
      return Self.Start;
   end Get_Start_State;

   ----------------------------------------------------------------------------
   function Is_Accepting(Self : Nfa_Class; State : State_Id_Type) return Boolean is
   begin
      return Is_Accepting(Self.States(State));
   end Is_Accepting;

   ----------------------------------------------------------------------------
   function Is_Terminal(Self : Nfa_Class; State : State_Id_Type) return Boolean is
   begin
      return Get_Transition_Count(Self.States(State)) = 0; -- Has no exit transitions
   end Is_Terminal;

   ----------------------------------------------------------------------------
   function Transition_Count(Self : Nfa_Class; State : State_Id_Type) return Natural is
   begin
--      Put_Line("Transition_Count checking " & State_Id_Type'IMAGE(State) & ", transitions = " & Natural'IMAGE(Get_Transition_Count(Self.States(State))));
      return Get_Transition_Count(Self.States(State));
   end Transition_Count;

   ----------------------------------------------------------------------------
   function Image(Self : Nfa_Class) return String is
   begin
      return "[" & Recursive_Image(Self, Self.States'LENGTH) & "]";
   end Image;

   ----------------------------------------------------------------------------
   function Get_Transition(Self : Nfa_Class; State : State_Id_Type; Index : Positive) return Transition_Type is
   begin
      return Get_Transition(Self.States(State), Index);
   end Get_Transition;

   ----------------------------------------------------------------------------
   procedure Mark_Transitions
      (Self  : in     Nfa_Class;
       These : in     Active_State_List_Pointer_Type;
       Next  : in     Active_State_List_Pointer_Type;
       Value : in     Wide_Wide_Character) is
      Count : Natural := 0;
   begin
      if Debug then Put_Line("Mark_Transitions on " & Img(Value)); end if;
      for I in These.all'RANGE loop
         if These(I) then
            Mark_Transitions(Self.States(I), Value, Next, Count);
         end if;
      end loop;
   end Mark_Transitions;

   ----------------------------------------------------------------------------
   procedure Epsilon_Transitions
      (Self  : in     Nfa_Class;
       Next  : in     Active_State_List_Pointer_Type) is
      Active : Natural;
      Previous : Natural := 0;
   begin
      Active := Active_Count(Next);
      while Active /= Previous loop
         Previous := Active;
         for I in Next.all'RANGE loop
            if Next(I) then
               Mark_Epsilon_Transitions(Self.States(I), Next);
            end if;
         end loop;
         Active := Active_Count(Next);
      end loop;
   end Epsilon_Transitions;



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
   procedure Initialize
      (Self : in out Nfa_State_Class;
       Nfa  : in     Nfa_Pointer_Type) is
      State_Count : State_Universe_Type;
   begin
      Self.Nfa := Nfa;
      State_Count := State_Universe_Type(Nfa.Get_State_Count);
      Self.Previous := new Active_State_List_Type(1..State_Count);
      Self.Next := new Active_State_List_Type(1..State_Count);
      Self.Reset;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Reset
      (Self : in out Nfa_State_Class) is
   begin
      if Debug then Put_Line("Nfa_State_Class.Reset"); end if;
      Clear_State_List(Self.Previous);
      Clear_State_List(Self.Next);
      Activate(Self.Previous, Self.Nfa.Get_Start_State);
      Activate(Self.Next, Self.Nfa.Get_Start_State);
      Self.Nfa.Epsilon_Transitions(Self.Previous);
      Self.Nfa.Epsilon_Transitions(Self.Next);
      Self.Moves := 0;
      if Debug then Put_Line("Nfa_State_Class.Reset complete"); end if;
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
      -- Now process epsilon transitions until there are no more to process
      Self.Nfa.Epsilon_Transitions(Self.Next);
      Self.Moves := Self.Moves + 1;
   end Ingest;

   ----------------------------------------------------------------------------
   function Is_Accepting(Self : Nfa_State_Class) return Boolean is
   begin
      return Accepting_Count(Self.Nfa, Self.Next) > 0;
   end;

   ----------------------------------------------------------------------------
   function Is_Terminal(Self : Nfa_State_Class) return Boolean is
   begin
      return For_Each_Active_State_Count_If(Self.Nfa, Self.Next, Count_Terminal_Accepting'ACCESS) > 0; --TODO: what if there are more than one terminal accepting state?
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
   function Move_Count(Self : Nfa_State_Class) return Natural is
   begin
      return Self.Moves;
   end;


end kv.apg.nfa;
