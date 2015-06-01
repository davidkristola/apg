with Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

package body kv.apg.fa.nfa is

   Debug : Boolean := False;

   -------------------------------------------------------------------------
   procedure Set_Debug(Value : in Boolean) is
   begin
      Debug := Value;
   end Set_Debug;

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
      (Self    : in out Nfa_Class;
       Combine : in     Nfa_Array_Type) is

      Total_States : Positive := 1;
      Offset : Positive := 1;
      To_Start : Transition_Type;
      Current : State_Id_Type;
      Index : Natural := 0;

   begin
      if Debug then Put_Line("Initialize(Combine)"); end if;
      for I of Combine loop
         Total_States := Total_States + I.Get_State_Count;
      end loop;
      Self.States := new State_List_Type(1..State_Id_Type(Total_States));
      if Debug then Put_Line("Total_States = " & Positive'IMAGE(Total_States)); end if;

      -- This is the epsilon transition to all start nodes node
      Set_Non_Accepting(Self => Self.States(1), Id => 1, Alloc => Combine'LENGTH);

      for I of Combine loop
         if Debug then Put_Line("Combining <" & I.Image & ">"); end if;
         Current := State_Id_Type(Offset + 1);
         Index := Index + 1;
         Set_Epsilon(To_Start, Current);
         Set_Transition(Self.States(1), Index, To_Start);
         if Debug then Put_Line("Current = " & State_Id_Type'IMAGE(Current) & ", Offset = " & Positive'IMAGE(Offset)); end if;
         for J of I.States.all loop
            if Debug then Put_Line("deep copy <" & Image(J) & ">"); end if;
            Init_By_Deep_Copy(Self.States(Current), J);
            Renumber(Self.States(Current), Offset);
            if Debug then Put_Line("yields Self.States("&State_Id_Type'IMAGE(Current)&") = <" & Image(Self.States(Current)) & ">"); end if;
            Current := Current + 1;
         end loop;
         if Debug then Put_Line("end loop"); end if;
         Offset := Offset + I.Get_State_Count;
         if Debug then Put_Line("Next Offset will be " & Positive'IMAGE(Offset)); end if;
      end loop;
   end Initialize;

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
   not overriding function Get_States(Self : Nfa_Class) return State_List_Pointer_Type is
   begin
      return Self.States;
   end Get_States;





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
   end Move_Count;

   ----------------------------------------------------------------------------
   function Get_Key(Self : Nfa_State_Class) return Key_Type is
   begin
      for S in Self.Next'RANGE loop
         if Self.Next(S) then
            return Self.Nfa.Get_Key(S);
         end if;
      end loop;
      return Key_Type'FIRST;
   end Get_Key;







   ----------------------------------------------------------------------------
   procedure Append_State
      (List  : in out State_Vector.Vector;
       State : in     State_Type) is

      Working_State : Working_State_Type;

   begin
      if Debug then Put_Line("Processing in state " & Image(State)); end if;
      Working_State.State := State;
      if State.Transitions /= null then
         for Transition of State.Transitions.all loop
            Working_State.Trans.Append(Transition);
         end loop;
      end if;
      List.Append(Working_State);
   end Append_State;

   ----------------------------------------------------------------------------
   function Convert_To_Vector_Form
      (Nfa_States : State_List_Pointer_Type) return State_Vector.Vector is

      Working_States : State_Vector.Vector;

   begin
      pragma Assert(Nfa_States /= null, "NFA must not be empty!");
      for State of Nfa_States.all loop
         Append_State(Working_States, State);
      end loop;
      return Working_States;
   end Convert_To_Vector_Form;


   ----------------------------------------------------------------------------
   procedure Copy_State
      (List  : in     State_List_Pointer_Type;
       Index : in     State_Universe_Type;
       State : in     Working_State_Type) is

      Trans_Index : Positive;

   begin
      List(Index) := State.State;
      pragma Assert(List(Index).Id = Index, "State ID does not match it's index!");
      if not State.Trans.Is_Empty then
         List(Index).Transitions := new Transition_List_Type(1..Positive(State.Trans.Length));
         Trans_Index := 1;
         for T of State.Trans loop
            List(Index).Transitions(Trans_Index) := T;
            Trans_Index := Trans_Index + 1;
         end loop;
      end if;
   end Copy_State;

   ----------------------------------------------------------------------------
   function Convert_To_Array_Form
      (Working_States : State_Vector.Vector) return State_List_Pointer_Type is

      List : State_List_Pointer_Type := new State_List_Type(1..State_Universe_Type(Working_States.Length));
      State_Index : State_Universe_Type := 1;

   begin
      for State of Working_States loop
         Copy_State(List, State_Index, State);
         State_Index := State_Index + 1;
      end loop;
      return List;
   end Convert_To_Array_Form;





   ----------------------------------------------------------------------------
   function Equals(L, R : Working_State_Type) return Boolean is
   begin
      -- TODO: Trust that the IDs are unique?
      return L.State.Id = R.State.Id;
   end Equals;

   ----------------------------------------------------------------------------
   function Trans_Img(Transitions : Transition_Vector.Vector; Index : Natural) return String is
   begin
      if Index = 1 then
         return Image(Transitions(1));
      else
         return Trans_Img(Transitions, Index-1) & "," & Image(Transitions(Index));
      end if;
   end Trans_Img;

   ----------------------------------------------------------------------------
   function Image(Self : Working_State_Type) return String is
      function State_Part return String is
      begin
         if Self.State.Accepting then
            return "(" & Img(Self.State.Id) & ":" & Img(Self.State.Accepted_Key) & ")";
         else
            return Img(Self.State.Id);
         end if;
      end State_Part;
      function Transition_Part return String is
      begin
         if Self.Trans.Is_Empty then
            return "";
         else
            return Trans_Img(Self.Trans, Natural(Self.Trans.Length));
         end if;
      end Transition_Part;
   begin
      return State_Part & "{" & Transition_Part & "}";
   end Image;

   ----------------------------------------------------------------------------
   function Recursive_Image(Self : Working_Nfa_Class; Index : Natural) return String is
   begin
      if Index = 1 then
         return Image(Self.Working_States(1));
      else
         return Recursive_Image(Self, Index-1) & "/" & Image(Self.Working_States(Index));
      end if;
   end Recursive_Image;


   ----------------------------------------------------------------------------
   function Image(Self : Working_Nfa_Class) return String is
   begin
      if Self.Working_States.Is_Empty then
         return "[null]";
      end if;
      return "[" & Recursive_Image(Self, Natural(Self.Working_States.Length)) & "]";
   end Image;

   ----------------------------------------------------------------------------
   procedure Internal_Set_Nfa
      (Self : in out Working_Nfa_Class;
       Nfa  : in     Nfa_Class'CLASS) is
   begin
      Self.Working_States := Convert_To_Vector_Form(Nfa.Get_States);
   end Internal_Set_Nfa;


   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Working_Nfa_Class) is
   begin
--      Self.Dfa := null;
      null;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Adjust
      (Self : in out Working_Nfa_Class) is
   begin
      null;
   end Adjust;

   ----------------------------------------------------------------------------
   procedure Finalize
      (Self : in out Working_Nfa_Class) is
   begin
      null;
   end Finalize;


end kv.apg.fa.nfa;
