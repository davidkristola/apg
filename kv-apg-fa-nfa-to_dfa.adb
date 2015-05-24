with Ada.Text_IO; use Ada.Text_IO;

package body kv.apg.fa.nfa.to_dfa is

   use kv.apg.fast;

   ----------------------------------------------------------------------------
   function Equals(L, R : Working_State_Type) return Boolean is
   begin
      -- TODO: Trust that the IDs are unique?
      return L.State.Id = R.State.Id;
   end Equals;

   ----------------------------------------------------------------------------
   procedure Append_State
      (List  : in out State_Vector.Vector;
       State : in     State_Type) is

      Working_State : Working_State_Type;

   begin
      Put_Line("Processing in state " & Image(State));
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
   function Find_Next_Mixed_Epsilon
      (Working_States : State_Vector.Vector) return State_Universe_Type is
      Epsilon_Count : Natural;
      Normal_Count : Natural;
   begin
      for S of Working_States loop
         Epsilon_Count := 0;
         Normal_Count := 0;
         for T of S.Trans loop
            if T.Criteria = Epsilon then
               Epsilon_Count := Epsilon_Count + 1;
            else
               Normal_Count := Normal_Count + 1;
            end if;
         end loop;
         if (Epsilon_Count > 0) and (Normal_Count > 0) then
            Put_Line("Find_Next_Mixed_Epsilon found " & Img(S.State.Id));
            return S.State.Id;
         end if;
      end loop;
      return Invalid_State;
   end Find_Next_Mixed_Epsilon;



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
   function Recursive_Image(Self : Converter_Class; Index : Natural) return String is
   begin
      if Index = 1 then
         return Image(Self.Working_States(1));
      else
         return Recursive_Image(Self, Index-1) & "/" & Image(Self.Working_States(Index));
      end if;
   end Recursive_Image;


   ----------------------------------------------------------------------------
   function Image(Self : Converter_Class) return String is
   begin
      if Self.Working_States.Is_Empty then
         return "[null]";
      end if;
      return "[" & Recursive_Image(Self, Natural(Self.Working_States.Length)) & "]";
   end Image;

   ----------------------------------------------------------------------------
   procedure Nfa_To_Dfa
      (Self : in out Converter_Class;
       Nfa  : in     Nfa_Class;
       Dfa  :    out kv.apg.fa.dfa.Dfa_Class) is
   begin
      Self.Internal_Set_Nfa(Nfa);

      --TODO: get rid of epsilon and nondeterministic transitions
      Self.Internal_Unmix_Epsilon_Transitions;

      Dfa := Self.Internal_Get_Dfa;
   end Nfa_To_Dfa;

   ----------------------------------------------------------------------------
   procedure Internal_Set_Nfa
      (Self : in out Converter_Class;
       Nfa  : in     Nfa_Class) is
   begin
      Self.Working_States := Convert_To_Vector_Form(Nfa.Get_States);
   end Internal_Set_Nfa;

   ----------------------------------------------------------------------------
   procedure Internal_Unmix_Epsilon_Transitions
      (Self : in out Converter_Class) is
      Index : State_Universe_Type;
   begin
      loop
         Index := Find_Next_Mixed_Epsilon(Self.Working_States);
         exit when Index = Invalid_State;
         Self.Internal_Bump_Non_Epsilon_At(Positive(Index));
      end loop;
   end Internal_Unmix_Epsilon_Transitions;

   ----------------------------------------------------------------------------
   procedure Internal_Bump_Non_Epsilon_At
      (Self  : in out Converter_Class;
       Index : in     Positive) is

      New_State : Working_State_Type;
      Non_Epsilon_Index : Positive := 1;
      New_Transition : Transition_Type;

   begin
      Put_Line("Internal_Bump_Non_Epsilon_At " & Positive'IMAGE(Index));
      -- Initialize State
      Set_Non_Accepting(New_State.State, State_Id_Type(Self.Working_States.Length) + 1, 0);
      Put_Line("New_State.State = " & Image(New_State.State));
      -- Find non-epsilon transition
      while Self.Working_States(Index).Trans(Non_Epsilon_Index).Criteria = Epsilon loop
         Non_Epsilon_Index := Non_Epsilon_Index + 1;
      end loop;
      Put_Line("The non-epsilon transition is <"&Image(Self.Working_States(Index).Trans(Non_Epsilon_Index))&"> at " & Positive'IMAGE(Non_Epsilon_Index));
      -- Copy transition
      New_State.Trans.Append(Self.Working_States(Index).Trans(Non_Epsilon_Index));
      -- Add new state to the list
      Put_Line("Appending New_State " & Image(New_State));
      Self.Working_States.Append(New_State);
      -- Replace non-epsilon with an epsilon to the new state
      Set_Epsilon(New_Transition, State_Id_Type(Self.Working_States.Length));
      Self.Working_States(Index).Trans.Replace_Element(Non_Epsilon_Index, New_Transition);
   end Internal_Bump_Non_Epsilon_At;


   ----------------------------------------------------------------------------
   function Internal_Get_Dfa
      (Self : Converter_Class) return kv.apg.fa.dfa.Dfa_Class is

      Answer : kv.apg.fa.dfa.Dfa_Class;

   begin
      Answer.Initialize(Convert_To_Array_Form(Self.Working_States));
      return Answer;
   end Internal_Get_Dfa;

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Converter_Class) is
   begin
--      Self.Dfa := null;
      null;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Adjust
      (Self : in out Converter_Class) is
   begin
      null;
   end Adjust;

   ----------------------------------------------------------------------------
   procedure Finalize
      (Self : in out Converter_Class) is
   begin
      null;
   end Finalize;

end kv.apg.fa.nfa.to_dfa;
