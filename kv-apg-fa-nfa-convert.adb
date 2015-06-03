with Ada.Text_IO; use Ada.Text_IO;

package body kv.apg.fa.nfa.convert is

   use kv.apg.fast;



   ----------------------------------------------------------------------------
   procedure Nfa_To_Dfa
      (Self : in out To_Dfa_Class;
       Nfa  : in     Nfa_Class;
       Dfa  :    out kv.apg.fa.dfa.Dfa_Class) is
   begin
      Self.Internal_Set_Nfa(Nfa);
      Self.Internal_Subset_Construction;
      Dfa := Self.Internal_Get_Dfa;
   end Nfa_To_Dfa;

   ----------------------------------------------------------------------------
   procedure Internal_Subset_Construction
      (Self : in out To_Dfa_Class) is
   begin
      null; --TODO
   end Internal_Subset_Construction;

   ----------------------------------------------------------------------------
   function Internal_Get_Dfa
      (Self : To_Dfa_Class) return kv.apg.fa.dfa.Dfa_Class is

      Answer : kv.apg.fa.dfa.Dfa_Class;

   begin
      Answer.Initialize(Convert_To_Array_Form(Self.Working_States));
      return Answer;
   end Internal_Get_Dfa;







   ----------------------------------------------------------------------------
   procedure Nfa_To_Cnfa
      (Self : in out To_Cnfa_Class;
       Nfa  : in     Nfa_Class;
       Cnfa :    out Nfa_Class) is
   begin
      Self.Internal_Set_Nfa(Nfa);
      --TODO: get rid of epsilon transitions
      Self.Internal_Unmix_Epsilon_Transitions;
      Cnfa := Self.Internal_Get_Cnfa;
   end Nfa_To_Cnfa;

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
   procedure Internal_Unmix_Epsilon_Transitions
      (Self : in out To_Cnfa_Class) is
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
      (Self  : in out To_Cnfa_Class;
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
   type State_And_Transition_Tuple is
      record
         State : Natural;
         Transition : Natural;
         Next : Natural;
      end record;

   ----------------------------------------------------------------------------
   function Find_Next_Chained_Epsilon
      (Working_States : State_Vector.Vector) return State_And_Transition_Tuple is
      Next_Index : Positive;
      Ti : Natural;
      use Ada.Containers;
   begin
      for S of Working_States loop
         Ti := 0;
         for T of S.Trans loop
            Ti := Ti + 1;
            if T.Criteria = Epsilon then
               Put_Line("Checking "&Image(T)&"...");
               Next_Index := Positive(T.To_State);
               if (Working_States.Element(Next_Index).Trans.Length > 0) and then (Working_States.Element(Next_Index).Trans.Element(1).Criteria = Epsilon) then
                  Put_Line("Found ɛ=>ɛ transition from state "&State_Universe_Type'IMAGE(S.State.Id)&" to "&Positive'IMAGE(Next_Index));
                  return State_And_Transition_Tuple'(State => Natural(S.State.Id), Transition => Ti, Next => Next_Index);
               end if;
            end if;
         end loop;
      end loop;
      return State_And_Transition_Tuple'(State => 0, Transition => 0, Next => 0);
   end Find_Next_Chained_Epsilon;

   ----------------------------------------------------------------------------
   procedure Internal_Collapse_Chain_At
      (Working_States : in out State_Vector.Vector;
       Where          : in     State_And_Transition_Tuple) is
   begin
      for T of Working_States.Reference(Positive(Where.Next)).Trans loop
         Working_States.Reference(Positive(Where.State)).Trans.Append(T);
      end loop;
      Working_States.Reference(Positive(Where.State)).Trans.Delete(Where.Transition);
   end Internal_Collapse_Chain_At;

   ----------------------------------------------------------------------------
   procedure Internal_Unchain_Epsilon_Transitions
      (Self : in out To_Cnfa_Class) is
      Where : State_And_Transition_Tuple;
   begin
      pragma Assert(Find_Next_Mixed_Epsilon(Self.Working_States) = Invalid_State);
      null; -- TODO
      loop
         Where := Find_Next_Chained_Epsilon(Self.Working_States);
         exit when Where.State = 0;
         Internal_Collapse_Chain_At(Self.Working_States, Where);
      end loop;
   end Internal_Unchain_Epsilon_Transitions;

   ----------------------------------------------------------------------------
   function Internal_Get_Cnfa
      (Self : To_Cnfa_Class) return kv.apg.fa.nfa.Nfa_Class is

      Answer : kv.apg.fa.nfa.Nfa_Class;

   begin
      Answer.Initialize(Convert_To_Array_Form(Self.Working_States));
      return Answer;
   end Internal_Get_Cnfa;

end kv.apg.fa.nfa.convert;
