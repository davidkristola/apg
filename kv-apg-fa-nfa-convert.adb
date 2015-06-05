with Ada.Text_IO; use Ada.Text_IO;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Sets;

package body kv.apg.fa.nfa.convert is

   use kv.apg.fast;

   package State_List is new Ada.Containers.Doubly_Linked_Lists(State_Id_Type);
   -- State_List.List;
   -- Append
   -- First_Element
   -- Delete_First

   package State_Set is new Ada.Containers.Ordered_Sets(State_Id_Type);
   -- State_Set.Set
   -- Include
   -- Contains



   Debug : Boolean := False;

   -------------------------------------------------------------------------
   procedure Set_Debug(Value : in Boolean) is
   begin
      Debug := Value;
   end Set_Debug;


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
      if Debug then Put_Line("Self.Image = " & Self.Image); end if;
      Self.Internal_Banish_Epsilon_Transitions;
      if Debug then Put_Line("Self.Image = " & Self.Image); end if;
      Self.Internal_Remove_Duplicates;
      if Debug then Put_Line("Self.Image = " & Self.Image); end if;
      Self.Internal_Remove_Unreachables;
      if Debug then Put_Line("Self.Image = " & Self.Image); end if;
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
            if Debug then Put_Line("Find_Next_Mixed_Epsilon found " & Img(S.State.Id)); end if;
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
      if Debug then Put_Line("Internal_Bump_Non_Epsilon_At " & Positive'IMAGE(Index)); end if;
      -- Initialize State
      Set_Non_Accepting(New_State.State, State_Id_Type(Self.Working_States.Length) + 1, 0);
      if Debug then Put_Line("New_State.State = " & Image(New_State.State)); end if;
      -- Find non-epsilon transition
      while Self.Working_States(Index).Trans(Non_Epsilon_Index).Criteria = Epsilon loop
         Non_Epsilon_Index := Non_Epsilon_Index + 1;
      end loop;
      if Debug then Put_Line("The non-epsilon transition is <"&Image(Self.Working_States(Index).Trans(Non_Epsilon_Index))&"> at " & Positive'IMAGE(Non_Epsilon_Index)); end if;
      -- Copy transition
      New_State.Trans.Append(Self.Working_States(Index).Trans(Non_Epsilon_Index));
      -- Add new state to the list
      if Debug then Put_Line("Appending New_State " & Image(New_State)); end if;
      Self.Working_States.Append(New_State);
      -- Replace non-epsilon with an epsilon to the new state
      Set_Epsilon(New_Transition, State_Id_Type(Self.Working_States.Length));
      Self.Working_States(Index).Trans.Replace_Element(Non_Epsilon_Index, New_Transition);
   end Internal_Bump_Non_Epsilon_At;

   ----------------------------------------------------------------------------
   -- I could use this as a cursor object
   type State_And_Transition_Tuple is
      record
         State : Natural;
         Transition : Natural;
         Next : Natural;
      end record;

   ----------------------------------------------------------------------------
   function Is_Epsilon_Out(Check : Working_State_Type) return Boolean is
      use Ada.Containers;
   begin
      return (Check.Trans.Length > 0) and then (Check.Trans.Element(1).Criteria = Epsilon);
   end Is_Epsilon_Out;

   ----------------------------------------------------------------------------
   function Find_Next_Chained_Epsilon
      (Working_States : State_Vector.Vector) return State_And_Transition_Tuple is
      Next_Index : Positive;
      Ti : Natural;
   begin
      for S of Working_States loop
         Ti := 0;
         for T of S.Trans loop
            Ti := Ti + 1;
            if T.Criteria = Epsilon then
               --Put_Line("Checking "&Image(T)&"...");
               Next_Index := Positive(T.To_State);
               if Is_Epsilon_Out(Working_States.Element(Next_Index)) then
                  --Put_Line("Found ɛ=>ɛ transition from state "&State_Universe_Type'IMAGE(S.State.Id)&" to "&Positive'IMAGE(Next_Index));
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
      loop
         Where := Find_Next_Chained_Epsilon(Self.Working_States);
         exit when Where.State = 0;
         Internal_Collapse_Chain_At(Self.Working_States, Where);
      end loop;
   end Internal_Unchain_Epsilon_Transitions;

   ----------------------------------------------------------------------------
   function Find_Next_Epsilon
      (Working_States : State_Vector.Vector) return State_And_Transition_Tuple is
      Next_Index : Positive;
      Ti : Natural;
   begin
      for S of Working_States loop
         Ti := 0;
         for T of S.Trans loop
            Ti := Ti + 1;
            if T.Criteria = Epsilon then
               Next_Index := Positive(T.To_State);
               --Put_Line("Found ɛ transition from state "&State_Universe_Type'IMAGE(S.State.Id)&" to "&Positive'IMAGE(Next_Index));
               return State_And_Transition_Tuple'(State => Natural(S.State.Id), Transition => Ti, Next => Next_Index);
            end if;
         end loop;
      end loop;
      return State_And_Transition_Tuple'(State => 0, Transition => 0, Next => 0);
   end Find_Next_Epsilon;

   ----------------------------------------------------------------------------
   procedure Internal_Replace_Epsilon_At
      (Working_States : in out State_Vector.Vector;
       Where          : in     State_And_Transition_Tuple) is
   begin
      for T of Working_States.Reference(Positive(Where.Next)).Trans loop
         Working_States.Reference(Positive(Where.State)).Trans.Append(T);
      end loop;
      Working_States.Reference(Positive(Where.State)).Trans.Delete(Where.Transition);
      if Working_States.Reference(Positive(Where.Next)).State.Accepting then
         if Debug then Put_Line("copy accepting key ="&Key_Type'IMAGE(Working_States.Reference(Positive(Where.Next)).State.Accepted_Key)); end if;
         Working_States.Reference(Positive(Where.State)).State.Accepted_Key := Working_States.Reference(Positive(Where.Next)).State.Accepted_Key;
         Working_States.Reference(Positive(Where.State)).State.Accepting := True;
      end if;
   end Internal_Replace_Epsilon_At;

   ----------------------------------------------------------------------------
   procedure Internal_Collapse_Epsilon_Transitions
      (Self : in out To_Cnfa_Class) is
      Where : State_And_Transition_Tuple;
   begin
      if Debug then Put_Line("--------------Internal_Collapse_Epsilon_Transitions----------------"); end if;
      pragma Assert(Find_Next_Mixed_Epsilon(Self.Working_States) = Invalid_State);
      pragma Assert(Find_Next_Chained_Epsilon(Self.Working_States).State = 0);
      loop
         Where := Find_Next_Epsilon(Self.Working_States);
         exit when Where.State = 0;
         Internal_Replace_Epsilon_At(Self.Working_States, Where);
      end loop;
   end Internal_Collapse_Epsilon_Transitions;

   ----------------------------------------------------------------------------
   procedure Internal_Banish_Epsilon_Transitions
      (Self : in out To_Cnfa_Class) is
      Where : State_And_Transition_Tuple;
   begin
      if Debug then Put_Line("-------------Internal_Banish_Epsilon_Transitions-----------------"); end if;
      loop
         Where := Find_Next_Epsilon(Self.Working_States);
         exit when Where.State = 0;
         Internal_Replace_Epsilon_At(Self.Working_States, Where);
      end loop;
   end Internal_Banish_Epsilon_Transitions;

   ----------------------------------------------------------------------------
   procedure Internal_Retarget_Transitions
      (Self       : in out To_Cnfa_Class;
       Old_Target : in     State_Id_Type;
       New_Target : in     State_Id_Type) is
   begin
      if Debug then Put_Line("Retarget from "&Img(Old_Target)&" to "&Img(New_Target)); end if;

      for S of Self.Working_States loop
         for T of S.Trans loop
            if T.To_State = Old_Target then
               T.To_State := New_Target;
            end if;
         end loop;
      end loop;
   end Internal_Retarget_Transitions;

   ----------------------------------------------------------------------------
   procedure Internal_Delete
      (Self   : in out To_Cnfa_Class;
       Remove : in     State_Id_Type) is

   begin
      if Debug then Put_Line("Deleting state "&Img(Remove)); end if;
      for Index in 1 .. Positive(Self.Working_States.Length) loop
         if Index > Positive(Remove) then
            Self.Internal_Retarget_Transitions(State_Id_Type(Index), State_Id_Type(Index - 1));
            Self.Working_States.Reference(Index).State.Id := Self.Working_States.Reference(Index).State.Id - 1;
         end if;
      end loop;
      Self.Working_States.Delete(Positive(Remove));
   end Internal_Delete;

   ----------------------------------------------------------------------------
   function Are_Effectively_The_Same(A : Working_State_Type; B : Working_State_Type) return Boolean is
      use Ada.Containers;
   begin
      if A.State.Accepting /= B.State.Accepting then
         return False;
      end if;
      if A.State.Accepting and then (A.State.Accepted_Key /= B.State.Accepted_Key) then
         return False;
      end if;
      if A.Trans.Length /= B.Trans.Length then
         return False;
      end if;
      for T of A.Trans loop
         if not B.Trans.Contains(T) then
            return False;
         end if;
      end loop;
      -- All the tests pass
      return True;
   end Are_Effectively_The_Same;

   ----------------------------------------------------------------------------
   function Internal_Duplicate_Of
      (Self  : To_Cnfa_Class;
       Index : Positive) return Natural is
   begin
      for Current in 1 .. Positive(Self.Working_States.Length) loop
         if Current /= Index then
            if Are_Effectively_The_Same(Self.Working_States.Constant_Reference(Current), Self.Working_States.Constant_Reference(Index)) then
               return Current;
            end if;
         end if;
      end loop;
      return 0; -- No duplicate
   end Internal_Duplicate_Of;

   ----------------------------------------------------------------------------
   type Duplicate_Tuple_Type is
      record
         First : Natural;
         Other : Natural;
      end record;

   ----------------------------------------------------------------------------
   function Find_Duplicates(Self : To_Cnfa_Class) return Duplicate_Tuple_Type is
      Other : Natural;
   begin
      for First in 1 .. Positive(Self.Working_States.Length) loop
         Other := Self.Internal_Duplicate_Of(First);
         if Other /= 0 then
            return Duplicate_Tuple_Type'(First => First, Other => Other);
         end if;
      end loop;
      return Duplicate_Tuple_Type'(First => 0, Other => 0);
   end Find_Duplicates;

   ----------------------------------------------------------------------------
   procedure Internal_Remove_Duplicates
      (Self : in out To_Cnfa_Class) is
      Where : Duplicate_Tuple_Type;
   begin
      Where := Find_Duplicates(Self);
      while Where.First /= 0 loop
         if Debug then Put_Line("Duplicate pair: "&Natural'IMAGE(Where.First) & Natural'IMAGE(Where.Other)); end if;
         Self.Internal_Retarget_Transitions(State_Id_Type(Where.Other), State_Id_Type(Where.First));
         Self.Internal_Delete(State_Id_Type(Where.Other));
         Where := Find_Duplicates(Self);
      end loop;
   end Internal_Remove_Duplicates;

   ----------------------------------------------------------------------------
   procedure Internal_Remove_Unreachables
      (Self : in out To_Cnfa_Class) is

      Unchecked : State_List.List;
      Reachable : State_Set.Set;
      Working : State_Id_Type;
      Target : State_Id_Type;

   begin
      if Debug then Put_Line("==========Internal_Remove_Unreachables==========="); end if;
      Unchecked.Append(1);
      while not Unchecked.Is_Empty loop
         Working := Unchecked.First_Element;
         Unchecked.Delete_First;
         Reachable.Include(Working);
         -- now add everything that we can reach that isin't reachable
         for T of Self.Working_States.Constant_Reference(Positive(Working)).Trans loop
            Target := T.To_State;
            if not Reachable.Contains(Target) then
               Unchecked.Append(Target);
            end if;
         end loop;
      end loop;
      -- Must delete in reverse order
      for Index in reverse 1 .. State_Id_Type(Self.Working_States.Length) loop
         if not Reachable.Contains(Index) then
            Self.Internal_Delete(Index);
         end if;
      end loop;
   end Internal_Remove_Unreachables;

   ----------------------------------------------------------------------------
   function Internal_Get_Cnfa
      (Self : To_Cnfa_Class) return kv.apg.fa.nfa.Nfa_Class is

      Answer : kv.apg.fa.nfa.Nfa_Class;

   begin
      Answer.Initialize(Convert_To_Array_Form(Self.Working_States));
      return Answer;
   end Internal_Get_Cnfa;

end kv.apg.fa.nfa.convert;
