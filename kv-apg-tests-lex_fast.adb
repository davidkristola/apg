with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Tags; use Ada.Tags;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Text_IO; use Ada.Text_IO;

with kv.core.wwstr;

with kv.apg.lex;
with kv.apg.tokens;
with kv.apg.parse;
with kv.apg.directives;
with kv.apg.regex;
with kv.apg.fast;

package body kv.apg.tests.lex_fast is

   use Ada.Characters.Conversions;
   use Ada.Strings.Wide_Wide_Unbounded;
   use kv.core.wwstr;
   use kv.apg.lex;
   use kv.apg.tokens;
   use kv.apg.fast;


   ----------------------------------------------------------------------------
   type Fast_Transition_Test_Class is abstract new kv.core.ut.Test_Class with
      record
         Uut : kv.apg.fast.Transition_Type;
      end record;

   ----------------------------------------------------------------------------
   procedure Check_Transition
      (T : in out Fast_Transition_Test_Class'CLASS;
       D : in     kv.apg.fast.State_Universe_Type;
       C : in     Character;
       E : in     String) is
      use kv.apg.fast;
      Answer : State_Universe_Type;
   begin
      Answer := Move(T.Uut, To_Wide_Wide_Character(C));
      T.Assert(Answer = D, "Wrong destination on '"&C&"' for "&E&" transition, got " & State_Universe_Type'IMAGE(Answer) & ", expected " & State_Universe_Type'IMAGE(D));
   end Check_Transition;


   ----------------------------------------------------------------------------
   type Fast_Uninit_Test is new Fast_Transition_Test_Class with null record;
   procedure Run(T : in out Fast_Uninit_Test) is
      use kv.apg.fast;
   begin
      Check_Transition(T, Invalid_State, '+', "uninitialized");
      T.Assert(Image(T.Uut) = ("any=>0"), "Wrong image! Got <" & (Image(T.Uut)) & ">");
   end Run;

   ----------------------------------------------------------------------------
   type Fast_Any_Test is new Fast_Transition_Test_Class with null record;
   procedure Run(T : in out Fast_Any_Test) is
      use kv.apg.fast;
      Dest : constant State_Id_Type := 2;
      Explanation : constant String := "any";
   begin
      Set_Any(T.Uut, Dest);
      T.Assert(Image(T.Uut) = ("any=>2"), "Wrong image! Got <" & (Image(T.Uut)) & ">");
      Check_Transition(T, Dest, '+', Explanation);
      Check_Transition(T, Dest, 'z', Explanation);
      Check_Transition(T, Dest, 'a', Explanation);
      Check_Transition(T, Dest, 'Z', Explanation);
      Check_Transition(T, Dest, 'A', Explanation);
      Check_Transition(T, Dest, Ada.Characters.Latin_1.NUL, Explanation);
      Check_Transition(T, Dest, Ada.Characters.Latin_1.Reserved_128, Explanation);
      Check_Transition(T, Dest, Ada.Characters.Latin_1.LC_Y_Diaeresis, Explanation);
   end Run;

   ----------------------------------------------------------------------------
   type Fast_Match_Test is new Fast_Transition_Test_Class with null record;
   procedure Run(T : in out Fast_Match_Test) is
      use kv.apg.fast;
      Dest : constant State_Id_Type := 9;
      Explanation : constant String := "match";
   begin
      Set_Match(T.Uut, Dest, To_Wide_Wide_Character(Character'('+')));
      T.Assert(Image(T.Uut) = ("43=>9"), "Wrong image! Got <" & (Image(T.Uut)) & ">");
      Check_Transition(T, Dest, '+', Explanation);
      Check_Transition(T, Invalid_State, 'z', Explanation);
      Check_Transition(T, Invalid_State, 'a', Explanation);
      Check_Transition(T, Invalid_State, 'Z', Explanation);
      Check_Transition(T, Invalid_State, 'A', Explanation);
      Check_Transition(T, Invalid_State, Ada.Characters.Latin_1.NUL, Explanation);
      Check_Transition(T, Invalid_State, Ada.Characters.Latin_1.Reserved_128, Explanation);
      Check_Transition(T, Invalid_State, Ada.Characters.Latin_1.LC_Y_Diaeresis, Explanation);
   end Run;

   ----------------------------------------------------------------------------
   type Fast_Range_Test is new Fast_Transition_Test_Class with null record;
   procedure Run(T : in out Fast_Range_Test) is
      use kv.apg.fast;
      Dest : constant State_Id_Type := 7;
      Explanation : constant String := "range";
      First_C : constant Character := 'b';
      Last_C : constant Character := 'y';
   begin
      Set_Range(T.Uut, Dest, To_Wide_Wide_Character(First_C), To_Wide_Wide_Character(Last_C));
      T.Assert(Image(T.Uut) = ("98-121=>7"), "Wrong image! Got <" & (Image(T.Uut)) & ">");
      Check_Transition(T, Invalid_State, '+', Explanation);
      Check_Transition(T, Invalid_State, 'a', Explanation);
      for C in First_C .. Last_C loop
         Check_Transition(T, Dest, C, Explanation);
      end loop;
      Check_Transition(T, Invalid_State, 'z', Explanation);
      Check_Transition(T, Invalid_State, 'A', Explanation);
      Check_Transition(T, Invalid_State, 'Z', Explanation);
      Check_Transition(T, Invalid_State, Ada.Characters.Latin_1.NUL, Explanation);
      Check_Transition(T, Invalid_State, Ada.Characters.Latin_1.Reserved_128, Explanation);
      Check_Transition(T, Invalid_State, Ada.Characters.Latin_1.LC_Y_Diaeresis, Explanation);
   end Run;


   ----------------------------------------------------------------------------
   type Fast_Transition_Source_Code_Test is new Fast_Transition_Test_Class with null record;
   procedure Run(T : in out Fast_Transition_Source_Code_Test) is
      use kv.apg.fast;
      Dest : constant State_Id_Type := 7;
      First_C : constant Character := 'b';
      Last_C : constant Character := 'y';
   begin
      Set_Range(T.Uut, Dest, To_Wide_Wide_Character(First_C), To_Wide_Wide_Character(Last_C));
      T.Assert(Source_Code(T.Uut) = ("(FROM_TO, 7, WWC'VAL(98), WWC'VAL(121))"), "Wrong Source_Code! Got <" & (Source_Code(T.Uut)) & ">");
      Set_Any(T.Uut, Dest);
      T.Assert(Source_Code(T.Uut) = ("(ANY, 7)"), "Wrong Source_Code! Got <" & (Source_Code(T.Uut)) & ">");
      Set_Match(T.Uut, Dest, To_Wide_Wide_Character(First_C));
      T.Assert(Source_Code(T.Uut) = ("(MATCH, 7, WWC'VAL(98))"), "Wrong Source_Code! Got <" & (Source_Code(T.Uut)) & ">");
      Set_Epsilon(T.Uut, Dest);
      T.Assert(Source_Code(T.Uut) = ("ERROR"), "Wrong Source_Code! Got <" & (Source_Code(T.Uut)) & ">");
   end Run;


   ----------------------------------------------------------------------------
   type Fast_Transition_List_Source_Code_Test is new Fast_Transition_Test_Class with null record;
   procedure Run(T : in out Fast_Transition_List_Source_Code_Test) is
      use kv.apg.fast;
      D1 : constant State_Id_Type := 7;
      D2 : constant State_Id_Type := 11;
      D3 : constant State_Id_Type := 13;
      First_C : constant Character := 'b';
      Last_C : constant Character := 'y';
      Uut : Transition_List_Type(1..3);
      Expected : constant String := "(1 => (FROM_TO, 7, WWC'VAL(98), WWC'VAL(121)), 2 => (ANY, 11), 3 => (MATCH, 13, WWC'VAL(98)))";
   begin
      Set_Range(Uut(1), D1, To_Wide_Wide_Character(First_C), To_Wide_Wide_Character(Last_C));
      Set_Any(Uut(2), D2);
      Set_Match(Uut(3), D3, To_Wide_Wide_Character(First_C));
      T.Assert(Source_Code(Uut) = Expected, "Wrong Source_Code! Got <" & (Source_Code(Uut)) & ">, expected <"&Expected&">");
   end Run;


   ----------------------------------------------------------------------------
   type Fast_State_Test_Class is abstract new kv.core.ut.Test_Class with
      record
         Uut : kv.apg.fast.State_Type;
      end record;

   ----------------------------------------------------------------------------
   type Fast_Init_State_Accepting_Test is new Fast_State_Test_Class with null record;
   procedure Run(T : in out Fast_Init_State_Accepting_Test) is
      use kv.apg.fast;
   begin
      Set_Accepting(T.Uut, 8, 42);
      T.Assert(Image(T.Uut) = ("(8:42){}"), "Wrong image! Got <" & (Image(T.Uut)) & ">");
      T.Assert(Get_Id(T.Uut) = 8, "Should be ID 8 but is not!");
      T.Assert(Is_Accepting(T.Uut), "Should be accepting but is not!");
      T.Assert(Get_Key(T.Uut) = 42, "Key should be 42 but is not!");
      T.Assert(Get_Transition_Count(T.Uut) = 0, "Get_Transition_Count should be 0 but is not!");
   end Run;

   ----------------------------------------------------------------------------
   type Fast_Init_State_Non_Accepting_Test is new Fast_State_Test_Class with null record;
   procedure Run(T : in out Fast_Init_State_Non_Accepting_Test) is
      use kv.apg.fast;
   begin
      Set_Non_Accepting(T.Uut, 7, 2);
      T.Assert(Image(T.Uut) = ("7{any=>0,any=>0}"), "Wrong image! Got <" & (Image(T.Uut)) & ">");
      T.Assert(Get_Id(T.Uut) = 7, "Should be ID 7 but is not!");
      T.Assert(not Is_Accepting(T.Uut), "Should be not accepting but is!");
      T.Assert(Get_Transition_Count(T.Uut) = 2, "Get_Transition_Count should be 2 but is not!");
   end Run;

   ----------------------------------------------------------------------------
   type Fast_Get_Count_Test is new Fast_State_Test_Class with null record;
   procedure Run(T : in out Fast_Get_Count_Test) is
      use kv.apg.fast;
   begin
      Set_Accepting(T.Uut, 6, 32, 3);
      T.Assert(Get_Transition_Count(T.Uut) = 3, "Get_Transition_Count should be 3 but is not!");
   end Run;

   ----------------------------------------------------------------------------
   type Fast_Set_Trans_Test is new Fast_State_Test_Class with null record;
   procedure Run(T : in out Fast_Set_Trans_Test) is
      use kv.apg.fast;
      Input : kv.apg.fast.Transition_Type;
      Output : kv.apg.fast.Transition_Type;
   begin
      Set_Match(Input, 67, To_Wide_Wide_Character(Character'('#')));
      Set_Match(Output, 33, To_Wide_Wide_Character(Character'('?')));
      Set_Non_Accepting(T.Uut, 5, 4);
      T.Assert(Get_Transition_Count(T.Uut) = 4, "Get_Transition_Count should be 4 but is not!");
      Set_Transition(T.Uut, 1, Input);
      Output := Get_Transition(T.Uut, 1);
      T.Assert(Input = Output, "Could not set first transition!");
   end Run;

   ----------------------------------------------------------------------------
   type Fast_Empty_Set_Neg_Test is new Fast_State_Test_Class with null record;
   procedure Run(T : in out Fast_Empty_Set_Neg_Test) is
      use kv.apg.fast;
      Input : kv.apg.fast.Transition_Type;
   begin
      Set_Accepting(T.Uut, 1, 88);
      T.Assert(Get_Transition_Count(T.Uut) = 0, "Get_Transition_Count should be 0 but is not!");
      Set_Match(Input, 2, To_Wide_Wide_Character(Character'('Q')));
      begin
         Set_Transition(T.Uut, 1, Input);
         T.Assert(False, "Set of an invalid transition did not raise an exception!");
      exception
         when others =>
            null;
      end;
   end Run;

   ----------------------------------------------------------------------------
   type Fast_Range_Set_Neg_Test is new Fast_State_Test_Class with null record;
   procedure Run(T : in out Fast_Range_Set_Neg_Test) is
      use kv.apg.fast;
      Input : kv.apg.fast.Transition_Type;
   begin
      Set_Non_Accepting(T.Uut, 5, 4);
      T.Assert(Get_Transition_Count(T.Uut) = 4, "Get_Transition_Count should be 4 but is not!");
      Set_Match(Input, 2, To_Wide_Wide_Character(Character'('Q')));
      begin
         Set_Transition(T.Uut, 5, Input);
         T.Assert(False, "Set of an invalid transition did not raise an exception!");
      exception
         when others =>
            null;
      end;
   end Run;

   ----------------------------------------------------------------------------
   type Fast_Mark_Transitions_Test is new Fast_State_Test_Class with null record;
   procedure Run(T : in out Fast_Mark_Transitions_Test) is
      use kv.apg.fast;
      T1 : kv.apg.fast.Transition_Type;
      T2 : kv.apg.fast.Transition_Type;
      T3 : kv.apg.fast.Transition_Type;
      Next : aliased Active_State_List_Type := (1 => False, 2 => False, 3 => False);
      Count : Natural := 5; -- pre-set with bad value
      A : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('a'));
      B : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('b'));
   begin
      Set_Match(T1, 1, A);
      Set_Match(T2, 2, B);
      Set_Any(T3, 3);
      Set_Non_Accepting(T.Uut, 5, 3);
      Set_Transition(T.Uut, 1, T1);
      Set_Transition(T.Uut, 2, T2);
      Set_Transition(T.Uut, 3, T3);
      -- Make a transition to 1
      Mark_Transitions(T.Uut, A, Next'UNCHECKED_ACCESS, Count);
      T.Assert(Count = 2, "Expected count to be 2, not " & Natural'IMAGE(Count));
      T.Assert(Next(1), "Expected Next(1) to be True but it isn't!");
      T.Assert(not Next(2), "Expected Next(1) to be False but it isn't!");
      T.Assert(Next(3), "Expected Next(3) to be True but it isn't!");
      T.Assert(Image(T.Uut) = ("5{97=>1,98=>2,any=>3}"), "Wrong image! Got <" & (Image(T.Uut)) & ">");
   end Run;

   ----------------------------------------------------------------------------
   type Fast_Append_Trans_Test is new Fast_State_Test_Class with null record;
   procedure Run(T : in out Fast_Append_Trans_Test) is
      use kv.apg.fast;
      T_1 : kv.apg.fast.Transition_Type;
      T_2 : kv.apg.fast.Transition_Type;
   begin
      Set_Match(T_1, 67, To_Wide_Wide_Character(Character'('a')));
      Set_Match(T_2, 33, To_Wide_Wide_Character(Character'('b')));
      Set_Non_Accepting(T.Uut, 5, 1);
      Set_Transition(T.Uut, 1, T_1);
      Append_Transition(T.Uut, T_2);
      T.Assert(Get_Transition_Count(T.Uut) = 2, "Get_Transition_Count should be 2 but is not!");
      T.Assert(Image(T.Uut) = ("5{97=>67,98=>33}"), "Wrong image! Got <" & (Image(T.Uut)) & ">");
   end Run;


   ----------------------------------------------------------------------------
   type Fast_Epsilon_Test is new Fast_State_Test_Class with null record;
   procedure Run(T : in out Fast_Epsilon_Test) is
      T1 : kv.apg.fast.Transition_Type;
      T2 : kv.apg.fast.Transition_Type;
      T3 : kv.apg.fast.Transition_Type;
      Next : aliased Active_State_List_Type := (1 => False, 2 => False, 3 => False);
      A : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('a'));
      B : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('b'));
   begin
      Set_Match(T1, 1, A);
      Set_Match(T2, 2, B);
      Set_Epsilon(T3, 3);
      Set_Non_Accepting(T.Uut, 5, 3);
      Set_Transition(T.Uut, 1, T1);
      Set_Transition(T.Uut, 2, T2);
      Set_Transition(T.Uut, 3, T3);
      -- Make a transition to 3
      Mark_Epsilon_Transitions(T.Uut, Next'UNCHECKED_ACCESS);
      T.Assert(not Next(1), "Expected Next(1) to be False but it isn't!");
      T.Assert(not Next(2), "Expected Next(1) to be False but it isn't!");
      T.Assert(Next(3), "Expected Next(3) to be True but it isn't!");
      T.Assert(Image(T.Uut) = ("5{97=>1,98=>2,ɛ=>3}"), "Wrong image! Got <" & (Image(T.Uut)) & ">");
   end Run;

   ----------------------------------------------------------------------------
   type Fast_Renumber_T_Test is new Fast_State_Test_Class with null record;
   procedure Run(T : in out Fast_Renumber_T_Test) is
      T_1 : kv.apg.fast.Transition_Type;
      T_2 : kv.apg.fast.Transition_Type;
   begin
      Set_Match(T_1, 67, To_Wide_Wide_Character(Character'('a')));
      Renumber(T_1, -10);
      Renumber(T_2, 10);
      T.Assert(Get_Dest(T_1) = 57, "Get_Dest(T_1) = 57 should be but is not!");
      T.Assert(Get_Dest(T_2) = Invalid_State, "Get_Dest(T_2) = Invalid_State should be but is not!");
   end Run;

   ----------------------------------------------------------------------------
   type Fast_Renumber_S_Test is new Fast_State_Test_Class with null record;
   procedure Run(T : in out Fast_Renumber_S_Test) is
      use kv.apg.fast;
      T_1 : kv.apg.fast.Transition_Type;
      T_2 : kv.apg.fast.Transition_Type;
   begin
      Set_Match(T_1, 67, To_Wide_Wide_Character(Character'('a')));
      Set_Match(T_2, 33, To_Wide_Wide_Character(Character'('b')));
      Set_Non_Accepting(T.Uut, 5, 1);
      Set_Transition(T.Uut, 1, T_1);
      Append_Transition(T.Uut, T_2);
      Renumber(T.Uut, 3);
      T.Assert(Get_Dest(Get_Transition(T.Uut, 1)) = 70, "Get_Dest(T_1) = 70 should be but is not!");
      T.Assert(Get_Dest(Get_Transition(T.Uut, 2)) = 36, "Get_Dest(T_2) = 36 should be but is not!");
      T.Assert(Get_Id(T.Uut) = 8, "Get_Id(T.Uut) = 8 should be but is not!");
   end Run;

   ----------------------------------------------------------------------------
   type Fast_Deep_Copy_Test is new Fast_State_Test_Class with null record;
   procedure Run(T : in out Fast_Deep_Copy_Test) is
      use kv.apg.fast;
      T_1 : kv.apg.fast.Transition_Type;
      T_2 : kv.apg.fast.Transition_Type;
      Copy : State_Type;
   begin
      Set_Match(T_1, 67, To_Wide_Wide_Character(Character'('a')));
      Set_Match(T_2, 33, To_Wide_Wide_Character(Character'('b')));
      Set_Non_Accepting(T.Uut, 5, 1);
      Set_Transition(T.Uut, 1, T_1);
      Append_Transition(T.Uut, T_2);
      Init_By_Deep_Copy(Copy, T.Uut);
      T.Assert(Get_Transition_Count(Copy) = 2, "Get_Transition_Count should be 2 but is not!");
      T.Assert(Image(Copy) = ("5{97=>67,98=>33}"), "Wrong image! Got <" & (Image(Copy)) & ">");
   end Run;

   ----------------------------------------------------------------------------
   type Fast_State_Source_Code_Test is new Fast_State_Test_Class with null record;
   procedure Run(T : in out Fast_State_Source_Code_Test) is
      use kv.apg.fast;
      Expected : constant String := "(Invalid, T5'ACCESS)";
   begin
      Set_Non_Accepting(T.Uut, 5, 0);
      declare
         Answer : constant String := Source_Code(T.Uut, "Invalid");
      begin
         T.Assert(Answer = Expected, "Wrong Source_Code! Got <" & (Answer) & ">, expected " & Expected);
      end;
   end Run;


   ----------------------------------------------------------------------------
   procedure register(suite : in kv.core.ut.Suite_Pointer_Type) is
   begin
      suite.register(new Fast_Uninit_Test, "Fast_Uninit_Test");
      suite.register(new Fast_Any_Test, "Fast_Any_Test");
      suite.register(new Fast_Match_Test, "Fast_Match_Test");
      suite.register(new Fast_Range_Test, "Fast_Range_Test");
      suite.register(new Fast_Transition_Source_Code_Test, "Fast_Transition_Source_Code_Test");
      suite.register(new Fast_Transition_List_Source_Code_Test, "Fast_Transition_List_Source_Code_Test");

      suite.register(new Fast_Init_State_Accepting_Test, "Fast_Init_State_Accepting_Test");
      suite.register(new Fast_Init_State_Non_Accepting_Test, "Fast_Init_State_Non_Accepting_Test");
      suite.register(new Fast_Get_Count_Test, "Fast_Get_Count_Test");
      suite.register(new Fast_Set_Trans_Test, "Fast_Set_Trans_Test");
      suite.register(new Fast_Empty_Set_Neg_Test, "Fast_Empty_Set_Neg_Test");
      suite.register(new Fast_Range_Set_Neg_Test, "Fast_Range_Set_Neg_Test");
      suite.register(new Fast_Mark_Transitions_Test, "Fast_Mark_Transitions_Test");
      suite.register(new Fast_Append_Trans_Test, "Fast_Append_Trans_Test");
      suite.register(new Fast_Epsilon_Test, "Fast_Epsilon_Test");
      suite.register(new Fast_Renumber_T_Test, "Fast_Renumber_T_Test");
      suite.register(new Fast_Renumber_S_Test, "Fast_Renumber_S_Test");
      suite.register(new Fast_Deep_Copy_Test, "Fast_Deep_Copy_Test");
      suite.register(new Fast_State_Source_Code_Test, "Fast_State_Source_Code_Test");
--      suite.register(new XXX, "XXX");
   end register;

end kv.apg.tests.lex_fast;
