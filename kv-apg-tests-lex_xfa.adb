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
with kv.apg.fa.nfa;
with kv.apg.fa.dfa;
with kv.apg.fa.nfa.convert;

package body kv.apg.tests.lex_xfa is

   use Ada.Characters.Conversions;
   use Ada.Strings.Wide_Wide_Unbounded;
   use kv.core.wwstr;
   use kv.apg.lex;
   use kv.apg.tokens;
   use kv.apg.fast;

   ----------------------------------------------------------------------------
   type Nfa_Test_Class is abstract new kv.core.ut.Test_Class with
      record
         Uut : aliased kv.apg.fa.nfa.Nfa_Class;
      end record;

   ----------------------------------------------------------------------------
   type Init_Nfa_Test is new Nfa_Test_Class with null record;
   procedure Run(T : in out Init_Nfa_Test) is
   begin
      T.Uut.Initialize(1);
      T.Assert(T.Uut.Get_State_Count = 1, "Wrong init state count (should be 1, is " & Natural'IMAGE(T.Uut.Get_State_Count) & ").");
   end Run;

   ----------------------------------------------------------------------------
   type Image_Nfa_Test is new Nfa_Test_Class with null record;
   procedure Run(T : in out Image_Nfa_Test) is
      use kv.apg.fast;
      T1 : kv.apg.fast.Transition_Type;
      T2 : kv.apg.fast.Transition_Type;
      A : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('a'));
      B : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('b'));
   begin
      Set_Match(T1, 2, A);
      Set_Match(T2, 2, B);
      T.Uut.Initialize(2);
      T.Assert(T.Uut.Get_State_Count = 2, "Wrong init state count (should be 2, is " & Natural'IMAGE(T.Uut.Get_State_Count) & ").");
      T.Uut.Set_State_Non_Accepting(1, 2);
      T.Uut.Set_State_Transition(1, 1, T1);
      T.Uut.Set_State_Transition(1, 2, T2);
      T.Uut.Set_State_Accepting(2, 36);
      T.Assert(T.Uut.Image = ("[1{97=>2,98=>2}/(2:36){}]"), "Wrong image! Got <" & T.Uut.Image & ">");
   end Run;

   ----------------------------------------------------------------------------
   type Combine_Nfa_Test is new Nfa_Test_Class with null record;
   procedure Run(T : in out Combine_Nfa_Test) is
      use kv.apg.fa.nfa;
      Nfas : Nfa_Array_Type(1..3);
      T1 : kv.apg.fast.Transition_Type;
      T2 : kv.apg.fast.Transition_Type;
      T3 : kv.apg.fast.Transition_Type;
      A : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('a'));
      B : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('b'));
      C : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('c'));
      D : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('d'));
      Expected_Image : constant String := "[1{ɛ=>2,ɛ=>4,ɛ=>6}/2{97=>3}/(3:1){}/4{98=>5}/(5:2){}/6{99=>7}/(7:3){}]";
      Recognizer : kv.apg.fa.nfa.Nfa_State_Class;
   begin
      Nfas(1).Initialize(Alloc => 2, Key => 1);
      Nfas(2).Initialize(Alloc => 2, Key => 2);
      Nfas(3).Initialize(Alloc => 2, Key => 3);

      Set_Match(T1, 2, A);
      Set_Match(T2, 2, B);
      Set_Match(T3, 2, C);

      Nfas(1).Set_State_Non_Accepting(State => 1, Alloc => 1);
      Nfas(1).Set_State_Transition(State => 1, Index => 1, Trans => T1);
      Nfas(1).Set_State_Accepting(State => 2, Key => 1);

      Nfas(2).Set_State_Non_Accepting(State => 1, Alloc => 1);
      Nfas(2).Set_State_Transition(State => 1, Index => 1, Trans => T2);
      Nfas(2).Set_State_Accepting(State => 2, Key => 2);

      Nfas(3).Set_State_Non_Accepting(State => 1, Alloc => 1);
      Nfas(3).Set_State_Transition(State => 1, Index => 1, Trans => T3);
      Nfas(3).Set_State_Accepting(State => 2, Key => 3);

      --kv.apg.fa.nfa.Set_Debug(True);
      T.Uut.Initialize(Nfas);
      T.Assert(T.Uut.Get_State_Count = 7, "Wrong init state count (should be 7, is " & Natural'IMAGE(T.Uut.Get_State_Count) & ").");
      T.Assert(T.Uut.Image = Expected_Image, "Wrong image! Expected <"&Expected_Image&">, got <" & T.Uut.Image & ">");
      kv.apg.fa.nfa.Set_Debug(False);

      Recognizer.Initialize(T.Uut'UNCHECKED_ACCESS);
      Recognizer.Ingest(A);
      T.Assert(Recognizer.Is_Accepting, "Should be accepting after 'a'");
      T.Assert(Recognizer.Is_Terminal, "Should be terminal after 'a'");
      T.Assert(Recognizer.Get_Key = 1, "Should be key = 1 after 'a'");

      Recognizer.Reset;
      Recognizer.Ingest(B);
      T.Assert(Recognizer.Is_Accepting, "Should be accepting after 'b'");
      T.Assert(Recognizer.Is_Terminal, "Should be terminal after 'b'");
      T.Assert(Recognizer.Get_Key = 2, "Should be key = 2 after 'b'");

      Recognizer.Reset;
      Recognizer.Ingest(C);
      T.Assert(Recognizer.Is_Accepting, "Should be accepting after 'c'");
      T.Assert(Recognizer.Is_Terminal, "Should be terminal after 'c'");
      T.Assert(Recognizer.Get_Key = 3, "Should be key = 3 after 'c'");

      Recognizer.Reset;
      Recognizer.Ingest(D);
      T.Assert(not Recognizer.Is_Accepting, "Should NOT be accepting after 'd'");
      T.Assert(Recognizer.Is_Failed, "Should be Is_Failed after 'd'");
   exception
      when others =>
         kv.apg.fa.nfa.Set_Debug(False);
         T.Assert(False, "exception");
   end Run;



   ----------------------------------------------------------------------------
   type Base_Nfa_State_Test_Class is abstract new kv.core.ut.Test_Class with
      record
         Nfa : aliased kv.apg.fa.nfa.Nfa_Class;
         Uut : aliased kv.apg.fa.nfa.Nfa_State_Class;
      end record;

   ----------------------------------------------------------------------------
   procedure Ingest_All(T : in out Base_Nfa_State_Test_Class'CLASS; S : in String) is
      WS : constant Wide_Wide_String := To_Wide_Wide_String(S);
   begin
      for WC of WS loop
         T.Uut.Ingest(WC);
      end loop;
   end Ingest_All;

   ----------------------------------------------------------------------------
   type Nfa_State_Test_Class is abstract new Base_Nfa_State_Test_Class with null record;

   ----------------------------------------------------------------------------
   overriding procedure Set_Up(T : in out Nfa_State_Test_Class) is
      use kv.apg.fast;
      --
      -- +----(c)-+                  +------+
      -- |        ^                  |      |
      -- v        |                  v      |
      -- 1 -(a)-> 2 -(b)-> 3!  plus  2 -(a)-+
      -- |        ^
      -- |        |
      -- +--(c)---+
      --
      -- This will accept "ab", "aab", "a*b", "cb", "cccb", "ccab", "cca*b", "(cc)*b", "(a|c)(a|c(a|c))*b"
      --
      T1_1 : kv.apg.fast.Transition_Type;
      T1_2 : kv.apg.fast.Transition_Type;
      T2_1 : kv.apg.fast.Transition_Type;
      T2_2 : kv.apg.fast.Transition_Type;
      T2_3 : kv.apg.fast.Transition_Type;
      A : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('a'));
      B : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('b'));
      C : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('c'));
   begin
      Set_Match(T1_1, 2, A);
      Set_Match(T1_2, 2, C);
      Set_Match(T2_1, 1, C);
      Set_Match(T2_2, 2, A);
      Set_Match(T2_3, 3, B);
      T.Nfa.Initialize(3); -- 3 states
      T.Nfa.Set_State_Non_Accepting(1, 2);
      T.Nfa.Set_State_Transition(1, 1, T1_1);
      T.Nfa.Set_State_Transition(1, 2, T1_2);
      T.Nfa.Set_State_Non_Accepting(2, 3);
      T.Nfa.Set_State_Transition(2, 1, T2_1);
      T.Nfa.Set_State_Transition(2, 2, T2_2);
      T.Nfa.Set_State_Transition(2, 3, T2_3);
      T.Nfa.Set_State_Accepting(3, 13); -- Payload 13 (and no transitions)
   end Set_Up;




   ----------------------------------------------------------------------------
   type Sanity_Check_Nfa_State_Test is new Nfa_State_Test_Class with null record;
   procedure Run(T : in out Sanity_Check_Nfa_State_Test) is
      use kv.apg.fast;
   begin
      T.Assert(T.Nfa.Image = ("[1{97=>2,99=>2}/2{99=>1,97=>2,98=>3}/(3:13){}]"), "Wrong image! Got <" & T.Nfa.Image & ">");
   end Run;

   ----------------------------------------------------------------------------
   type Init_Nfa_State_Test is new Nfa_State_Test_Class with null record;
   procedure Run(T : in out Init_Nfa_State_Test) is
      use kv.apg.fast;
   begin
      T.Uut.Initialize(T.Nfa'UNCHECKED_ACCESS);
      T.Assert(not T.Uut.Is_Accepting, "Should not be accepting yet");
      T.Assert(T.Uut.Active_State_Count = 1, "Should have only one active state");
      T.Assert(not T.Uut.Is_Terminal, "Should not be terminal yet");
      T.Assert(not T.Uut.Is_Failed, "Should not be failed yet");
      T.Assert(T.Uut.Move_Count = 0, "Should have no moves yet");
   end Run;

   ----------------------------------------------------------------------------
   type Failed_Nfa_State_Test is new Nfa_State_Test_Class with null record;
   procedure Run(T : in out Failed_Nfa_State_Test) is
      use kv.apg.fast;
      X : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('x'));
   begin
      T.Uut.Initialize(T.Nfa'UNCHECKED_ACCESS);
      T.Uut.Ingest(X);
      T.Assert(T.Uut.Move_Count = 1, "Should have 1 move");
      T.Assert(not T.Uut.Is_Accepting, "Should not be accepting");
      T.Assert(T.Uut.Active_State_Count = 0, "Should have no active states");
      T.Assert(not T.Uut.Is_Terminal, "Should not be terminal");
      T.Assert(T.Uut.Is_Failed, "Should be failed");
   end Run;

   ----------------------------------------------------------------------------
   type Short_1_Nfa_State_Test is new Nfa_State_Test_Class with null record;
   procedure Run(T : in out Short_1_Nfa_State_Test) is
   begin
      T.Uut.Initialize(T.Nfa'UNCHECKED_ACCESS);
      Ingest_All(T, "ab");
      T.Assert(T.Uut.Move_Count = 2, "Should have 2 move");
      T.Assert(T.Uut.Is_Accepting, "Should be accepting");
      T.Assert(T.Uut.Active_State_Count = 1, "Should have one active state");
      T.Assert(T.Uut.Is_Terminal, "Should be terminal");
      T.Assert(not T.Uut.Is_Failed, "Should not be failed");
      T.Assert(T.Uut.Get_Key = 13, "Should be key = 13, was " & Key_Type'IMAGE(T.Uut.Get_Key));
   end Run;

   ----------------------------------------------------------------------------
   type Short_2_Nfa_State_Test is new Nfa_State_Test_Class with null record;
   procedure Run(T : in out Short_2_Nfa_State_Test) is
   begin
      T.Uut.Initialize(T.Nfa'UNCHECKED_ACCESS);
      Ingest_All(T, "cb");
      T.Assert(T.Uut.Move_Count = 2, "Should have 2 move");
      T.Assert(T.Uut.Is_Accepting, "Should be accepting");
      T.Assert(T.Uut.Active_State_Count = 1, "Should have one active state");
      T.Assert(T.Uut.Is_Terminal, "Should be terminal");
      T.Assert(not T.Uut.Is_Failed, "Should not be failed");
   end Run;

   ----------------------------------------------------------------------------
   type Long_1_Nfa_State_Test is new Nfa_State_Test_Class with null record;
   procedure Run(T : in out Long_1_Nfa_State_Test) is
   begin
      T.Uut.Initialize(T.Nfa'UNCHECKED_ACCESS);
      Ingest_All(T, "ccab");
      T.Assert(T.Uut.Move_Count = 4, "Should have 4 move");
      T.Assert(T.Uut.Is_Accepting, "Should be accepting");
      T.Assert(T.Uut.Active_State_Count = 1, "Should have one active state");
      T.Assert(T.Uut.Is_Terminal, "Should be terminal");
      T.Assert(not T.Uut.Is_Failed, "Should not be failed");
   end Run;

   ----------------------------------------------------------------------------
   type Long_2_Nfa_State_Test is new Nfa_State_Test_Class with null record;
   procedure Run(T : in out Long_2_Nfa_State_Test) is
   begin
      T.Uut.Initialize(T.Nfa'UNCHECKED_ACCESS);
      Ingest_All(T, "accaccaaaaab");
      T.Assert(T.Uut.Move_Count = 12, "Should have 12 move");
      T.Assert(T.Uut.Is_Accepting, "Should be accepting");
      T.Assert(T.Uut.Active_State_Count = 1, "Should have one active state");
      T.Assert(T.Uut.Is_Terminal, "Should be terminal");
      T.Assert(not T.Uut.Is_Failed, "Should not be failed");
   end Run;

   ----------------------------------------------------------------------------
   type Long_3_Nfa_State_Test is new Nfa_State_Test_Class with null record;
   procedure Run(T : in out Long_3_Nfa_State_Test) is
   begin
      T.Uut.Initialize(T.Nfa'UNCHECKED_ACCESS);
      Ingest_All(T, "accaccaaaaa");
      T.Assert(T.Uut.Move_Count = 11, "Should have 12 move");
      T.Assert(not T.Uut.Is_Accepting, "Should not be accepting");
      T.Assert(T.Uut.Active_State_Count = 1, "Should have one active state");
      T.Assert(not T.Uut.Is_Terminal, "Should not be terminal");
      T.Assert(not T.Uut.Is_Failed, "Should not be failed");
   end Run;

   ----------------------------------------------------------------------------
   type Long_4_Nfa_State_Test is new Nfa_State_Test_Class with null record;
   procedure Run(T : in out Long_4_Nfa_State_Test) is
   begin
      T.Uut.Initialize(T.Nfa'UNCHECKED_ACCESS);
      Ingest_All(T, "accaccaaaaqa");
      T.Assert(T.Uut.Move_Count = 12, "Should have 12 move");
      T.Assert(not T.Uut.Is_Accepting, "Should not be accepting");
      T.Assert(T.Uut.Active_State_Count = 0, "Should have no active states");
      T.Assert(not T.Uut.Is_Terminal, "Should not be terminal");
      T.Assert(T.Uut.Is_Failed, "Should be failed");
   end Run;


   package Static_Nfa_Definition is
      A : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('a'));
      B : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('b'));
      C : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('c'));
      D : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('d'));
      E : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('e'));
      T1 : aliased Transition_List_Type := (1 => (Any, 1), 2 => (Match, 2, A));
      T2 : aliased Transition_List_Type := (1 => (Match, 3, B));
      T3 : aliased Transition_List_Type := (1 => (Match, 4, C), 2 => (Match, 3, B));
      State_List : aliased State_List_Type :=
         (1 => (1, False, 0, T1'ACCESS),
          2 => (2, False, 0, T2'ACCESS),
          3 => (3, False, 0, T3'ACCESS),
          4 => (4, True, 1, null)
          );
   end Static_Nfa_Definition;

   type Static_Nfa_State_Test_Class is abstract new Base_Nfa_State_Test_Class with null record;

   ----------------------------------------------------------------------------
   overriding procedure Set_Up(T : in out Static_Nfa_State_Test_Class) is
   begin
      T.Nfa.Initialize(Static_Nfa_Definition.State_List'ACCESS);
      T.Uut.Initialize(T.Nfa'UNCHECKED_ACCESS);
   end Set_Up;

   ----------------------------------------------------------------------------
   type Abc_State_Test is new Static_Nfa_State_Test_Class with null record;
   procedure Run(T : in out Abc_State_Test) is
   begin
      Ingest_All(T, "abc");
      T.Assert(T.Uut.Move_Count = 3, "Should have 3 move");
      T.Assert(T.Uut.Is_Accepting, "Should be accepting");
      T.Assert(T.Uut.Active_State_Count = 2, "Should have two active states");
      T.Assert(T.Uut.Is_Terminal, "Should be terminal");
      T.Assert(not T.Uut.Is_Failed, "Should not be failed");
   end Run;

   ----------------------------------------------------------------------------
   type False_Start_State_Test is new Static_Nfa_State_Test_Class with null record;
   procedure Run(T : in out False_Start_State_Test) is
   begin
      Ingest_All(T, "abxxxabc");
      T.Assert(T.Uut.Is_Accepting, "Should be accepting");
      T.Assert(T.Uut.Active_State_Count = 2, "Should have two active states");
      T.Assert(T.Uut.Is_Terminal, "Should be terminal");
      T.Assert(not T.Uut.Is_Failed, "Should not be failed");
   end Run;

   ----------------------------------------------------------------------------
   type Longer_State_Test is new Static_Nfa_State_Test_Class with null record;
   procedure Run(T : in out Longer_State_Test) is
   begin
      Ingest_All(T, "aqwsdewasdcdesawdrdfgdewaabbbbbbbbbbbbbbbbbbbbbbbbbbbc");
      T.Assert(T.Uut.Is_Accepting, "Should be accepting");
      T.Assert(T.Uut.Active_State_Count = 2, "Should have two active states");
      T.Assert(T.Uut.Is_Terminal, "Should be terminal");
      T.Assert(not T.Uut.Is_Failed, "Should not be failed");
   end Run;

   ----------------------------------------------------------------------------
   type Reset_Start_State_Test is new Static_Nfa_State_Test_Class with null record;
   procedure Run(T : in out Reset_Start_State_Test) is
   begin
      Ingest_All(T, "aqwsdewasdcdesawdrdfgdewaabbbbbbbbbbbbbbbbbbbbbbbbbbbc");
      T.Uut.Reset;
      Ingest_All(T, "abxxxabc");
      T.Assert(T.Uut.Is_Accepting, "Should be accepting");
      T.Assert(T.Uut.Active_State_Count = 2, "Should have two active states");
      T.Assert(T.Uut.Is_Terminal, "Should be terminal");
      T.Assert(not T.Uut.Is_Failed, "Should not be failed");
   end Run;


   ----------------------------------------------------------------------------
   type RegEx_Nfa_Test_Class is abstract new Base_Nfa_State_Test_Class with
      record
         Lexer : kv.apg.lex.Lexer_Class;
         Parser : kv.apg.parse.Parser_Class;
      end record;

   ----------------------------------------------------------------------------
   procedure Parse_Line(T : in out RegEx_Nfa_Test_Class'CLASS; S : in String) is
      WS : constant Wide_Wide_String := To_Wide_Wide_String(S & Ada.Characters.Latin_1.LF);
      Token : kv.apg.tokens.Token_Class;
   begin
      for WC of WS loop
         T.Lexer.Ingest_Character(WC);
      end loop;
      while T.Lexer.Tokens_Available > 0 loop
         Token := T.Lexer.Get_Next_Token;
         T.Parser.Ingest_Token(Token);
      end loop;
   end Parse_Line;

   ----------------------------------------------------------------------------
   procedure Recognize_Pattern
      (T       : in out Base_Nfa_State_Test_Class'CLASS;
       Pattern : in     String) is
   begin
      T.Uut.Reset;
      Ingest_All(T, Pattern);
      T.Assert(T.Uut.Is_Accepting, "Pattern <"&Pattern&"> should be accepting");
      T.Assert(T.Uut.Is_Terminal, "Pattern <"&Pattern&"> should be terminal");
      T.Assert(not T.Uut.Is_Failed, "Pattern <"&Pattern&"> should not be failed");
   end Recognize_Pattern;

   ----------------------------------------------------------------------------
   procedure Reject_Pattern
      (T       : in out Base_Nfa_State_Test_Class'CLASS;
       Pattern : in     String;
       Failed  : in     Boolean := True) is
   begin
      T.Uut.Reset;
      Ingest_All(T, Pattern);
      T.Assert(not T.Uut.Is_Accepting, "Pattern <"&Pattern&"> should not be accepting");
      if Failed then
         T.Assert(T.Uut.Is_Failed, "Pattern <"&Pattern&"> should be failed");
      else
         T.Assert(not T.Uut.Is_Failed, "Pattern <"&Pattern&"> should not be failed");
      end if;
   end Reject_Pattern;

   ----------------------------------------------------------------------------
   type RegEx_To_Nfa_1_Test is new RegEx_Nfa_Test_Class with null record;
   procedure Run(T : in out RegEx_To_Nfa_1_Test) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
      Expected_Image : constant String := "[1{ɛ=>2,ɛ=>6}/2{97=>3}/3{98=>4}/4{99=>5}/5{ɛ=>2,ɛ=>6}/6{100=>7}/(7:13){}]";
   begin
      Parse_Line(T, "token foo = ('a' 'b' 'c') * 'd';");
      Directive := T.Parser.Next_Directive;

      --kv.apg.regex.Set_Debug(True);
      T.Nfa := kv.apg.directives.Token_Class'CLASS(Directive.all).Get_Tree.To_Nfa(13);
      --kv.apg.regex.Set_Debug(False);

      T.Assert(T.Nfa.Image = Expected_Image, "Wrong image! Expected <"&Expected_Image&">, got <" & T.Nfa.Image & ">");

      T.Uut.Initialize(T.Nfa'UNCHECKED_ACCESS);

      --kv.apg.fast.Set_Debug(True);
      --kv.apg.fa.nfa.Set_Debug(True);

      Reject_Pattern(T, "w");
      Recognize_Pattern(T, "d"); -- 0
      Recognize_Pattern(T, "abcd"); -- 1
      Recognize_Pattern(T, "abcabcabcd"); -- 3
      Reject_Pattern(T, "abcabcabcdd");

      --kv.apg.fast.Set_Debug(False);
      --kv.apg.fa.nfa.Set_Debug(False);

      kv.apg.directives.Free(Directive);
   end Run;

   ----------------------------------------------------------------------------
   type RegEx_To_Nfa_2_Test is new RegEx_Nfa_Test_Class with null record;
   procedure Run(T : in out RegEx_To_Nfa_2_Test) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
      Expected_Image : constant String := "[1{ɛ=>2,ɛ=>4}/2{any=>3}/3{ɛ=>2,ɛ=>4}/4{100=>5}/(5:8){}]";
   begin
      Parse_Line(T, "token foo = . * 'd';");
      Directive := T.Parser.Next_Directive;

      --kv.apg.regex.Set_Debug(True);
      T.Nfa := kv.apg.directives.Token_Class'CLASS(Directive.all).Get_Tree.To_Nfa(8);

      T.Assert(T.Nfa.Image = Expected_Image, "Wrong image! Expected <"&Expected_Image&">, got <" & T.Nfa.Image & ">");

      T.Uut.Initialize(T.Nfa'UNCHECKED_ACCESS);

      Recognize_Pattern(T, "d");
      Recognize_Pattern(T, "KSDFKJASDFKJFDGKJFd");
      Reject_Pattern(T, "dw", False); -- not failed because we haven't run off the end of a pattern

      kv.apg.directives.Free(Directive);
      --kv.apg.regex.Set_Debug(False);
   end Run;

   ----------------------------------------------------------------------------
   type RegEx_To_Nfa_3_Test is new RegEx_Nfa_Test_Class with null record;
   procedure Run(T : in out RegEx_To_Nfa_3_Test) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
      Expected_Image : constant String := "[1{ɛ=>2,ɛ=>6}/2{97=>3}/3{98=>4}/4{99=>5}/5{ɛ=>10}/6{100=>7}/7{101=>8}/8{102=>9}/9{ɛ=>10}/(10:7){}]";
   begin
      Parse_Line(T, "token foo = ""abc"" | ""def"";");
      Directive := T.Parser.Next_Directive;

--      kv.apg.regex.Set_Debug(True);
--      T.Log("Tree = " & To_String(+kv.apg.directives.Token_Class'CLASS(Directive.all).Get_Tree.Image_Tree));
      T.Nfa := kv.apg.directives.Token_Class'CLASS(Directive.all).Get_Tree.To_Nfa(7);

      T.Assert(T.Nfa.Image = Expected_Image, "Wrong image! Expected <"&Expected_Image&">, got <" & T.Nfa.Image & ">");

      T.Uut.Initialize(T.Nfa'UNCHECKED_ACCESS);

      Recognize_Pattern(T, "abc");
      Recognize_Pattern(T, "def");
      Reject_Pattern(T, "bar");

      kv.apg.directives.Free(Directive);
      kv.apg.regex.Set_Debug(False);
   end Run;

   ----------------------------------------------------------------------------
   type RegEx_To_Nfa_4_Test is new RegEx_Nfa_Test_Class with null record;
   procedure Run(T : in out RegEx_To_Nfa_4_Test) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
      Expected_Image : constant String := "[1{97=>2}/2{ɛ=>1,ɛ=>3}/3{98=>4}/4{ɛ=>3,ɛ=>5}/5{99=>6}/6{ɛ=>5,ɛ=>7}/(7:99){}]";
   begin
      Parse_Line(T, "token foo = 'a' + 'b' + 'c' +;");
      Directive := T.Parser.Next_Directive;

      --kv.apg.regex.Set_Debug(True);
      T.Nfa := kv.apg.directives.Token_Class'CLASS(Directive.all).Get_Tree.To_Nfa(99);

      T.Assert(T.Nfa.Image = Expected_Image, "Wrong image! Expected <"&Expected_Image&">, got <" & T.Nfa.Image & ">");

      T.Uut.Initialize(T.Nfa'UNCHECKED_ACCESS);

      Recognize_Pattern(T, "abc");
      Recognize_Pattern(T, "aaaaaaaaaaabcc");
      Reject_Pattern(T, "bc");

      kv.apg.directives.Free(Directive);
      --kv.apg.regex.Set_Debug(False);
   end Run;

   ----------------------------------------------------------------------------
   type RegEx_To_Nfa_5_Test is new RegEx_Nfa_Test_Class with null record;
   procedure Run(T : in out RegEx_To_Nfa_5_Test) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
      Expected_Image : constant String := "[1{97=>2}/2{98=>3,ɛ=>3}/3{99=>4}/(4:7){}]";
   begin
      Parse_Line(T, "token foo = 'a' ('b' ?) 'c';");
      Directive := T.Parser.Next_Directive;

      --kv.apg.regex.Set_Debug(True);
      T.Nfa := kv.apg.directives.Token_Class'CLASS(Directive.all).Get_Tree.To_Nfa(7);
      --kv.apg.regex.Set_Debug(False);

      T.Assert(T.Nfa.Image = Expected_Image, "Wrong image! Expected <"&Expected_Image&">, got <" & T.Nfa.Image & ">");

      T.Uut.Initialize(T.Nfa'UNCHECKED_ACCESS);

      --kv.apg.fast.Set_Debug(True);
      --kv.apg.fa.nfa.Set_Debug(True);

      Recognize_Pattern(T, "abc");
      Recognize_Pattern(T, "ac");
      Reject_Pattern(T, "abbc");

      --kv.apg.fast.Set_Debug(False);
      --kv.apg.fa.nfa.Set_Debug(False);

      kv.apg.directives.Free(Directive);
   end Run;



   ----------------------------------------------------------------------------
   type Base_Dfa_State_Test_Class is abstract new kv.core.ut.Test_Class with
      record
         Dfa : aliased kv.apg.fa.dfa.Dfa_Class;
         Uut : aliased kv.apg.fa.dfa.Dfa_State_Class;
      end record;

   ----------------------------------------------------------------------------
   procedure Ingest_All(T : in out Base_Dfa_State_Test_Class'CLASS; S : in String) is
      WS : constant Wide_Wide_String := To_Wide_Wide_String(S);
   begin
      for WC of WS loop
         T.Uut.Ingest(WC);
      end loop;
   end Ingest_All;

   package Static_Dfa_Definition is
      A : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('a'));
      B : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('b'));
      C : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('c'));
      D : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('d'));
      E : constant Wide_Wide_Character := To_Wide_Wide_Character(Character'('e'));
      T1 : aliased Transition_List_Type := (1 => (Match, 2, A));
      T2 : aliased Transition_List_Type := (1 => (Match, 3, B), 2 => (Match, 2, A));
      T3 : aliased Transition_List_Type := (1 => (Match, 4, C), 2 => (Match, 2, A), 3 => (Match, 3, B));
      T4 : aliased Transition_List_Type := (1 => (Match, 5, D), 2 => (Match, 2, A), 3 => (Match, 3, B), 4 => (Match, 4, C));
      T5 : aliased Transition_List_Type := (1 => (Match, 5, D));
      State_List : aliased State_List_Type :=
         (1 => (1, False, 0, T1'ACCESS),
          2 => (2, False, 0, T2'ACCESS),
          3 => (3, False, 0, T3'ACCESS),
          4 => (4, False, 0, T4'ACCESS),
          5 => (5, True, 1, T5'ACCESS)
          );
   end Static_Dfa_Definition;

   ----------------------------------------------------------------------------
   type Dfa_State_Test_Class is abstract new Base_Dfa_State_Test_Class with null record;

   ----------------------------------------------------------------------------
   overriding procedure Set_Up(T : in out Dfa_State_Test_Class) is
   begin
      --kv.apg.fa.dfa.Set_Debug(True);
      T.Dfa.Initialize(Static_Dfa_Definition.State_List'ACCESS);
      T.Uut.Initialize(T.Dfa'UNCHECKED_ACCESS);
      kv.apg.fa.dfa.Set_Debug(False);
   end Set_Up;

   ----------------------------------------------------------------------------
   type Dfa_1_State_Test is new Dfa_State_Test_Class with null record;
   procedure Run(T : in out Dfa_1_State_Test) is
   begin
      --kv.apg.fa.dfa.Set_Debug(True);
      Ingest_All(T, "abcd");
      T.Assert(T.Uut.Is_Accepting, "Should be accepting");
      T.Assert(not T.Uut.Is_Terminal, "Should not be terminal");
      T.Assert(not T.Uut.Is_Failed, "Should not be failed");
      --kv.apg.fa.dfa.Set_Debug(False);
   end Run;

   ----------------------------------------------------------------------------
   type Dfa_2_State_Test is new Dfa_State_Test_Class with null record;
   procedure Run(T : in out Dfa_2_State_Test) is
   begin
      --kv.apg.fa.dfa.Set_Debug(True);
      Ingest_All(T, "abcde");
      T.Assert(not T.Uut.Is_Accepting, "Should not be accepting");
      T.Assert(T.Uut.Is_Failed, "Should be failed");
      --kv.apg.fa.dfa.Set_Debug(False);
   end Run;

   ----------------------------------------------------------------------------
   type Dfa_3_State_Test is new Dfa_State_Test_Class with null record;
   procedure Run(T : in out Dfa_3_State_Test) is
   begin
      --kv.apg.fa.dfa.Set_Debug(True);
      Ingest_All(T, "abcabbbbcbcdddd");
      T.Assert(T.Uut.Is_Accepting, "Should be accepting");
      T.Assert(not T.Uut.Is_Terminal, "Should not be terminal");
      T.Assert(not T.Uut.Is_Failed, "Should not be failed");
      --kv.apg.fa.dfa.Set_Debug(False);
   end Run;


   ----------------------------------------------------------------------------
   type Base_Nfa_Convert_Test_Class is abstract new kv.core.ut.Test_Class with
      record
         Lexer : kv.apg.lex.Lexer_Class;
         Parser : kv.apg.parse.Parser_Class;
         Directive : kv.apg.directives.Directive_Pointer_Type;
         Nfa : aliased kv.apg.fa.nfa.Nfa_Class;
         Dfa : aliased kv.apg.fa.dfa.Dfa_Class;
      end record;

   ----------------------------------------------------------------------------
   type Base_Nfa_To_Dfa_Test_Class is abstract new Base_Nfa_Convert_Test_Class with
      record
         Uut : aliased kv.apg.fa.nfa.convert.To_Dfa_Class;
      end record;
   ----------------------------------------------------------------------------
   type Base_Nfa_To_Cnfa_Test_Class is abstract new Base_Nfa_Convert_Test_Class with
      record
         Uut : aliased kv.apg.fa.nfa.convert.To_Cnfa_Class;
      end record;

   ----------------------------------------------------------------------------
   overriding procedure Tear_Down(T : in out Base_Nfa_Convert_Test_Class) is
      use kv.apg.directives;
   begin
      if T.Directive /= null then
         kv.apg.directives.Free(T.Directive);
      end if;
   end Tear_Down;

   ----------------------------------------------------------------------------
   procedure Parse_Line(T : in out Base_Nfa_Convert_Test_Class'CLASS; S : in String) is
      WS : constant Wide_Wide_String := To_Wide_Wide_String(S & Ada.Characters.Latin_1.LF);
      Token : kv.apg.tokens.Token_Class;
   begin
      --T.Log("Ingesting <"&S&">");
      for WC of WS loop
         T.Lexer.Ingest_Character(WC);
      end loop;
      while T.Lexer.Tokens_Available > 0 loop
         Token := T.Lexer.Get_Next_Token;
         T.Parser.Ingest_Token(Token);
      end loop;
      --T.Log("Done parsing. Directive count = " & Natural'IMAGE(T.Parser.Directive_Count));
   end Parse_Line;

   ----------------------------------------------------------------------------
   procedure Prepare_Nfa
      (T     : in out Base_Nfa_Convert_Test_Class'CLASS;
       RegEx : in     String) is
   begin
      Parse_Line(T, "token foo = "&RegEx&";");
      T.Directive := T.Parser.Next_Directive;
      T.Nfa := kv.apg.directives.Token_Class'CLASS(T.Directive.all).Get_Tree.To_Nfa(96);
   end Prepare_Nfa;

   ----------------------------------------------------------------------------
   type Nfa_To_Dfa_1_Test is new Base_Nfa_To_Dfa_Test_Class with null record;
   procedure Run(T : in out Nfa_To_Dfa_1_Test) is
      Expected_Image : constant String := "[1{100=>2}/(2:96){}]";
   begin
      Prepare_Nfa(T, " 'd' "); -- No epsilon or nondeterministic transitions
      T.Assert(T.Nfa.Image = Expected_Image, "Wrong NFA image! Expected <"&Expected_Image&">, got <" & T.Nfa.Image & ">");
      T.Uut.Nfa_To_Dfa(T.Nfa, T.Dfa);
      T.Assert(T.Dfa.Image = Expected_Image, "Wrong DFA image! Expected <"&Expected_Image&">, got <" & T.Dfa.Image & ">");
   end Run;

   ----------------------------------------------------------------------------
   type Nfa_To_Cnfa_1_Test is new Base_Nfa_To_Cnfa_Test_Class with null record;
   procedure Run(T : in out Nfa_To_Cnfa_1_Test) is
      Nfa_Image : constant String := "[1{97=>2}/2{98=>3,ɛ=>3}/3{99=>4}/(4:96){}]";
      Uut_Image : constant String := "[1{97=>2}/2{ɛ=>5,ɛ=>3}/3{99=>4}/(4:96){}/5{98=>3}]";
   begin
      Prepare_Nfa(T, " 'a' ('b' ?) 'c' "); -- one epsilon
      T.Assert(T.Nfa.Image = Nfa_Image, "Wrong NFA image! Expected <"&Nfa_Image&">, got <" & T.Nfa.Image & ">");
      T.Uut.Internal_Set_Nfa(T.Nfa);
      T.Uut.Internal_Unmix_Epsilon_Transitions;
      T.Assert(T.Uut.Image = Uut_Image, "Wrong UUT image! Expected <"&Uut_Image&">, got <" & T.Uut.Image & ">");
   end Run;

   ----------------------------------------------------------------------------
   type Nfa_To_Cnfa_2_Test is new Base_Nfa_To_Cnfa_Test_Class with null record;
   procedure Run(T : in out Nfa_To_Cnfa_2_Test) is
      Nfa_Image : constant String := "[1{ɛ=>2,ɛ=>4}/2{97=>3}/3{ɛ=>10}/4{ɛ=>5,ɛ=>7}/5{98=>6}/6{ɛ=>9}/7{99=>8}/8{ɛ=>9}/9{ɛ=>10}/(10:96){}]";
      Uut_Image : constant String := "[1{ɛ=>2,ɛ=>5,ɛ=>7}/2{97=>3}/3{ɛ=>10}/4{ɛ=>5,ɛ=>7}/5{98=>6}/6{ɛ=>10}/7{99=>8}/8{ɛ=>10}/9{ɛ=>10}/(10:96){}]";
   begin
      Prepare_Nfa(T, " 'a' | ('b' | 'c') ");
      T.Assert(T.Nfa.Image = Nfa_Image, "Wrong NFA image! Expected <"&Nfa_Image&">, got <" & T.Nfa.Image & ">");
      T.Uut.Internal_Set_Nfa(T.Nfa);
      T.Uut.Internal_Unchain_Epsilon_Transitions;
      T.Assert(T.Uut.Image = Uut_Image, "Wrong UUT image! Expected <"&Uut_Image&">, got <" & T.Uut.Image & ">");
   end Run;

   ----------------------------------------------------------------------------
   type Nfa_To_Cnfa_3_Test is new Base_Nfa_To_Cnfa_Test_Class with null record;
   procedure Run(T : in out Nfa_To_Cnfa_3_Test) is
      Nfa_Image : constant String := "[1{ɛ=>2,ɛ=>4}/2{97=>3}/3{ɛ=>6}/4{98=>5}/5{ɛ=>6}/(6:96){}]";
      Uut_Image : constant String := "[1{97=>3,98=>5}/2{97=>3}/(3:96){}/4{98=>5}/(5:96){}/(6:96){}]";
   begin
      Prepare_Nfa(T, " 'a' | 'b' ");
      T.Assert(T.Nfa.Image = Nfa_Image, "Wrong NFA image! Expected <"&Nfa_Image&">, got <" & T.Nfa.Image & ">");
      T.Uut.Internal_Set_Nfa(T.Nfa);
      T.Uut.Internal_Collapse_Epsilon_Transitions;
      T.Assert(T.Uut.Image = Uut_Image, "Wrong UUT image! Expected <"&Uut_Image&">, got <" & T.Uut.Image & ">");
   end Run;

   ----------------------------------------------------------------------------
   type Nfa_To_Cnfa_4_Test is new Base_Nfa_To_Cnfa_Test_Class with null record;
   procedure Run(T : in out Nfa_To_Cnfa_4_Test) is
      Nfa_Image : constant String := "[1{ɛ=>2,ɛ=>4}/2{97=>3}/3{ɛ=>10}/4{ɛ=>5,ɛ=>7}/5{98=>6}/6{ɛ=>9}/7{99=>8}/8{ɛ=>9}/9{ɛ=>10}/(10:96){}]";
      Uut_Image : constant String := "[1{97=>3,98=>6,99=>8}/2{97=>3}/(3:96){}/4{98=>6,99=>8}/5{98=>6}/(6:96){}/7{99=>8}/(8:96){}/(9:96){}/(10:96){}]";
   begin
      Prepare_Nfa(T, " 'a' | ('b' | 'c') ");
      T.Assert(T.Nfa.Image = Nfa_Image, "Wrong NFA image! Expected <"&Nfa_Image&">, got <" & T.Nfa.Image & ">");
      T.Uut.Internal_Set_Nfa(T.Nfa);
      T.Uut.Internal_Banish_Epsilon_Transitions;
      T.Assert(T.Uut.Image = Uut_Image, "Wrong UUT image! Expected <"&Uut_Image&">, got <" & T.Uut.Image & ">");
   end Run;

   ----------------------------------------------------------------------------
   type Nfa_To_Cnfa_5_Test is new Base_Nfa_To_Cnfa_Test_Class with null record;
   procedure Run(T : in out Nfa_To_Cnfa_5_Test) is
      Nfa_Image : constant String := "[1{ɛ=>2,ɛ=>4}/2{97=>3}/3{ɛ=>10}/4{ɛ=>5,ɛ=>7}/5{98=>6}/6{ɛ=>9}/7{99=>8}/8{ɛ=>9}/9{ɛ=>10}/(10:96){}]";
      Uut_Image : constant String := "[1{ɛ=>2,ɛ=>4}/2{97=>10}/3{ɛ=>10}/4{ɛ=>5,ɛ=>7}/5{98=>6}/6{ɛ=>9}/7{99=>8}/8{ɛ=>9}/9{ɛ=>10}/(10:96){}]";
   begin
      Prepare_Nfa(T, " 'a' | ('b' | 'c') ");
      T.Assert(T.Nfa.Image = Nfa_Image, "Wrong NFA image! Expected <"&Nfa_Image&">, got <" & T.Nfa.Image & ">");
      T.Uut.Internal_Set_Nfa(T.Nfa);
      T.Uut.Internal_Retarget_Transitions(3, 10);
      T.Assert(T.Uut.Image = Uut_Image, "Wrong UUT image! Expected <"&Uut_Image&">, got <" & T.Uut.Image & ">");
   end Run;

   ----------------------------------------------------------------------------
   type Nfa_To_Cnfa_6_Test is new Base_Nfa_To_Cnfa_Test_Class with null record;
   procedure Run(T : in out Nfa_To_Cnfa_6_Test) is
      Nfa_Image : constant String := "[1{ɛ=>2,ɛ=>4}/2{97=>3}/3{ɛ=>10}/4{ɛ=>5,ɛ=>7}/5{98=>6}/6{ɛ=>9}/7{99=>8}/8{ɛ=>9}/9{ɛ=>10}/(10:96){}]";
      Uut_Image : constant String := "[1{ɛ=>2,ɛ=>3}/2{97=>9}/3{ɛ=>4,ɛ=>6}/4{98=>5}/5{ɛ=>8}/6{99=>7}/7{ɛ=>8}/8{ɛ=>9}/(9:96){}]";
   begin
      Prepare_Nfa(T, " 'a' | ('b' | 'c') ");
      T.Assert(T.Nfa.Image = Nfa_Image, "Wrong NFA image! Expected <"&Nfa_Image&">, got <" & T.Nfa.Image & ">");
      T.Uut.Internal_Set_Nfa(T.Nfa);
      T.Uut.Internal_Retarget_Transitions(3, 10);
      T.Uut.Internal_Delete(3);
      T.Assert(T.Uut.Image = Uut_Image, "Wrong UUT image! Expected <"&Uut_Image&">, got <" & T.Uut.Image & ">");
   end Run;

   ----------------------------------------------------------------------------
   type Nfa_To_Cnfa_7_Test is new Base_Nfa_To_Cnfa_Test_Class with null record;
   procedure Run(T : in out Nfa_To_Cnfa_7_Test) is
      Nfa_Image : constant String := "[1{ɛ=>2,ɛ=>4}/2{97=>3}/3{ɛ=>10}/4{ɛ=>5,ɛ=>7}/5{98=>6}/6{ɛ=>9}/7{99=>8}/8{ɛ=>9}/9{ɛ=>10}/(10:96){}]";
      Uut_Image : constant String := "[1{97=>3,98=>6,99=>8}/2{97=>3}/(3:96){}/4{98=>6,99=>8}/5{98=>6}/(6:96){}/7{99=>8}/(8:96){}/(9:96){}/(10:96){}]";
      Index : Natural;
   begin
      Prepare_Nfa(T, " 'a' | ('b' | 'c') ");
      T.Assert(T.Nfa.Image = Nfa_Image, "Wrong NFA image! Expected <"&Nfa_Image&">, got <" & T.Nfa.Image & ">");
      T.Uut.Internal_Set_Nfa(T.Nfa);
      T.Uut.Internal_Banish_Epsilon_Transitions;
      T.Assert(T.Uut.Image = Uut_Image, "Wrong UUT image! Expected <"&Uut_Image&">, got <" & T.Uut.Image & ">");
      Index := T.Uut.Internal_Duplicate_Of(3);
      T.Assert(Index = 6, "Duplicate of 3 should be 6, was " & Natural'IMAGE(Index));
      Index := T.Uut.Internal_Duplicate_Of(10);
      T.Assert(Index = 3, "Duplicate of 10 should be 3, was " & Natural'IMAGE(Index));
   end Run;

   ----------------------------------------------------------------------------
   type Nfa_To_Cnfa_8_Test is new Base_Nfa_To_Cnfa_Test_Class with null record;
   procedure Run(T : in out Nfa_To_Cnfa_8_Test) is
      Nfa_Image : constant String := "[1{ɛ=>2,ɛ=>4}/2{97=>3}/3{ɛ=>10}/4{ɛ=>5,ɛ=>7}/5{98=>6}/6{ɛ=>9}/7{99=>8}/8{ɛ=>9}/9{ɛ=>10}/(10:96){}]";
      Uut_Image : constant String := "[1{97=>3,98=>6,99=>8}/2{97=>3}/(3:96){}/4{98=>6,99=>8}/5{98=>6}/(6:96){}/7{99=>8}/(8:96){}/(9:96){}/(10:96){}]";
      Fin_Image : constant String := "[1{97=>3,98=>3,99=>3}/2{97=>3}/(3:96){}/4{98=>3,99=>3}/5{98=>3}/6{99=>3}]";
   begin
      Prepare_Nfa(T, " 'a' | ('b' | 'c') ");
      T.Assert(T.Nfa.Image = Nfa_Image, "Wrong NFA image! Expected <"&Nfa_Image&">, got <" & T.Nfa.Image & ">");
      T.Uut.Internal_Set_Nfa(T.Nfa);
      T.Uut.Internal_Banish_Epsilon_Transitions;
      T.Assert(T.Uut.Image = Uut_Image, "Wrong UUT image! Expected <"&Uut_Image&">, got <" & T.Uut.Image & ">");
      T.Uut.Internal_Remove_Duplicates;
      T.Assert(T.Uut.Image = Fin_Image, "Wrong Fin image! Expected <"&Fin_Image&">, got <" & T.Uut.Image & ">");
   end Run;

   ----------------------------------------------------------------------------
   type Nfa_To_Cnfa_9_Test is new Base_Nfa_To_Cnfa_Test_Class with null record;
   procedure Run(T : in out Nfa_To_Cnfa_9_Test) is
      Nfa_Image : constant String := "[1{ɛ=>2,ɛ=>4}/2{97=>3}/3{ɛ=>10}/4{ɛ=>5,ɛ=>7}/5{98=>6}/6{ɛ=>9}/7{99=>8}/8{ɛ=>9}/9{ɛ=>10}/(10:96){}]";
      Uut_Image : constant String := "[1{97=>3,98=>3,99=>3}/2{97=>3}/(3:96){}/4{98=>3,99=>3}/5{98=>3}/6{99=>3}]";
      Fin_Image : constant String := "[1{97=>2,98=>2,99=>2}/(2:96){}]";
   begin
      Prepare_Nfa(T, " 'a' | ('b' | 'c') ");
      T.Assert(T.Nfa.Image = Nfa_Image, "Wrong NFA image! Expected <"&Nfa_Image&">, got <" & T.Nfa.Image & ">");
      T.Uut.Internal_Set_Nfa(T.Nfa);
      T.Uut.Internal_Banish_Epsilon_Transitions;
      T.Uut.Internal_Remove_Duplicates;
      T.Assert(T.Uut.Image = Uut_Image, "Wrong UUT image! Expected <"&Uut_Image&">, got <" & T.Uut.Image & ">");
      T.Uut.Internal_Remove_Unreachables;
      T.Assert(T.Uut.Image = Fin_Image, "Wrong Fin image! Expected <"&Fin_Image&">, got <" & T.Uut.Image & ">");
   end Run;

   ----------------------------------------------------------------------------
   type Nfa_To_Cnfa_10_Test is new Base_Nfa_To_Cnfa_Test_Class with null record;
   procedure Run(T : in out Nfa_To_Cnfa_10_Test) is
      Clean_NFA_Image : constant String := "[1{97=>2,98=>2,99=>2}/(2:96){}]";
      Cnfa : aliased kv.apg.fa.nfa.Nfa_Class;
   begin
      Prepare_Nfa(T, " 'a' | ('b' | 'c') ");
      --T.Log("++++++++++++++++++++Nfa_To_Cnfa_10_Test");
      --kv.apg.fa.nfa.convert.Set_Debug(True);
      T.Uut.Nfa_To_Cnfa(T.Nfa, Cnfa);
      T.Assert(Cnfa.Image = Clean_NFA_Image, "Wrong Clean NFA image! Expected <"&Clean_NFA_Image&">, got <" & Cnfa.Image & ">");
      --kv.apg.fa.nfa.convert.Set_Debug(False);
   end Run;

   ----------------------------------------------------------------------------
   type Nfa_To_Cnfa_11_Test is new Base_Nfa_To_Cnfa_Test_Class with null record;
   procedure Run(T : in out Nfa_To_Cnfa_11_Test) is
      Clean_NFA_Image : constant String := "[1{109=>2,109=>7}/2{111=>3}/3{100=>4}/4{101=>5}/5{108=>6}/(6:96){}/7{109=>7,111=>8}/8{111=>8,100=>6}]";
      Cnfa : aliased kv.apg.fa.nfa.Nfa_Class;
   begin
      Prepare_Nfa(T, " ""model"" | ( 'm' + 'o' + 'd' ) ");
      T.Uut.Nfa_To_Cnfa(T.Nfa, Cnfa);
      T.Assert(Cnfa.Image = Clean_NFA_Image, "Wrong Clean NFA image! Expected <"&Clean_NFA_Image&">, got <" & Cnfa.Image & ">");
   end Run;



   ----------------------------------------------------------------------------
   procedure register(suite : in kv.core.ut.Suite_Pointer_Type) is
   begin
      suite.register(new Init_Nfa_Test, "Init_Nfa_Test");
      suite.register(new Image_Nfa_Test, "Image_Nfa_Test");
      suite.register(new Combine_Nfa_Test, "Combine_Nfa_Test");

      suite.register(new Sanity_Check_Nfa_State_Test, "Sanity_Check_Nfa_State_Test");
      suite.register(new Init_Nfa_State_Test, "Init_Nfa_State_Test");
      suite.register(new Failed_Nfa_State_Test, "Failed_Nfa_State_Test");
      suite.register(new Short_1_Nfa_State_Test, "Short_1_Nfa_State_Test");
      suite.register(new Short_2_Nfa_State_Test, "Short_2_Nfa_State_Test");
      suite.register(new Long_1_Nfa_State_Test, "Long_1_Nfa_State_Test");
      suite.register(new Long_2_Nfa_State_Test, "Long_2_Nfa_State_Test");
      suite.register(new Long_3_Nfa_State_Test, "Long_3_Nfa_State_Test");
      suite.register(new Long_4_Nfa_State_Test, "Long_4_Nfa_State_Test");

      suite.register(new Abc_State_Test, "Abc_State_Test");
      suite.register(new False_Start_State_Test, "False_Start_State_Test");
      suite.register(new Longer_State_Test, "Longer_State_Test");
      suite.register(new Reset_Start_State_Test, "Reset_Start_State_Test");

      suite.register(new RegEx_To_Nfa_1_Test, "RegEx_To_Nfa_1_Test");
      suite.register(new RegEx_To_Nfa_2_Test, "RegEx_To_Nfa_2_Test");
      suite.register(new RegEx_To_Nfa_3_Test, "RegEx_To_Nfa_3_Test");
      suite.register(new RegEx_To_Nfa_4_Test, "RegEx_To_Nfa_4_Test");
      suite.register(new RegEx_To_Nfa_5_Test, "RegEx_To_Nfa_5_Test");

      suite.register(new Dfa_1_State_Test, "Dfa_1_State_Test");
      suite.register(new Dfa_2_State_Test, "Dfa_2_State_Test");
      suite.register(new Dfa_3_State_Test, "Dfa_3_State_Test");

      suite.register(new Nfa_To_Dfa_1_Test, "Nfa_To_Dfa_1_Test");
--      suite.register(new XXX, "XXX");

      suite.register(new Nfa_To_Cnfa_1_Test, "Nfa_To_Cnfa_1_Test");
      suite.register(new Nfa_To_Cnfa_2_Test, "Nfa_To_Cnfa_2_Test");
      suite.register(new Nfa_To_Cnfa_3_Test, "Nfa_To_Cnfa_3_Test");
      suite.register(new Nfa_To_Cnfa_4_Test, "Nfa_To_Cnfa_4_Test");
      suite.register(new Nfa_To_Cnfa_5_Test, "Nfa_To_Cnfa_5_Test");
      suite.register(new Nfa_To_Cnfa_6_Test, "Nfa_To_Cnfa_6_Test");
      suite.register(new Nfa_To_Cnfa_7_Test, "Nfa_To_Cnfa_7_Test");
      suite.register(new Nfa_To_Cnfa_8_Test, "Nfa_To_Cnfa_8_Test");
      suite.register(new Nfa_To_Cnfa_9_Test, "Nfa_To_Cnfa_9_Test");
      suite.register(new Nfa_To_Cnfa_10_Test, "Nfa_To_Cnfa_10_Test");
      suite.register(new Nfa_To_Cnfa_11_Test, "Nfa_To_Cnfa_11_Test");
--      suite.register(new XXX, "XXX");

   end register;

end kv.apg.tests.lex_xfa;
