with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;

with kv.apg.lex;

package body kv.apg.tests is

   use Ada.Characters.Conversions;

   Pi : constant Wide_Wide_Character := Wide_Wide_Character'VAL(16#03C0#);
   Three : constant Wide_Wide_Character := '3';
   Space : constant Wide_Wide_Character := ' ';
   Open_Block : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Left_Angle_Quotation);
   Close_Block : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Right_Angle_Quotation);

   type Lexer_Test_Class is abstract new kv.core.ut.Test_Class with
      record
         Lexer : kv.apg.lex.Lexer_Class;
      end record;

   ----------------------------------------------------------------------------
   procedure Ingest_All(T : in out Lexer_Test_Class'CLASS; S : in String) is
      WS : constant Wide_Wide_String := To_Wide_Wide_String(S);
   begin
      for WC of WS loop
         T.Lexer.Ingest_Character(WC);
      end loop;
   end Ingest_All;

   ----------------------------------------------------------------------------
   procedure Check_Tokens_Available(T : in out Lexer_Test_Class'CLASS; Expected : in Natural; S : in String) is
   begin
      T.Assert(T.Lexer.Tokens_Available = Expected, S & ": Should have " & Natural'IMAGE(Expected) & " tokens, got " & Natural'IMAGE(T.Lexer.Tokens_Available));
   end Check_Tokens_Available;

   ----------------------------------------------------------------------------
   type Initial_State_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Initial_State_Test) is
   begin
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Did not start between tokens");
      Check_Tokens_Available(T, 0, "Unused lexer");
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_One_Char_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_One_Char_Test) is
   begin
      T.Lexer.Ingest_Character(Pi);
      T.Assert(T.Lexer.Inbetween_Tokens = False, "Inter-token state error after one character seen");
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_One_Word_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_One_Word_Test) is
   begin
      T.Lexer.Ingest_Character(Pi);
      T.Lexer.Ingest_Character(Three);
      T.Assert(T.Lexer.Inbetween_Tokens = False, "Inter-token state error after two characters seen");
      T.Lexer.Ingest_Character(Space);
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Inter-token state error after compete word seen");
      Check_Tokens_Available(T, 1, "One word");
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_Two_Words_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_Two_Words_Test) is
   begin
      Ingest_All(T, "One two" & Ada.Characters.Latin_1.CR);
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be between tokens");
      T.Assert(T.Lexer.Tokens_Available = 2, "Two words should be available");
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_Comment_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_Comment_Test) is
   begin
      Ingest_All(T, "Alpha # comment");
      T.Assert(T.Lexer.Inbetween_Tokens = False, "Should be in a comment");
      Ingest_All(T, "...'' " & Ada.Characters.Latin_1.CR);
      Check_Tokens_Available(T, 2, "word+comment");
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_Char_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_Char_Test) is
   begin
      Ingest_All(T, "Alpha 'a'");
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after char literal");
      Check_Tokens_Available(T, 2, "word+char");
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_Symbol_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_Symbol_Test) is
   begin
      Ingest_All(T, " => ");
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after symbol");
      Check_Tokens_Available(T, 1, "one symbol");
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_String_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_String_Test) is
   begin
      Ingest_All(T, " ""embedded");
      T.Assert(T.Lexer.Inbetween_Tokens = False, "Should be in string");
      Ingest_All(T, " string"" ");
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after string");
      Check_Tokens_Available(T, 1, "one string");
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_A_Bunch_Of_Stuff_1_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_A_Bunch_Of_Stuff_1_Test) is
   begin
      Ingest_All(T, " word => ""embedded string"" + 'c'" & Ada.Characters.Latin_1.CR);
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after everything");
      Check_Tokens_Available(T, 5, "bunch of stuff 1");
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_Block_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_Block_Test) is
   begin
      T.Lexer.Ingest_Character(Open_Block);
      Ingest_All(T, " word => ""embedded string"" + 'c'" & Ada.Characters.Latin_1.CR);
      T.Lexer.Ingest_Character(Close_Block);
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after everything");
      Check_Tokens_Available(T, 1, "block");
   end Run;

   ----------------------------------------------------------------------------
   procedure register(suite : in kv.core.ut.Suite_Pointer_Type) is
   begin
      suite.register(new Initial_State_Test, "Initial_State_Test");
      suite.register(new Ingest_One_Char_Test, "Ingest_One_Char_Test");
      suite.register(new Ingest_One_Word_Test, "Ingest_One_Word_Test");
      suite.register(new Ingest_Two_Words_Test, "Ingest_Two_Words_Test");
      suite.register(new Ingest_Comment_Test, "Ingest_Comment_Test");
      suite.register(new Ingest_Char_Test, "Ingest_Char_Test");
      suite.register(new Ingest_Symbol_Test, "Ingest_Symbol_Test");
      suite.register(new Ingest_String_Test, "Ingest_String_Test");
      suite.register(new Ingest_A_Bunch_Of_Stuff_1_Test, "Ingest_A_Bunch_Of_Stuff_1_Test");
      suite.register(new Ingest_Block_Test, "Ingest_Block_Test");
   end register;

end kv.apg.tests;
