with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Tags; use Ada.Tags;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Text_IO; use Ada.Text_IO;

with kv.core.wwstr;

with kv.apg.tokens;
with kv.apg.locations;

package body kv.apg.tests.lex_lex is

   use Ada.Characters.Conversions;
   use Ada.Strings.Wide_Wide_Unbounded;

   use kv.core.wwstr;

   use kv.apg.lex;
   use kv.apg.tokens;

   Pi : constant Wide_Wide_Character := Wide_Wide_Character'VAL(16#03C0#);
   Three : constant Wide_Wide_Character := '3';
   Space : constant Wide_Wide_Character := ' ';
   Open_Block : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Left_Angle_Quotation);
   Close_Block : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Right_Angle_Quotation);


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
      Ingest_All(T, "One two_2" & Ada.Characters.Latin_1.LF);
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be between tokens");
      Check_Tokens_Available(T, 2, "One two_2");
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_Comment_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_Comment_Test) is
   begin
      Ingest_All(T, "Alpha # comment");
      T.Assert(T.Lexer.Inbetween_Tokens = False, "Should be in a comment");
      Ingest_All(T, "...'' " & Ada.Characters.Latin_1.LF);
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
   type Ingest_Symbol_2_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_Symbol_2_Test) is
   begin
      Ingest_All(T, " := => ");
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after symbol");
      Check_Tokens_Available(T, 2, "two 2-char symbols");
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_Symbol_3a_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_Symbol_3a_Test) is
   begin
      Ingest_All(T, "word; ");
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after symbol");
      Check_Tokens_Available(T, 2, "word+symbol");
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_Symbol_3b_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_Symbol_3b_Test) is
   begin
      Ingest_All(T, "word;"); -- ';' should be a mono-char symbol and always self-complete
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after symbol");
      Check_Tokens_Available(T, 2, "word+symbol");
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_Symbol_3c_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_Symbol_3c_Test) is
      Token : kv.apg.tokens.Token_Class;
   begin
      Ingest_All(T, "word-word ");
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after symbol");
      Check_Tokens_Available(T, 3, "word+symbol-word (no spaces)");

      Token := T.Lexer.Get_Next_Token;
      T.Assert(Token.Get_Kind = A_Word, "Kind should be A_Word, is " & kv.apg.tokens.Token_Type'IMAGE(Token.Get_Kind));
      T.Assert(Token.Get_Data_As_String = "word", "Word is wrong, got " & Token.Get_Data_As_String);

      Token := T.Lexer.Get_Next_Token;
      T.Assert(Token.Get_Kind = A_Symbol, "Kind should be A_Symbol, is " & kv.apg.tokens.Token_Type'IMAGE(Token.Get_Kind));
      T.Assert(Token.Get_Data_As_String = "-", "- is wrong, got " & Token.Get_Data_As_String);

      Token := T.Lexer.Get_Next_Token;
      T.Assert(Token.Get_Kind = A_Word, "Kind should be A_Word, is " & kv.apg.tokens.Token_Type'IMAGE(Token.Get_Kind));
      T.Assert(Token.Get_Data_As_String = "word", "second Word is wrong, got " & Token.Get_Data_As_String);
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_Symbol_4_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_Symbol_4_Test) is
      Token : kv.apg.tokens.Token_Class;
   begin
      --kv.apg.lex.Set_Debug(True);
      Ingest_All(T, "(*);");
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after symbol");
      Check_Tokens_Available(T, 4, "(*);");

      Token := T.Lexer.Get_Next_Token;
      T.Assert(Token.Get_Kind = A_Symbol, "Kind should be A_Symbol, is " & kv.apg.tokens.Token_Type'IMAGE(Token.Get_Kind));
      T.Assert(Token.Get_Data_As_String = "(", "( is wrong, got " & Token.Get_Data_As_String);

      Token := T.Lexer.Get_Next_Token;
      T.Assert(Token.Get_Kind = A_Symbol, "Kind should be A_Symbol, is " & kv.apg.tokens.Token_Type'IMAGE(Token.Get_Kind));
      T.Assert(Token.Get_Data_As_String = "*", "* is wrong, got " & Token.Get_Data_As_String);

      Token := T.Lexer.Get_Next_Token;
      T.Assert(Token.Get_Kind = A_Symbol, "Kind should be A_Symbol, is " & kv.apg.tokens.Token_Type'IMAGE(Token.Get_Kind));
      T.Assert(Token.Get_Data_As_String = ")", ") is wrong, got " & Token.Get_Data_As_String);

      Token := T.Lexer.Get_Next_Token;
      T.Assert(Token.Get_Kind = A_Symbol, "Kind should be A_Symbol, is " & kv.apg.tokens.Token_Type'IMAGE(Token.Get_Kind));
      T.Assert(Token.Get_Data_As_String = ";", "; is wrong, got " & Token.Get_Data_As_String);

      --kv.apg.lex.Set_Debug(False);
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_Symbol_5_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_Symbol_5_Test) is
   begin
      Ingest_All(T, "'*';");
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after symbol");
      Check_Tokens_Available(T, 2, "char+symbol");
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
      Ingest_All(T, " word => ""embedded string"" + 'c'" & Ada.Characters.Latin_1.LF);
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after everything");
      Check_Tokens_Available(T, 5, "bunch of stuff 1");
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_Block_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_Block_Test) is
   begin
      T.Lexer.Ingest_Character(Open_Block);
      Ingest_All(T, " word => ""embedded string"" + 'c'" & Ada.Characters.Latin_1.LF);
      T.Lexer.Ingest_Character(Close_Block);
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after everything");
      Check_Tokens_Available(T, 1, "block");
   end Run;

   ----------------------------------------------------------------------------
   type Ingest_Block_2_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Ingest_Block_2_Test) is
      use Ada.Strings.UTF_Encoding;
      use Ada.Strings.UTF_Encoding.Strings;
   begin
      Ingest_All(T, Decode("«test.ads»", UTF_8));
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after everything");
      Check_Tokens_Available(T, 1, "block");
   end Run;

   ----------------------------------------------------------------------------
   type Lex_Tokens_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Lex_Tokens_Test) is
      Token : kv.apg.tokens.Token_Class;
      Word : constant String := "hello";
      Symbol_1 : constant String := "=>";
      String_1 : constant String := "embedded string";
      Symbol_2 : constant String := "+";
      Char_1 : constant String := "c";
      Comment_1 : constant String := "done";
   begin
      --T.Log("Lex_Tokens_Test");
      Ingest_All(T, "hello => ""embedded string"" + 'c' #done" & Ada.Characters.Latin_1.LF);
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after everything");
      Check_Tokens_Available(T, 6, "word+symbol+string+symbol+char+comment");
      Token := T.Lexer.Get_Next_Token;
      Check_Tokens_Available(T, 5, "symbol+string+symbol+char+comment");
      T.Assert(Token.Get_Line = 1, "Line should be 1, is " & Positive'IMAGE(Token.Get_Line));
      T.Assert(Token.Get_Kind = A_Word, "Kind should be A_Word, is " & kv.apg.tokens.Token_Type'IMAGE(Token.Get_Kind));
      T.Assert(Token.Get_Data = To_String_Type(Word), "Word is wrong");

      Token := T.Lexer.Get_Next_Token;
      Check_Tokens_Available(T, 4, "string+symbol+char+comment");
      T.Assert(Token.Get_Line = 1, "Line should be 1, is " & Positive'IMAGE(Token.Get_Line));
      T.Assert(Token.Get_Kind = A_Symbol, "Kind should be A_Symbol, is " & kv.apg.tokens.Token_Type'IMAGE(Token.Get_Kind));
      T.Assert(Token.Get_Data = To_String_Type(Symbol_1), "Symbol_1 is wrong, expected '"&Symbol_1&"', got '"&To_String(+Token.Get_Data)&"'.");

      Token := T.Lexer.Get_Next_Token;
      Check_Tokens_Available(T, 3, "symbol+char+comment");
      T.Assert(Token.Get_Line = 1, "Line should be 1, is " & Positive'IMAGE(Token.Get_Line));
      T.Assert(Token.Get_Kind = A_String, "Kind should be A_String, is " & kv.apg.tokens.Token_Type'IMAGE(Token.Get_Kind));
      T.Assert(Token.Get_Data = To_String_Type(String_1), "String_1 is wrong, expected '"&String_1&"', got '"&To_String(+Token.Get_Data)&"'.");

      Token := T.Lexer.Get_Next_Token;
      Check_Tokens_Available(T, 2, "char+comment");
      T.Assert(Token.Get_Line = 1, "Line should be 1, is " & Positive'IMAGE(Token.Get_Line));
      T.Assert(Token.Get_Kind = A_Symbol, "Kind should be A_Symbol, is " & kv.apg.tokens.Token_Type'IMAGE(Token.Get_Kind));
      T.Assert(Token.Get_Data = To_String_Type(Symbol_2), "Symbol_2 is wrong, expected '"&Symbol_2&"', got '"&To_String(+Token.Get_Data)&"'.");

      Token := T.Lexer.Get_Next_Token;
      Check_Tokens_Available(T, 1, "comment");
      T.Assert(Token.Get_Line = 1, "Line should be 1, is " & Positive'IMAGE(Token.Get_Line));
      T.Assert(Token.Get_Kind = A_Char, "Kind should be A_Char, is " & kv.apg.tokens.Token_Type'IMAGE(Token.Get_Kind));
      T.Assert(Token.Get_Data = To_String_Type(Char_1), "Char_1 is wrong, expected '"&Char_1&"', got '"&To_String(+Token.Get_Data)&"'.");

      Token := T.Lexer.Get_Next_Token;
      Check_Tokens_Available(T, 0, "empty");
      T.Assert(Token.Get_Line = 1, "Line should be 1, is " & Positive'IMAGE(Token.Get_Line));
      T.Assert(Token.Get_Kind = A_Comment, "Kind should be A_Comment, is " & kv.apg.tokens.Token_Type'IMAGE(Token.Get_Kind));
      T.Assert(Token.Get_Data = To_String_Type(Comment_1), "Comment_1 is wrong, expected '"&Comment_1&"', got '"&To_String(+Token.Get_Data)&"'.");
   end Run;

   ----------------------------------------------------------------------------
   type Multi_Line_Test is new Lexer_Test_Class with null record;
   procedure Run(T : in out Multi_Line_Test) is
      Token : kv.apg.tokens.Token_Class;
   begin
      T.Lexer.Ingest_Character(Open_Block);
      Ingest_All(T, " word => ""embedded string"" + 'c'" & Ada.Characters.Latin_1.LF);
      T.Lexer.Ingest_Character(Close_Block);
      Ingest_All(T, "line_two #done" & Ada.Characters.Latin_1.LF);
      Ingest_All(T, "line_three!" & Ada.Characters.Latin_1.LF);
      T.Assert(T.Lexer.Inbetween_Tokens = True, "Should be after everything");
      Check_Tokens_Available(T, 5, "block+word+comment+word+symbol");

      Token := T.Lexer.Get_Next_Token;
      T.Assert(Token.Get_Line = 1, "Line should be 1, is " & Positive'IMAGE(Token.Get_Line));
      Token := T.Lexer.Get_Next_Token;
      T.Assert(Token.Get_Line = 2, "Line should be 2, is " & Positive'IMAGE(Token.Get_Line));
      Token := T.Lexer.Get_Next_Token;
      T.Assert(Token.Get_Line = 2, "Line should be 2, is " & Positive'IMAGE(Token.Get_Line));
      Token := T.Lexer.Get_Next_Token;
      T.Assert(Token.Get_Line = 3, "Line should be 3, is " & Positive'IMAGE(Token.Get_Line));
   end Run;


   ----------------------------------------------------------------------------
   type Simple_Token_Test is new kv.core.ut.Test_Class with null record;
   procedure Run(T : in out Simple_Token_Test) is
      Token : kv.apg.tokens.Token_Class;
      Word : constant String := "hello";
      Where : kv.apg.locations.File_Location_Type;
   begin
      Token.Initialize(A_Word, Where, To_String_Type(Word));
      T.Assert(Token.Get_Line = 1, "Line should be 1, is " & Positive'IMAGE(Token.Get_Line));
      T.Assert(Token.Get_Kind = A_Word, "Kind should be A_Word, is " & kv.apg.tokens.Token_Type'IMAGE(Token.Get_Kind));
      T.Assert(Token.Get_Data = To_String_Type(Word), "Data is wrong");
      T.Assert(Token.Get_Data_As_String = Word, "Data as string is wrong");
   end Run;

   ----------------------------------------------------------------------------
   type File_Loc_Token_Test is new kv.core.ut.Test_Class with null record;
   procedure Run(T : in out File_Loc_Token_Test) is
      Token : kv.apg.tokens.Token_Class;
      Word  : constant String := "hello";
      Where : kv.apg.locations.File_Location_Type;
      Expected_Location : constant String := "File: my_file.txt, line 5, column 3";
   begin
      Where.Initialize(To_String_Type("my_file.txt"), 5, 3);
      Token.Initialize(A_Word, Where, To_String_Type(Word));
      T.Assert(Token.Get_Line = 5, "Line should be 5, is " & Positive'IMAGE(Token.Get_Line));
      T.Assert(Token.Get_Location.Image = Expected_Location, "Location should be <"&Expected_Location&">, is " & Token.Get_Location.Image);
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
      suite.register(new Ingest_Symbol_2_Test, "Ingest_Symbol_2_Test");
      suite.register(new Ingest_Symbol_3a_Test, "Ingest_Symbol_3a_Test");
      suite.register(new Ingest_Symbol_3b_Test, "Ingest_Symbol_3b_Test");
      suite.register(new Ingest_Symbol_3c_Test, "Ingest_Symbol_3c_Test");
      suite.register(new Ingest_Symbol_4_Test, "Ingest_Symbol_4_Test");
      suite.register(new Ingest_Symbol_5_Test, "Ingest_Symbol_5_Test");
      suite.register(new Ingest_String_Test, "Ingest_String_Test");
      suite.register(new Ingest_A_Bunch_Of_Stuff_1_Test, "Ingest_A_Bunch_Of_Stuff_1_Test");
      suite.register(new Ingest_Block_Test, "Ingest_Block_Test");
      suite.register(new Ingest_Block_2_Test, "Ingest_Block_2_Test");
      suite.register(new Lex_Tokens_Test, "Lex_Tokens_Test");
      suite.register(new Multi_Line_Test, "Multi_Line_Test");
--      suite.register(new XXX, "XXX");
      suite.register(new Simple_Token_Test, "Simple_Token_Test");
      suite.register(new File_Loc_Token_Test, "File_Loc_Token_Test");
   end register;

end kv.apg.tests.lex_lex;
