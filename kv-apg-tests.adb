with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Tags; use Ada.Tags;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Text_IO; use Ada.Text_IO;

with kv.apg.lex;
with kv.apg.tokens;
with kv.apg.parse;
with kv.apg.directives;
with kv.apg.regex;
with kv.core.wwstr;

package body kv.apg.tests is

   use Ada.Characters.Conversions;
   use kv.apg.lex;
   use kv.apg.tokens;
   use Ada.Strings.Wide_Wide_Unbounded;
   use kv.core.wwstr;

   Pi : constant Wide_Wide_Character := Wide_Wide_Character'VAL(16#03C0#);
   Three : constant Wide_Wide_Character := '3';
   Space : constant Wide_Wide_Character := ' ';
   Open_Block : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Left_Angle_Quotation);
   Close_Block : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Right_Angle_Quotation);

   ----------------------------------------------------------------------------
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
   type Simple_Token_Test is new kv.core.ut.Test_Class with null record;
   procedure Run(T : in out Simple_Token_Test) is
      Token : kv.apg.tokens.Token_Class;
      Word : constant String := "hello";
   begin
      Token.Initialize(A_Word, 1, To_String_Type(Word));
      T.Assert(Token.Get_Line = 1, "Line should be 1, is " & Positive'IMAGE(Token.Get_Line));
      T.Assert(Token.Get_Kind = A_Word, "Kind should be A_Word, is " & kv.apg.tokens.Token_Type'IMAGE(Token.Get_Kind));
      T.Assert(Token.Get_Data = To_String_Type(Word), "Data is wrong");
      T.Assert(Token.Get_Data_As_String = Word, "Data as string is wrong");
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
   type Parser_Test_Class is abstract new Lexer_Test_Class with
      record
         Parser : kv.apg.parse.Parser_Class;
      end record;

   ----------------------------------------------------------------------------
   procedure Parse_This(T : in out Parser_Test_Class'CLASS; S : in String) is
      Token : kv.apg.tokens.Token_Class;
   begin
      Ingest_All(T, S);
      while T.Lexer.Tokens_Available > 0 loop
         Token := T.Lexer.Get_Next_Token;
         T.Parser.Ingest_Token(Token);
      end loop;
   end Parse_This;

   ----------------------------------------------------------------------------
   procedure Check_States(T : in out Parser_Test_Class'CLASS; Errors : in Natural; Directives : in Natural) is
   begin
      T.Assert(T.Parser.Inbetween_Directives, "Should be between directives");
      T.Assert(T.Parser.Error_Count = Errors, "Error_Count: Should have " & Natural'IMAGE(Errors) & ", got " & Natural'IMAGE(T.Parser.Error_Count));
      T.Assert(T.Parser.Directive_Count = Directives, "Directive_Count: Should have " & Natural'IMAGE(Directives) & ", got " & Natural'IMAGE(T.Parser.Directive_Count));
   end Check_States;


   ----------------------------------------------------------------------------
   type Parse_Set_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Set_Test) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
      use kv.apg.directives;
   begin
      T.Parser.Initialise; --TODO: make this OBE
      Parse_This(T, "set lex_spec = ""test.ads"";" & Ada.Characters.Latin_1.LF);
      Check_States(T, Errors => 0, Directives => 1);
      Directive := T.Parser.Next_Directive;
      T.Assert(Directive.all'TAG = kv.apg.directives.Set_Class'TAG, "wrong class");
      T.Assert(Directive.Get_Name = +"lex_spec", "wrong name");
      kv.apg.directives.Free(Directive);
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Multi_Line_Set_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Multi_Line_Set_Test) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
      use kv.apg.directives;
      use Ada.Characters.Latin_1;
   begin
      Parse_This(T, "set #lex_spec = ""test.ads"";" & LF & "lex_spec #= ""test.ads"";" & LF & "= #""test.ads"";" & LF & """test.ads"";" & LF);
      Check_States(T, Errors => 0, Directives => 1);
      Directive := T.Parser.Next_Directive;
      T.Assert(Directive.all'TAG = kv.apg.directives.Set_Class'TAG, "wrong class");
      T.Assert(Directive.Get_Name = +"lex_spec", "wrong name");
      kv.apg.directives.Free(Directive);
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Set_Error_1_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Set_Error_1_Test) is
   begin
      Parse_This(T, "set lex_spec **** ""test.ads"";" & Ada.Characters.Latin_1.LF);
      Check_States(T, Errors => 1, Directives => 0);
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Set_Error_2_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Set_Error_2_Test) is
   begin
      Parse_This(T, "set ""lex_spec"" = ""test.ads"";" & Ada.Characters.Latin_1.LF);
      Check_States(T, Errors => 1, Directives => 0);
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Set_Error_3_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Set_Error_3_Test) is
   begin
      Parse_This(T, "set lex_spec = test;" & Ada.Characters.Latin_1.LF);
      Check_States(T, Errors => 1, Directives => 0);
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Block_Set_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Block_Set_Test) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
      use kv.apg.directives;
      use Ada.Strings.UTF_Encoding;
      use Ada.Strings.UTF_Encoding.Strings;
   begin
      T.Parser.Initialise; --TODO: make this OBE
      Parse_This(T, Decode("set lex_spec = «test.ads»;" & Ada.Characters.Latin_1.LF, UTF_8));
      Check_States(T, Errors => 0, Directives => 1);
      Directive := T.Parser.Next_Directive;
      T.Assert(Directive.all'TAG = kv.apg.directives.Set_Class'TAG, "wrong class");
      T.Assert(Directive.Get_Name = +"lex_spec", "wrong name");
      T.Assert(Set_Class'CLASS(Directive.all).Get_Value = +"test.ads", "wrong value");
      kv.apg.directives.Free(Directive);
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Set_Error_4_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Set_Error_4_Test) is
   begin
      Parse_This(T, "set lex_spec = ""test"" foo ;" & Ada.Characters.Latin_1.LF);
      Check_States(T, Errors => 1, Directives => 0);
   end Run;


   ----------------------------------------------------------------------------
   procedure Run_Token_Test
      (T          : in out Parser_Test_Class'CLASS;
       Name       : in String;
       Definition : in String;
       Expected   : in String) is

      Directive : kv.apg.directives.Directive_Pointer_Type;
      Tree_Image : String_Type;
      use kv.apg.directives;
      use kv.apg.regex; -- =

   begin
      Parse_This(T, "token " & name & " = " & Definition & Ada.Characters.Latin_1.LF);
      Check_States(T, Errors => 0, Directives => 1);
      Directive := T.Parser.Next_Directive;
      T.Assert(Directive.all'TAG = kv.apg.directives.Token_Class'TAG, "wrong class");
      T.Assert(Directive.Get_Name = To_String_Type(Name), "wrong name, got " & To_String(+Directive.Get_Name) & ", expected " & Name);
      T.Assert(not kv.apg.directives.Token_Class'CLASS(Directive.all).Get_Tree.Is_Empty, "tree shouldn't be null");
      Tree_Image := kv.apg.directives.Token_Class'CLASS(Directive.all).Get_Tree.Image_Tree;
      T.Assert(Tree_Image = To_String_Type(Expected), "wrong regex, got <" & To_String(+Tree_Image) & ">, expected <" & Expected & ">");
      kv.apg.directives.Free(Directive);
   end Run_Token_Test;


   ----------------------------------------------------------------------------
   type Parse_Basic_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Basic_Token_Test) is
   begin
      Run_Token_Test(T, "match", """foo"";", """foo""");
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Wild_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Wild_Token_Test) is
   begin
      Run_Token_Test(T, "any", ". ;", ".");
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Or_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Or_Token_Test) is
   begin
      Run_Token_Test(T, "Op_Mod", """%""|""mod"" ;", "(""%""|""mod"")");
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Or_Token_2_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Or_Token_2_Test) is
   begin
      Run_Token_Test(T, "abc", "'a' 'b' | 'c' ;", """a"" (""b""|""c"")");
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Cat_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Cat_Token_Test) is
   begin
      Run_Token_Test(T, "ab", "'a' 'b';", """a"" ""b""");
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Or_Token_3_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Or_Token_3_Test) is
   begin
      Run_Token_Test(T, "abcd", "'a' 'b' | 'c' | 'd' ;", """a"" ((""b""|""c"")|""d"")");
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Star_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Star_Token_Test) is
   begin
      Run_Token_Test(T, "ab", "'a' 'b' * ;", """a"" (""b"")*");
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Subsequence_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Subsequence_Token_Test) is
   begin
      Run_Token_Test(T, "abcbcbc", "'a' ('b' 'c') * ;", """a"" ((""b"" ""c""))*");
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Or_Subsequence_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Or_Subsequence_Token_Test) is
   begin
      Run_Token_Test(T, "bc", "'a' | ('b' 'c') ;", "(""a""|(""b"" ""c""))");
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Sub_Sub_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Sub_Sub_Token_Test) is
   begin
      Run_Token_Test(T, "subsub", "('c' ('d' 'e') ) ;", "(""c"" (""d"" ""e""))");
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Sub_Sub_Star_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Sub_Sub_Star_Token_Test) is
   begin
      Put_Line("----------");
      Run_Token_Test(T, "subsub", "('c' ('d' 'e') * ) ;", "(""c"" (""d"" ""e"")*)");
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Or_Sub_Sub_Star_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Or_Sub_Sub_Star_Token_Test) is
   begin
      Put_Line("----------");
      Run_Token_Test(T, "subsub", "('a' 'b' *) | ('c' ('d' 'e') *) ;", "((""a"" (""b"")*)|(""c"" (""d"" ""e"")*))");
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
      suite.register(new Ingest_Block_2_Test, "Ingest_Block_2_Test");
      suite.register(new Simple_Token_Test, "Simple_Token_Test");
      suite.register(new Lex_Tokens_Test, "Lex_Tokens_Test");
      suite.register(new Multi_Line_Test, "Multi_Line_Test");
      suite.register(new Parse_Set_Test, "Parse_Set_Test");
      suite.register(new Parse_Multi_Line_Set_Test, "Parse_Multi_Line_Set_Test");
      suite.register(new Parse_Set_Error_1_Test, "Parse_Set_Error_1_Test");
      suite.register(new Parse_Set_Error_2_Test, "Parse_Set_Error_2_Test");
      suite.register(new Parse_Set_Error_3_Test, "Parse_Set_Error_3_Test");
      suite.register(new Parse_Block_Set_Test, "Parse_Block_Set_Test");
      suite.register(new Parse_Set_Error_4_Test, "Parse_Set_Error_4_Test");
      suite.register(new Parse_Basic_Token_Test, "Parse_Basic_Token_Test");
      suite.register(new Parse_Wild_Token_Test, "Parse_Wild_Token_Test");
      suite.register(new Parse_Or_Token_Test, "Parse_Or_Token_Test");
      suite.register(new Parse_Or_Token_2_Test, "Parse_Or_Token_2_Test");
      suite.register(new Parse_Cat_Token_Test, "Parse_Cat_Token_Test");
      suite.register(new Parse_Or_Token_3_Test, "Parse_Or_Token_3_Test");
      suite.register(new Parse_Star_Token_Test, "Parse_Star_Token_Test");
      suite.register(new Parse_Subsequence_Token_Test, "Parse_Subsequence_Token_Test");
      suite.register(new Parse_Or_Subsequence_Token_Test, "Parse_Or_Subsequence_Token_Test");
      suite.register(new Parse_Sub_Sub_Token_Test, "Parse_Sub_Sub_Token_Test");
      suite.register(new Parse_Sub_Sub_Star_Token_Test, "Parse_Sub_Sub_Star_Token_Test");
      suite.register(new Parse_Or_Sub_Sub_Star_Token_Test, "Parse_Or_Sub_Sub_Star_Token_Test");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");
   end register;

end kv.apg.tests;
