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
with kv.apg.logger.writer;
with kv.apg.writer.buffer;
with kv.apg.incidents;

with kv.core.wwstr;

package body kv.apg.tests.lex_parse is

   use Ada.Characters.Conversions;
   use Ada.Strings.Wide_Wide_Unbounded;
   use kv.core.wwstr;
   use kv.apg.lex;
   use kv.apg.tokens;
   use kv.apg.tests.lex_lex;
   use kv.apg.logger.writer;
   use kv.apg.writer.buffer;
   use kv.apg.incidents;

   ----------------------------------------------------------------------------
   overriding procedure Tear_Down(T : in out Parser_Test_Class) is
   begin
      T.Parser.Delete_Directives;
   end Tear_Down;

   ----------------------------------------------------------------------------
   procedure Parse_This(T : in out Parser_Test_Class'CLASS; S : in String) is
      Token : kv.apg.tokens.Token_Class;
   begin
      Ingest_All(T, S & Ada.Characters.Latin_1.LF);
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
   procedure Test_Line
      (T      : in out kv.core.ut.Test_Class'CLASS;
       Buffer : in     Buffer_Writer_Class;
       Number : in     Positive;
       Line   : in     String) is
   begin
      if Buffer.Line_Count < Number then
         T.Fail("Too few lines in the buffer!");
      else
         T.Assert(Buffer.Get_Line(Number) = To_String_Type(Line), "Should be '"&Line&"', is '" & To_UTF(+Buffer.Get_Line(Number)) & "'");
      end if;
   end Test_Line;




   ----------------------------------------------------------------------------
   type Parse_Set_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Set_Test) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
      use kv.apg.directives;
   begin
      T.Parser.Initialise; --TODO: make this OBE
      Parse_This(T, "set lex_spec = ""test.ads"";");
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
      Parse_This(T, "set #lex_spec = ""test.ads"";" & LF & "lex_spec #= ""test.ads"";" & LF & "= #""test.ads"";" & LF & """test.ads"";");
      Check_States(T, Errors => 0, Directives => 1);
      Directive := T.Parser.Next_Directive;
      T.Assert(Directive.all'TAG = kv.apg.directives.Set_Class'TAG, "wrong class");
      T.Assert(Directive.Get_Name = +"lex_spec", "wrong name");
      kv.apg.directives.Free(Directive);
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Set_Error_1_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Set_Error_1_Test) is
      Buffer : aliased Buffer_Writer_Class;
      Logger : aliased Writer_Logger_Class;
      Expected : constant String := "ERROR (File: test.lex, line 1, column 14): Expected '=' (""****"").";
   begin
      T.Lexer.Start_File(To_String_Type("test.lex"));
      Logger.Initialize
         (Writer => Buffer'UNCHECKED_ACCESS,
          Level  => Error);
      T.Parser.Set_Logger(Logger'UNCHECKED_ACCESS);
      Parse_This(T, "set lex_spec **** ""test.ads"";");
      Check_States(T, Errors => 1, Directives => 0);
      T.Assert(Buffer.Line_Count > 0, "Expected an error log entry.");
      Test_Line(T, Buffer, 1, Expected);
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Set_Error_2_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Set_Error_2_Test) is
      Buffer : aliased Buffer_Writer_Class;
      Logger : aliased Writer_Logger_Class;
      Expected : constant String := "ERROR (File: test.lex, line 1, column 5): Expected an unquoted name (""lex_spec"").";
   begin
      T.Lexer.Start_File(To_String_Type("test.lex"));
      Logger.Initialize
         (Writer => Buffer'UNCHECKED_ACCESS,
          Level  => Error);
      T.Parser.Set_Logger(Logger'UNCHECKED_ACCESS);
      Parse_This(T, "set ""lex_spec"" = ""test.ads"";");
      Check_States(T, Errors => 1, Directives => 0);
      T.Assert(Buffer.Line_Count > 0, "Expected an error log entry.");
      Test_Line(T, Buffer, 1, Expected);
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Set_Error_3_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Set_Error_3_Test) is
      Buffer : aliased Buffer_Writer_Class;
      Logger : aliased Writer_Logger_Class;
      Expected : constant String := "ERROR (File: test.lex, line 1, column 16): Expected a quoted string or block (""test"").";
   begin
      T.Lexer.Start_File(To_String_Type("test.lex"));
      Logger.Initialize
         (Writer => Buffer'UNCHECKED_ACCESS,
          Level  => Error);
      T.Parser.Set_Logger(Logger'UNCHECKED_ACCESS);
      Parse_This(T, "set lex_spec = test;");
      Check_States(T, Errors => 1, Directives => 0);
      T.Assert(Buffer.Line_Count > 0, "Expected an error log entry.");
      Test_Line(T, Buffer, 1, Expected);
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
      Parse_This(T, Decode("set lex_spec = «test.ads»;", UTF_8));
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
      Buffer : aliased Buffer_Writer_Class;
      Logger : aliased Writer_Logger_Class;
      Expected : constant String := "ERROR (File: test.lex, line 1, column 23): Expected ';' (""foo"").";
   begin
      T.Lexer.Start_File(To_String_Type("test.lex"));
      Logger.Initialize
         (Writer => Buffer'UNCHECKED_ACCESS,
          Level  => Error);
      T.Parser.Set_Logger(Logger'UNCHECKED_ACCESS);
      Parse_This(T, "set lex_spec = ""test"" foo ;");
      Check_States(T, Errors => 1, Directives => 0);
      T.Assert(Buffer.Line_Count > 0, "Expected an error log entry.");
      Test_Line(T, Buffer, 1, Expected);
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
      --kv.apg.regex.Set_Debug(True);
      --Put_Line("------ token test: " & "token " & name & " = " & Definition);
      Parse_This(T, "token " & name & " = " & Definition);
      Check_States(T, Errors => 0, Directives => 1);
      Directive := T.Parser.Next_Directive;
      T.Assert(Directive.all'TAG = kv.apg.directives.Token_Class'TAG, "wrong class");
      T.Assert(Directive.Get_Name = To_String_Type(Name), "wrong name, got " & To_String(+Directive.Get_Name) & ", expected " & Name);
      T.Assert(not kv.apg.directives.Token_Class'CLASS(Directive.all).Get_Tree.Is_Empty, "tree shouldn't be null");
      Tree_Image := kv.apg.directives.Token_Class'CLASS(Directive.all).Get_Tree.Image_Tree;
      T.Assert(+Tree_Image = To_WWS(Expected), "wrong regex, got <" & To_UTF(+Tree_Image) & ">, expected <" & Expected & ">");
      kv.apg.directives.Free(Directive);
      --kv.apg.regex.Set_Debug(False);
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
   type Parse_Or_Token_Pi_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Or_Token_Pi_Test) is
   begin
      --kv.apg.regex.Set_Debug(True);
      Run_Token_Test(T, "pi", """Pi"" | U03C0 ;", "(""Pi""|""π"")");
      kv.apg.regex.Set_Debug(False);
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
      Run_Token_Test(T, "subsub", "('c' ('d' 'e') * ) ;", "(""c"" ((""d"" ""e""))*)");
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Or_Sub_Sub_Star_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Or_Sub_Sub_Star_Token_Test) is
   begin
      Run_Token_Test(T, "subsub", "('a' 'b' * ) | ('c' ('d' 'e') * ) ;", "((""a"" (""b"")*)|(""c"" ((""d"" ""e""))*))");
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Plus_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Plus_Token_Test) is
   begin
      Run_Token_Test(T, "ab", "'a' 'b' + ;", """a"" (""b"")+");
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Zoro_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Zoro_Token_Test) is
   begin
      Run_Token_Test(T, "a_and_maybe_b", "'a' 'b' ? ;", """a"" (""b"")?");
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Range_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Range_Token_Test) is
   begin
      --kv.apg.regex.Set_Debug(True);
      Run_Token_Test(T, "a_to_z", "'a'-'z';", "(U0061-U007A)");
      --kv.apg.regex.Set_Debug(False);
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Range_2_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Range_2_Token_Test) is
   begin
      --kv.apg.regex.Set_Debug(True);
      Run_Token_Test(T, "a_to_z", "U0061-U007A;", "(U0061-U007A)");
      --kv.apg.regex.Set_Debug(False);
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Pattern_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Pattern_Token_Test) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
      use kv.apg.directives;
   begin
      Parse_This(T, "pattern pat1 = '0'-'9' + ;");
      Check_States(T, Errors => 0, Directives => 1);
      Directive := T.Parser.Next_Directive;
      T.Assert(kv.apg.directives.Token_Class'CLASS(Directive.all).Get_Subtype = Pattern, "Expected subtype to be pattern");
      kv.apg.directives.Free(Directive);
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Skipover_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Skipover_Token_Test) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
      use kv.apg.directives;
   begin
      Parse_This(T, "skipover pat2 = (' ' | U0009) + ;");
      Check_States(T, Errors => 0, Directives => 1);
      Directive := T.Parser.Next_Directive;
      T.Assert(kv.apg.directives.Token_Class'CLASS(Directive.all).Get_Subtype = Skipover, "Expected subtype to be skipover");
      kv.apg.directives.Free(Directive);
   end Run;


   --<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   package Parse_Visitor_Test_Util is
      type Visitor_Class is new kv.apg.directives.Directive_Visitor_Class with
         record
            S : Natural := 0;
            T : Natural := 0;
         end record;
      overriding procedure Process_Set
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Set_Class'CLASS);

      overriding procedure Process_Token
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Token_Class'CLASS);
   end Parse_Visitor_Test_Util;

   package body Parse_Visitor_Test_Util is
      overriding procedure Process_Set
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Set_Class'CLASS) is
      begin
         Self.S := Self.S + 1;
      end Process_Set;

      overriding procedure Process_Token
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Token_Class'CLASS) is
      begin
         Self.T := Self.T + 1;
      end Process_Token;
   end Parse_Visitor_Test_Util;
   -->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

   ----------------------------------------------------------------------------
   type Parse_Visitor_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Visitor_Test) is
      V : Parse_Visitor_Test_Util.Visitor_Class;
      use kv.apg.directives;
   begin
      Parse_This(T, "set lex_spec = ""test.ads"";");
      Parse_This(T, "token foo = 'a' 'b' ?;");
      Parse_This(T, "pattern pat1 = '0'-'9' + ;");
      Parse_This(T, "skipover pat2 = (' ' | U0009) + ;");
      Check_States(T, Errors => 0, Directives => 4);

      T.Parser.Process_Directives(V);
      T.Assert(V.S = 1, "Expected one set directive visited!");
      T.Assert(V.T = 3, "Expected three token directives visited!");

      T.Parser.Delete_Directives;
      T.Assert(T.Parser.Directive_Count = 0, "Undeleted directives ramain!");
   end Run;


   ----------------------------------------------------------------------------
   type Parse_Invalid_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Invalid_Token_Test) is
      Buffer : aliased Buffer_Writer_Class;
      Logger : aliased Writer_Logger_Class;
      Expected : constant String := "ERROR (File: token.lex, line 1, column 1): Expected directive (""nerk"").";
   begin
      T.Lexer.Start_File(To_String_Type("token.lex"));
      Logger.Initialize
         (Writer => Buffer'UNCHECKED_ACCESS,
          Level  => Error);
      T.Parser.Set_Logger(Logger'UNCHECKED_ACCESS);
      Parse_This(T, "nerk pat2 = (' ' | U0009) + ;");
      Check_States(T, Errors => 1, Directives => 0);
      T.Assert(Buffer.Line_Count > 0, "Expected an error log entry.");
      Test_Line(T, Buffer, 1, Expected);
   end Run;

   ----------------------------------------------------------------------------
   procedure Run_Broken_Token_Test
      (T          : in out Parser_Test_Class'CLASS;
       Definition : in String;
       Expected   : in String) is

      Buffer : aliased Buffer_Writer_Class;
      Logger : aliased Writer_Logger_Class;

   begin
      T.Lexer.Start_File(To_String_Type("token.lex"));
      Logger.Initialize
         (Writer => Buffer'UNCHECKED_ACCESS,
          Level  => Error);
      T.Parser.Set_Logger(Logger'UNCHECKED_ACCESS);

      Parse_This(T, "token bad = " & Definition);
      Check_States(T, Errors => 1, Directives => 0);

      Test_Line(T, Buffer, 1, Expected);
   end Run_Broken_Token_Test;

   ----------------------------------------------------------------------------
   type Parse_Bad_Regex_1_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Bad_Regex_1_Token_Test) is
   begin
      --kv.apg.regex.Set_Debug(True);
      Run_Broken_Token_Test(T, "U0061-;", "ERROR (File: token.lex, line 1, column 19): Incomplete regular expression ("";"").");
      --kv.apg.regex.Set_Debug(False);
   end Run;

   ----------------------------------------------------------------------------
   type Parse_Bad_Regex_2_Token_Test is new Parser_Test_Class with null record;
   procedure Run(T : in out Parse_Bad_Regex_2_Token_Test) is
   begin
      -- This is a regular expression that seemed to work until beter error checking was added
      --kv.apg.regex.Set_Debug(True);
      Run_Broken_Token_Test(T, "(('a'-'z') | ('A'-'Z')) * ('_'? (('a'-'z')|('A'-'Z')|('0'-'9'))*;", "ERROR (File: token.lex, line 1, column 77): Incomplete regular expression ("";"").");
      --kv.apg.regex.Set_Debug(False);
   end Run;




   ----------------------------------------------------------------------------
   procedure register(suite : in kv.core.ut.Suite_Pointer_Type) is
   begin
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
      suite.register(new Parse_Or_Token_Pi_Test, "Parse_Or_Token_Pi_Test");
      suite.register(new Parse_Star_Token_Test, "Parse_Star_Token_Test");
      suite.register(new Parse_Subsequence_Token_Test, "Parse_Subsequence_Token_Test");
      suite.register(new Parse_Or_Subsequence_Token_Test, "Parse_Or_Subsequence_Token_Test");
      suite.register(new Parse_Sub_Sub_Token_Test, "Parse_Sub_Sub_Token_Test");
      suite.register(new Parse_Sub_Sub_Star_Token_Test, "Parse_Sub_Sub_Star_Token_Test");
      suite.register(new Parse_Or_Sub_Sub_Star_Token_Test, "Parse_Or_Sub_Sub_Star_Token_Test");
      suite.register(new Parse_Plus_Token_Test, "Parse_Plus_Token_Test");
      suite.register(new Parse_Zoro_Token_Test, "Parse_Zoro_Token_Test");
      suite.register(new Parse_Range_Token_Test, "Parse_Range_Token_Test");
      suite.register(new Parse_Range_2_Token_Test, "Parse_Range_2_Token_Test");
      suite.register(new Parse_Pattern_Token_Test, "Parse_Pattern_Token_Test");
      suite.register(new Parse_Skipover_Token_Test, "Parse_Skipover_Token_Test");
      suite.register(new Parse_Visitor_Test, "Parse_Visitor_Test");
      suite.register(new Parse_Invalid_Token_Test, "Parse_Invalid_Token_Test");
      suite.register(new Parse_Bad_Regex_1_Token_Test, "Parse_Bad_Regex_1_Token_Test");
      suite.register(new Parse_Bad_Regex_2_Token_Test, "Parse_Bad_Regex_2_Token_Test");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");
   end register;

end kv.apg.tests.lex_parse;
