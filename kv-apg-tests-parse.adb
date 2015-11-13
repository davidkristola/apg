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
with kv.apg.rules;

with kv.core.wwstr;

package body kv.apg.tests.parse is

   use Ada.Characters.Conversions;
   use Ada.Strings.Wide_Wide_Unbounded;
   use kv.core.wwstr;
   use kv.apg.lex;
   use kv.apg.tokens;
   use kv.apg.logger.writer;
   use kv.apg.writer.buffer;
   use kv.apg.incidents;

   use kv.apg.tests.lex_lex;
   use kv.apg.tests.lex_parse;

   ----------------------------------------------------------------------------
   procedure Test_Line
      (T      : in out Rule_Test_Class;
       Number : in     Positive;
       Line   : in     String) is
   begin
      if T.Buffer.Line_Count < Number then
         T.Fail("Too few lines in the buffer!");
      else
         T.Assert(T.Buffer.Get_Line(Number) = To_String_Type(Line), "Should be '"&Line&"', is '" & To_UTF(+T.Buffer.Get_Line(Number)) & "'");
      end if;
   end Test_Line;


   ----------------------------------------------------------------------------
   type One_Line_Rule_Test is new Rule_Test_Class with null record;
   procedure Run(T : in out One_Line_Rule_Test) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
      use kv.apg.directives;
      use Ada.Strings.UTF_Encoding;
      use Ada.Strings.UTF_Encoding.Strings;
   begin
      Parse_This(T, Decode("rule empty = | => «null;»;", UTF_8));
      Check_States(T, Errors => 0, Directives => 1);
      Directive := T.Parser.Next_Directive;
      T.Assert(Directive.all'TAG = kv.apg.directives.Rule_Class'TAG, "Expected directive to be Rule_Class");
      kv.apg.directives.Free(Directive);
   end Run;

   ----------------------------------------------------------------------------
   type Multi_Line_Rule_Test is new Rule_Test_Class with null record;
   procedure Run(T : in out Multi_Line_Rule_Test) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
      Rule : kv.apg.rules.Rule_Class;
      use kv.apg.directives;
      use Ada.Strings.UTF_Encoding;
      use Ada.Strings.UTF_Encoding.Strings;
      Expected_1 : constant String := "( import_list class_list eos_token ) => null;";
      Expected_2 : constant String := "( pragma_token name_token eos_token ) => jump;";
      Expected_3 : constant String := "(  ) => pause;";
   begin
      Parse_This(T, Decode("rule program = start", UTF_8));
      Parse_This(T, Decode(" | import_list class_list  eos_token => «null;»", UTF_8));
      Parse_This(T, Decode(" | pragma_token name_token eos_token => «jump;»", UTF_8));
      Parse_This(T, Decode(" | => «pause;»;", UTF_8));
      Check_States(T, Errors => 0, Directives => 1);
      Directive := T.Parser.Next_Directive;
      T.Assert(Directive.all'TAG = kv.apg.directives.Rule_Class'TAG, "Expected directive to be Rule_Class");
      Rule := kv.apg.directives.Rule_Class'CLASS(Directive.all).Get_Rule;
      T.Assert(Rule.Productions(1).Image = To_String_Type(Expected_1), "Expected '"&Expected_1&"', got '"&To_UTF(Rule.Productions(1).Image)&"'.");
      T.Assert(Rule.Productions(2).Image = To_String_Type(Expected_2), "Expected '"&Expected_2&"', got '"&To_UTF(Rule.Productions(2).Image)&"'.");
      T.Assert(Rule.Productions(3).Image = To_String_Type(Expected_3), "Expected '"&Expected_3&"', got '"&To_UTF(Rule.Productions(3).Image)&"'.");
      kv.apg.directives.Free(Directive);
   end Run;



   ----------------------------------------------------------------------------
   procedure register(suite : in kv.core.ut.Suite_Pointer_Type) is
   begin
      suite.register(new One_Line_Rule_Test, "One_Line_Rule_Test");
      suite.register(new Multi_Line_Rule_Test, "Multi_Line_Rule_Test");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");
   end register;

end kv.apg.tests.parse;
