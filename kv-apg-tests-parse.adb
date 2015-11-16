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
with kv.apg.enum;
with kv.apg.locations;

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
   begin
      Parse_This(T, "rule empty = | => «null;»;");
      Check_States(T, Errors => 0, Directives => 1);
      Directive := T.Parser.Next_Directive;
      T.Assert(Directive.all'TAG = kv.apg.directives.Rule_Class'TAG, "Expected directive to be Rule_Class");
      kv.apg.directives.Free(Directive);
   end Run;

   ----------------------------------------------------------------------------
   type Multi_Line_Rule_Test is new Rule_Test_Class with null record;
   procedure Run(T : in out Multi_Line_Rule_Test) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
      Rule : kv.apg.rules.Rule_Pointer;
      use kv.apg.directives;
      Expected_1 : constant String := "( import_list class_list eos_token ) => null;";
      Expected_2 : constant String := "( pragma_token name_token eos_token ) => jump;";
      Expected_3 : constant String := "( ) => pause;";
   begin
      Parse_This(T, "rule program = start");
      Parse_This(T, " | import_list class_list  eos_token => «null;»");
      Parse_This(T, " | pragma_token name_token eos_token => «jump;»");
      Parse_This(T, " | => «pause;»;");
      Check_States(T, Errors => 0, Directives => 1);
      Directive := T.Parser.Next_Directive;
      T.Assert(Directive.all'TAG = kv.apg.directives.Rule_Class'TAG, "Expected directive to be Rule_Class");
      Rule := kv.apg.directives.Rule_Class'CLASS(Directive.all).Get_Rule;
      T.Assert(Rule.Productions(1).Image = To_String_Type(Expected_1), "Expected '"&Expected_1&"', got '"&To_UTF(Rule.Productions(1).Image)&"'.");
      T.Assert(Rule.Productions(2).Image = To_String_Type(Expected_2), "Expected '"&Expected_2&"', got '"&To_UTF(Rule.Productions(2).Image)&"'.");
      T.Assert(Rule.Productions(3).Image = To_String_Type(Expected_3), "Expected '"&Expected_3&"', got '"&To_UTF(Rule.Productions(3).Image)&"'.");
      T.Assert(Rule.Is_Start, "Expected the rule to be flagged as the start rule.");
      kv.apg.directives.Free(Directive);
   end Run;





   type Grammar_Test_Class is abstract new Rule_Test_Class with
      record
         Grammar : aliased kv.apg.rules.Grammar_Class;
         Enum : aliased kv.apg.enum.Enumeration_Class;
      end record;


   ----------------------------------------------------------------------------
   -- TODO: this is a copy from another file; make just one copy
   function "+"(Name : String) return kv.apg.tokens.Token_Class is
      T : kv.apg.tokens.Token_Class;
      Here : kv.apg.locations.File_Location_Type;
   begin
      Here.Initialize(+"test", 1, 1);
      T.Initialize(kv.apg.tokens.A_Word, Here, kv.core.wwstr.To_String_Type(Name));
      return T;
   end "+";


   ----------------------------------------------------------------------------
   type Init_Gramar_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Init_Gramar_Test) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
      Rule : kv.apg.rules.Rule_Pointer;

      Expected_1 : constant String := "( Alpha Gamma ) => null;";
      Expected_2 : constant String := "( pragma_token name_token eos_token ) => jump;";
      Expected_3 : constant String := "( ) => pause;";

   begin
      T.Enum.Initialize(+"Enum_Type");
      T.Enum.Append(+"Alpha");
      T.Enum.Append(+"Beta");
      T.Enum.Append(+"Gamma");
      T.Grammar.Initialize(T.Enum);

      Parse_This(T, "rule program = start");
      Parse_This(T, " | import_list class_list Gamma => «null;»");
      Parse_This(T, " | class_list Gamma => «null;»");
      Parse_This(T, " | => «pause;»;");

      Parse_This(T, "rule import_list =");
      Parse_This(T, " | Alpha Gamma => «null;»");
      Parse_This(T, " ;");

      Parse_This(T, "rule class_list =");
      Parse_This(T, " | Beta Gamma => «null;»");
      Parse_This(T, " ;");

      Check_States(T, Errors => 0, Directives => 3);
      for X in 1..3 loop
         Directive := T.Parser.Next_Directive;
         T.Assert(Directive.all'TAG = kv.apg.directives.Rule_Class'TAG, "Expected directive to be Rule_Class");
         Rule := kv.apg.directives.Rule_Class'CLASS(Directive.all).Get_Rule;
         T.Grammar.Add_Rule(Rule);
      end loop;
      -- TODO: need more testing

      Rule := T.Grammar.Find(To_String_Type("import_list"));

      T.Assert(Rule.Productions(1).Image = To_String_Type(Expected_1), "Expected '"&Expected_1&"', got '"&To_UTF(Rule.Productions(1).Image)&"'.");
      T.Assert(not Rule.Is_Start, "Expected the rule to *NOT* be flagged as the start rule.");

--      T.Assert(Rule.Productions(2).Image = To_String_Type(Expected_2), "Expected '"&Expected_2&"', got '"&To_UTF(Rule.Productions(2).Image)&"'.");
--      T.Assert(Rule.Productions(3).Image = To_String_Type(Expected_3), "Expected '"&Expected_3&"', got '"&To_UTF(Rule.Productions(3).Image)&"'.");
   end Run;



   ----------------------------------------------------------------------------
   procedure register(suite : in kv.core.ut.Suite_Pointer_Type) is
   begin
      suite.register(new One_Line_Rule_Test, "One_Line_Rule_Test");
      suite.register(new Multi_Line_Rule_Test, "Multi_Line_Rule_Test");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");

      suite.register(new Init_Gramar_Test, "Init_Gramar_Test");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");
   end register;

end kv.apg.tests.parse;
