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




   type Grammar_Test_Class is abstract new Rule_Test_Class with
      record
         Grammar : aliased kv.apg.rules.Grammar_Class;
         Enum : aliased kv.apg.enum.Enumeration_Class;
      end record;

   ----------------------------------------------------------------------------
   overriding procedure Set_Up(T : in out Grammar_Test_Class) is
   begin
      T.Logger.Initialize
         (Writer => T.Buffer'UNCHECKED_ACCESS,
          Level  => Error);
      T.Enum.Initialize(+"Enum_Type");
      T.Enum.Append(+"Alpha");
      T.Enum.Append(+"Beta");
      T.Enum.Append(+"Gamma");
      T.Grammar.Initialize(T.Enum);
   end Set_Up;


   type String_Array_Type is array (Positive range <>) of String_Type;

   ----------------------------------------------------------------------------
   procedure Run_Basic_Grammar_Test
      (T          : in out Grammar_Test_Class'CLASS;
       Rule_Count : in     Natural;
       Definition : in     String_Array_Type) is

      Directive : kv.apg.directives.Directive_Pointer_Type;
      Rule : kv.apg.rules.Rule_Pointer;

   begin
      for I in Definition'RANGE loop
         Parse_This(T, To_String(Definition(I)));
      end loop;

      Check_States(T, Errors => 0, Directives => Rule_Count);
      for X in 1..Rule_Count loop
         Directive := T.Parser.Next_Directive;
         T.Assert(Directive.all'TAG = kv.apg.directives.Rule_Class'TAG, "Expected directive to be Rule_Class");
         Rule := kv.apg.directives.Rule_Class'CLASS(Directive.all).Get_Rule;
         T.Grammar.Add_Rule(Rule);
      end loop;
   end Run_Basic_Grammar_Test;


   ----------------------------------------------------------------------------
   type Init_Gramar_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Init_Gramar_Test) is

      Rule : kv.apg.rules.Rule_Pointer;

      Expected_1 : constant String := "( Alpha Gamma ) => null;";
      Expected_2 : constant String := "( Beta Gamma ) => null;";
      Expected_3 : constant String := "( import_list class_list Gamma ) => null;";

   begin
      Run_Basic_Grammar_Test(T, 3,
         (01 => To_String_Type("rule program = start"),
          02 => To_String_Type(" | import_list class_list Gamma => «null;»"),
          03 => To_String_Type(" | class_list Gamma => «null;»"),
          04 => To_String_Type(" | => «pause;»;"),
          05 => To_String_Type("rule import_list ="),
          06 => To_String_Type(" | Alpha Gamma => «null;»"),
          07 => To_String_Type(" ;"),
          08 => To_String_Type("rule class_list ="),
          09 => To_String_Type(" | Beta Gamma => «null;»"),
          10 => To_String_Type(" ;")));

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("import_list"));
      T.Assert(Rule.Productions(1).Image = To_String_Type(Expected_1), "Expected '"&Expected_1&"', got '"&To_UTF(Rule.Productions(1).Image)&"'.");
      T.Assert(not Rule.Is_Start, "Expected the rule to *NOT* be flagged as the start rule.");

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("class_list"));
      T.Assert(Rule.Productions(1).Image = To_String_Type(Expected_2), "Expected '"&Expected_2&"', got '"&To_UTF(Rule.Productions(1).Image)&"'.");
      T.Assert(not Rule.Is_Start, "Expected the rule to *NOT* be flagged as the start rule.");

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("program"));
      T.Assert(Rule.Productions(1).Image = To_String_Type(Expected_3), "Expected '"&Expected_3&"', got '"&To_UTF(Rule.Productions(1).Image)&"'.");
      T.Assert(Rule.Is_Start, "Expected the rule to be flagged as the start rule.");
   end Run;

   ----------------------------------------------------------------------------
   type Resolve_Gramar_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Resolve_Gramar_Test) is
      Expected : constant String := "ERROR (File: , line 3, column 31): Element of production rule ""program"" not found. (""Epsilon"").";
   begin
      Run_Basic_Grammar_Test(T, 3,
         (01 => To_String_Type("rule program = start"),
          02 => To_String_Type(" | alpha_list Gamma beta_list => «null;»"),
          03 => To_String_Type(" | beta_list Gamma alpha_list Epsilon => «null;»"),
          04 => To_String_Type(" ;"),
          05 => To_String_Type("rule alpha_list ="),
          06 => To_String_Type(" | Alpha alpha_list => «null;»"),
          07 => To_String_Type(" | => «null;»"),
          08 => To_String_Type(" ;"),
          09 => To_String_Type("rule beta_list ="),
          10 => To_String_Type(" | Beta beta_list => «null;»"),
          11 => To_String_Type(" | => «null;»"),
          12 => To_String_Type(" ;")));

      T.Grammar.Resolve_Rules(T.Logger'UNCHECKED_ACCESS);
      T.Assert(T.Buffer.Line_Count > 0, "Expected an error log entry.");
      Test_Line(T, 1, Expected);
      T.Assert(T.Grammar.Get_Error_Count = 1, "Expected error count of 1.");
   end Run;


   ----------------------------------------------------------------------------
   procedure Set_Up_Start_Test
      (T  : in out Grammar_Test_Class'CLASS;
       S1 : in     String := "";
       S2 : in     String := "";
       S3 : in     String := "") is
   begin
      Run_Basic_Grammar_Test(T, 3,
         (01 => To_String_Type("rule program =" & S1),
          02 => To_String_Type(" | alpha_list Gamma beta_list Gamma => «null;»"),
          03 => To_String_Type(" ;"),
          04 => To_String_Type("rule alpha_list =" & S2),
          05 => To_String_Type(" | alpha_list Alpha => «null;»"),
          06 => To_String_Type(" | Alpha => «null;»"),
          07 => To_String_Type(" ;"),
          08 => To_String_Type("rule beta_list =" & S3),
          09 => To_String_Type(" | beta_list Beta => «null;»"),
          10 => To_String_Type(" | Beta => «null;»"),
          11 => To_String_Type(" ;")));
   end Set_Up_Start_Test;

   ----------------------------------------------------------------------------
   type No_Start_Gramar_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out No_Start_Gramar_Test) is
      Expected : constant String := "ERROR (File: , line 1, column 1): Expected one rule flagged as the start rule, found 0. (""start"").";
   begin
      Set_Up_Start_Test(T);

      T.Grammar.Resolve_Rules(T.Logger'UNCHECKED_ACCESS);
      T.Assert(T.Buffer.Line_Count = 0, "Expected no error log entries.");
      T.Assert(T.Grammar.Get_Error_Count = 0, "Expected no error counts (yet).");

      T.Grammar.Validate(T.Logger'UNCHECKED_ACCESS);
      Test_Line(T, 1, Expected);
      T.Assert(T.Grammar.Get_Error_Count = 1, "Expected error count of 1.");
   end Run;

   ----------------------------------------------------------------------------
   type Two_Start_Gramar_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Two_Start_Gramar_Test) is
      Expected : constant String := "ERROR (File: , line 1, column 1): Expected one rule flagged as the start rule, found 2. (""start"").";
   begin
      Set_Up_Start_Test(T, " start", "", " start");

      T.Grammar.Resolve_Rules(T.Logger'UNCHECKED_ACCESS);
      T.Assert(T.Buffer.Line_Count = 0, "Expected no error log entries.");
      T.Assert(T.Grammar.Get_Error_Count = 0, "Expected no error counts (yet).");

      T.Grammar.Validate(T.Logger'UNCHECKED_ACCESS);
      Test_Line(T, 1, Expected);
      T.Assert(T.Grammar.Get_Error_Count = 1, "Expected error count of 1.");
   end Run;

   ----------------------------------------------------------------------------
   procedure register(suite : in kv.core.ut.Suite_Pointer_Type) is
   begin
      suite.register(new One_Line_Rule_Test, "One_Line_Rule_Test");
      suite.register(new Multi_Line_Rule_Test, "Multi_Line_Rule_Test");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");

      suite.register(new Init_Gramar_Test, "Init_Gramar_Test");
      suite.register(new Resolve_Gramar_Test, "Resolve_Gramar_Test");
      suite.register(new No_Start_Gramar_Test, "No_Start_Gramar_Test");
      suite.register(new Two_Start_Gramar_Test, "Two_Start_Gramar_Test");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");
   end register;

end kv.apg.tests.parse;
