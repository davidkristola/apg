with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Tags; use Ada.Tags;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers;
with Ada.Exceptions; use Ada.Exceptions;

with kv.apg.lex;
with kv.apg.tokens;
with kv.apg.parse;
with kv.apg.directives;
with kv.apg.regex;
with kv.apg.logger.writer;
with kv.apg.writer.buffer;
with kv.apg.writer.console;
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
      T.Assert(Rule.Get_Production(1).Image = To_String_Type(Expected_1), "Expected '"&Expected_1&"', got '"&To_UTF(Rule.Get_Production(1).Image)&"'.");
      T.Assert(Rule.Get_Production(2).Image = To_String_Type(Expected_2), "Expected '"&Expected_2&"', got '"&To_UTF(Rule.Get_Production(2).Image)&"'.");
      T.Assert(Rule.Get_Production(3).Image = To_String_Type(Expected_3), "Expected '"&Expected_3&"', got '"&To_UTF(Rule.Get_Production(3).Image)&"'.");
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




   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
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
   end Set_Up;

   ----------------------------------------------------------------------------
   procedure Add_ABG_Enum(T : in out Grammar_Test_Class'CLASS) is
   begin
      T.Enum.Initialize(+"Enum_Type");
      T.Enum.Append(+"Alpha");
      T.Enum.Append(+"Beta");
      T.Enum.Append(+"Gamma");
      T.Grammar.Initialize(T.Enum);
   end Add_ABG_Enum;


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
      Add_ABG_Enum(T);
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
      T.Assert(Rule.Get_Production(1).Image = To_String_Type(Expected_1), "Expected '"&Expected_1&"', got '"&To_UTF(Rule.Get_Production(1).Image)&"'.");
      T.Assert(not Rule.Is_Start, "Expected the rule to *NOT* be flagged as the start rule.");

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("class_list"));
      T.Assert(Rule.Get_Production(1).Image = To_String_Type(Expected_2), "Expected '"&Expected_2&"', got '"&To_UTF(Rule.Get_Production(1).Image)&"'.");
      T.Assert(not Rule.Is_Start, "Expected the rule to *NOT* be flagged as the start rule.");

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("program"));
      T.Assert(Rule.Get_Production(1).Image = To_String_Type(Expected_3), "Expected '"&Expected_3&"', got '"&To_UTF(Rule.Get_Production(1).Image)&"'.");
      T.Assert(Rule.Is_Start, "Expected the rule to be flagged as the start rule.");
   end Run;

   ----------------------------------------------------------------------------
   type Resolve_Gramar_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Resolve_Gramar_Test) is
      Rule : kv.apg.rules.Rule_Pointer;
      Expected : constant String := "ERROR (File: , line 3, column 31): Symbol of production rule ""program"" not found. (""Epsilon"").";
      Expected_1 : constant String := "beta_list -> ( Beta beta_list ) => null;";
      use kv.apg.rules;
   begin
      Add_ABG_Enum(T);
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

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("program"));
      T.Assert(Rule.Get_Production(1).Get_Rule = Rule, "Expected a resolved production to point to its rule.");
      T.Assert(not Rule.Get_Production(1).Matches_An_Empty_Sequence, "Expected production to not match an empty sequence.");

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("alpha_list"));
      T.Assert(Rule.Get_Production(2).Matches_An_Empty_Sequence, "Expected production to match an empty sequence.");

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("beta_list"));
      T.Assert(Rule.Get_Production(1).Image = To_String_Type(Expected_1), "Expected '"&Expected_1&"', got '"&To_UTF(Rule.Get_Production(1).Image)&"'.");
   end Run;


   ----------------------------------------------------------------------------
   procedure Set_Up_Start_Test
      (T  : in out Grammar_Test_Class'CLASS;
       S1 : in     String := "";
       S2 : in     String := "";
       S3 : in     String := "") is
   begin
      Add_ABG_Enum(T);
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
      Expected : constant String := "ERROR: Expected one rule flagged as the start rule, found 0.";
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
      Expected : constant String := "ERROR: Expected one rule flagged as the start rule, found 2.";
   begin
      Set_Up_Start_Test(T, " start", "", " start");

      T.Grammar.Resolve_Rules(T.Logger'UNCHECKED_ACCESS);
      T.Assert(T.Buffer.Line_Count = 0, "Expected no error log entries.");
      T.Assert(T.Grammar.Get_Error_Count = 0, "Expected no error counts (yet).");

      T.Grammar.Validate(T.Logger'UNCHECKED_ACCESS);
      Test_Line(T, 1, Expected);
      T.Assert(T.Grammar.Get_Error_Count = 1, "Expected error count of 1, got " & Natural'IMAGE(T.Grammar.Get_Error_Count));
   end Run;



   ----------------------------------------------------------------------------
   procedure Add_ETF_Enum(T : in out Grammar_Test_Class'CLASS) is
   begin
      T.Enum.Initialize(+"Enum_Type");
      T.Enum.Append(+"plus");        -- 1
      T.Enum.Append(+"times");       -- 2
      T.Enum.Append(+"id");          -- 3
      T.Enum.Append(+"open_paren");  -- 4
      T.Enum.Append(+"close_paren"); -- 5
      T.Enum.Append(+"eof");
      T.Grammar.Initialize(T.Enum);
   end Add_ETF_Enum;

   Terminal_plus : constant := 1;
   Terminal_times : constant := 2;
   Terminal_id : constant := 3;
   Terminal_open_paren : constant := 4;
   Termianl_close_paren : constant := 5;

   ----------------------------------------------------------------------------
   -- Grammar 4.19 from the Dragon Book (page 222)
   procedure Set_Up_ETF_Grammar
      (T : in out Grammar_Test_Class'CLASS) is
   begin
      Add_ETF_Enum(T);
      Run_Basic_Grammar_Test(T, 3,
         (01 => To_String_Type("rule E = start"),
          02 => To_String_Type(" | E plus T => «null;»"),
          03 => To_String_Type(" | T => «null;»"),
          04 => To_String_Type(" ;"),
          05 => To_String_Type("rule T ="),
          06 => To_String_Type(" | T times F => «null;»"),
          07 => To_String_Type(" | F => «null;»"),
          08 => To_String_Type(" ;"),
          09 => To_String_Type("rule F ="),
          10 => To_String_Type(" | open_paren E close_paren => «null;»"),
          11 => To_String_Type(" | id => «null;»"),
          12 => To_String_Type(" ;")));
   end Set_Up_ETF_Grammar;


   ----------------------------------------------------------------------------
   type Grammar_Prime_1_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Grammar_Prime_1_Test) is
      CW : aliased kv.apg.writer.console.Console_Writer_Class;
      CL : aliased kv.apg.logger.writer.Writer_Logger_Class;
      Add_EOF : constant Boolean := True;
      Logger : kv.apg.logger.Safe_Logger_Pointer := T.Logger'UNCHECKED_ACCESS;
   begin
      CL.Initialize
         (Writer => CW'UNCHECKED_ACCESS,
          Level  => Debug);
      --Logger := CL'UNCHECKED_ACCESS;
      Set_Up_ETF_Grammar(T);
      T.Grammar.Add_Meta_Rule(Logger); -- Convert from G to G'
      T.Grammar.Resolve_Rules(Logger);
      T.Grammar.Resolve_Productions(Logger);
      T.Grammar.Resolve_Firsts(Logger);
      T.Grammar.Resolve_Follows(not Add_EOF, Logger);
      T.Grammar.Validate(Logger);
      T.Assert(T.Grammar.Get_Error_Count = 0, "Expected no Resolve/Validate error counts for ETF.");
   end Run;


   ----------------------------------------------------------------------------
   type Can_Disappear_1_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Can_Disappear_1_Test) is
      Ep : kv.apg.rules.Constant_Symbol_Pointer;
   begin
      Set_Up_ETF_Grammar(T);
      T.Grammar.Resolve_Rules(T.Logger'UNCHECKED_ACCESS);
      T.Grammar.Resolve_Productions(T.Logger'UNCHECKED_ACCESS);
      T.Grammar.Validate(T.Logger'UNCHECKED_ACCESS);
      T.Assert(T.Grammar.Get_Error_Count = 0, "Expected no Resolve/Validate error counts for ETF.");
      T.Assert(T.Grammar.Production_Count(To_String_Type("E")) = 2, "Expected E to have 2 productions, got " & Positive'IMAGE(T.Grammar.Production_Count(To_String_Type("E"))));
      T.Assert(T.Grammar.Symbol_Count(To_String_Type("E"), 1) = 3, "Expected E production 1 to have 3 elements, got " & Positive'IMAGE(T.Grammar.Symbol_Count(To_String_Type("E"), 1)));

      Ep := T.Grammar.Get_Symbol(To_String_Type("F"), 1, 1);
      T.Assert(Ep.Name = To_String_Type("open_paren"), "Expected Ep.Name to be open_paren, got " & To_UTF(Ep.Name));
      T.Assert(not Ep.Can_Disappear, "Expected not Ep.Can_Disappear");

      Ep := T.Grammar.Get_Symbol(To_String_Type("F"), 2, 1);
      T.Assert(Ep.Name = To_String_Type("id"), "Expected Ep.Name to be id, got " & To_UTF(Ep.Name));
      T.Assert(not Ep.Can_Disappear, "Expected not Ep.Can_Disappear"); -- Not a stressful test
      T.Assert(Ep.Is_Terminal, "Expected id to be a terminal element.");

      Ep := T.Grammar.Get_Symbol(To_String_Type("T"), 1, 3);
      T.Assert(Ep.Name = To_String_Type("F"), "Expected Ep.Name to be F, got " & To_UTF(Ep.Name));
      T.Assert(not Ep.Can_Disappear, "Expected not Ep.Can_Disappear");
      T.Assert(not Ep.Is_Terminal, "Expected F to be a nonterminal element.");

      Ep := T.Grammar.Get_Symbol(To_String_Type("E"), 1, 3);
      T.Assert(Ep.Name = To_String_Type("T"), "Expected Ep.Name to be T, got " & To_UTF(Ep.Name));
      T.Assert(not Ep.Can_Disappear, "Expected not Ep.Can_Disappear");
      T.Assert(not Ep.Is_Terminal, "Expected T to be a nonterminal element.");
      T.Assert(Ep.Is_Same_As(T.Grammar.Get_Symbol(To_String_Type("E"), 2, 1).all), "Expected two different T's to be 'Is_Same_As'.");
   end Run;

   ----------------------------------------------------------------------------
   procedure Set_Up_ABG_Grammar
      (T : in out Grammar_Test_Class'CLASS) is
      Add_EOF : constant Boolean := True;
   begin
      Add_ABG_Enum(T);
      Run_Basic_Grammar_Test(T, 4,
         (01 => To_String_Type("rule program = start"),
          02 => To_String_Type(" | alpha_list Gamma => «null;»"),
          03 => To_String_Type(" ;"),
          04 => To_String_Type("rule alpha_list ="),
          05 => To_String_Type(" | alpha_list Alpha => «null;»"),
          06 => To_String_Type(" | beta_list => «null;»"),
          07 => To_String_Type(" ;"),
          08 => To_String_Type("rule beta_list ="),
          09 => To_String_Type(" | beta_list Beta => «null;»"),
          10 => To_String_Type(" | gamma_list => «null;»"),
          11 => To_String_Type(" ;"),
          12 => To_String_Type("rule gamma_list ="),
          13 => To_String_Type(" | gamma_list Gamma => «null;»"),
          14 => To_String_Type(" | => «null;»"),
          15 => To_String_Type(" ;")
          ));
      T.Grammar.Resolve_Rules(T.Logger'UNCHECKED_ACCESS);
      T.Grammar.Resolve_Productions(T.Logger'UNCHECKED_ACCESS);
      T.Grammar.Resolve_Firsts(T.Logger'UNCHECKED_ACCESS);
      T.Grammar.Resolve_Follows(Add_EOF, T.Logger'UNCHECKED_ACCESS);
      T.Grammar.Validate(T.Logger'UNCHECKED_ACCESS);
      T.Assert(T.Grammar.Get_Error_Count = 0, "Expected no resolve/validate errors.");
   end Set_Up_ABG_Grammar;

   ----------------------------------------------------------------------------
   type Can_Disappear_2_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Can_Disappear_2_Test) is
      Rule : kv.apg.rules.Rule_Pointer;
      Sp : kv.apg.rules.Constant_Symbol_Pointer;
      Pp : kv.apg.rules.Production_Pointer;
      use kv.apg.rules;
   begin
      Set_Up_ABG_Grammar(T);

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("program"));
      T.Assert(Rule.Production_Count = 1, "Expected 1 production for rule 'program'");
      T.Assert(not Rule.Has_An_Empty_Sequence, "Rule 'program' should not have an empty sequence");
      T.Assert(not Rule.Can_Disappear, "Rule 'program' should not be able to disappear");
      Pp := Rule.Get_Production(1);
      T.Assert(not Pp.Can_Disappear, "Rule 'program' only production should not be able to disappear");
      T.Assert(not Pp.Matches_An_Empty_Sequence, "Rule 'program' only production should not match an empty sequence");
      T.Assert(Pp.Has_A_Terminal, "Rule 'program' only production should have a terminal");
      T.Assert(Pp.Get_Number = 5, "Expected production number to be 5, got" & Production_Index_Type'IMAGE(Pp.Get_Number));

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("alpha_list"));
      T.Assert(Rule.Production_Count = 2, "Expected 2 productions for rule 'alpha_list'");
      T.Assert(not Rule.Has_An_Empty_Sequence, "Rule 'alpha_list' should not have an empty sequence");
      T.Assert(Rule.Can_Disappear, "Rule 'alpha_list' should be able to disappear");
      Pp := Rule.Get_Production(1);
      T.Assert(not Pp.Can_Disappear, "Rule 'alpha_list' production 1 should not be able to disappear");
      T.Assert(not Pp.Matches_An_Empty_Sequence, "Rule 'alpha_list' production 1 should not match an empty sequence");
      T.Assert(Pp.Has_A_Terminal, "Rule 'alpha_list' production 1 should have a terminal");
      T.Assert(Pp.Get_Number = 1, "Expected production number to be 1, got" & Production_Index_Type'IMAGE(Pp.Get_Number));
      Pp := Rule.Get_Production(2);
      T.Assert(Pp.Can_Disappear, "Rule 'alpha_list' production 2 should be able to disappear");
      T.Assert(not Pp.Matches_An_Empty_Sequence, "Rule 'alpha_list' production 2 should not match an empty sequence");
      T.Assert(not Pp.Has_A_Terminal, "Rule 'alpha_list' production 2 should not have a terminal");
      T.Assert(Pp.Get_Number = 2, "Expected production number to be 2, got" & Production_Index_Type'IMAGE(Pp.Get_Number));

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("beta_list"));
      T.Assert(Rule.Production_Count = 2, "Expected 2 productions for rule 'beta_list'");
      T.Assert(not Rule.Has_An_Empty_Sequence, "Rule 'beta_list' should not have an empty sequence");
      T.Assert(Rule.Can_Disappear, "Rule 'beta_list' should be able to disappear");
      Pp := Rule.Get_Production(1);
      T.Assert(not Pp.Can_Disappear, "Rule 'beta_list' production 1 should not be able to disappear");
      T.Assert(not Pp.Matches_An_Empty_Sequence, "Rule 'beta_list' production 1 should not match an empty sequence");
      T.Assert(Pp.Has_A_Terminal, "Rule 'beta_list' production 1 should have a terminal");
      T.Assert(Pp.Get_Number = 6, "Expected production number to be 6, got" & Production_Index_Type'IMAGE(Pp.Get_Number));
      Pp := Rule.Get_Production(2);
      T.Assert(Pp.Can_Disappear, "Rule 'beta_list' production 2 should be able to disappear");
      T.Assert(not Pp.Matches_An_Empty_Sequence, "Rule 'beta_list' production 2 should not match an empty sequence");
      T.Assert(not Pp.Has_A_Terminal, "Rule 'beta_list' production 2 should not have a terminal");
      T.Assert(Pp.Get_Number = 7, "Expected production number to be 7, got" & Production_Index_Type'IMAGE(Pp.Get_Number));

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("gamma_list"));
      T.Assert(Rule.Production_Count = 2, "Expected 2 productions for rule 'gamma_list'");
      T.Assert(Rule.Has_An_Empty_Sequence, "Rule 'gamma_list' should have an empty sequence");
      T.Assert(Rule.Can_Disappear, "Rule 'gamma_list' should be able to disappear");
      Pp := Rule.Get_Production(1);
      T.Assert(not Pp.Can_Disappear, "Rule 'gamma_list' production 1 should not be able to disappear");
      T.Assert(not Pp.Matches_An_Empty_Sequence, "Rule 'gamma_list' production 1 should not match an empty sequence");
      T.Assert(Pp.Has_A_Terminal, "Rule 'gamma_list' production 1 should have a terminal");
      T.Assert(Pp.Get_Number = 3, "Expected production number to be 3, got" & Production_Index_Type'IMAGE(Pp.Get_Number));
      Pp := Rule.Get_Production(2);
      T.Assert(Pp.Can_Disappear, "Rule 'gamma_list' production 2 should be able to disappear");
      T.Assert(Pp.Matches_An_Empty_Sequence, "Rule 'gamma_list' production 2 should match an empty sequence");
      T.Assert(not Pp.Has_A_Terminal, "Rule 'gamma_list' production 2 should not have a terminal");
      T.Assert(Pp.Get_Number = 4, "Expected production number to be 4, got" & Production_Index_Type'IMAGE(Pp.Get_Number));

      Sp := T.Grammar.Get_Symbol(To_String_Type("program"), 1, 1);
      T.Assert(Sp.Name = To_String_Type("alpha_list"), "Expected Ep.Name to be alpha_list, got " & To_UTF(Sp.Name));
      T.Assert(not Sp.Is_Terminal, "Expected T to be a nonterminal element.");
      T.Assert(Sp.Can_Disappear, "Expected alpha_list Ep.Can_Disappear");
      T.Assert(Sp.Is_Same_As(T.Grammar.Get_Symbol(To_String_Type("alpha_list"), 1, 1).all), "Expected two different alpha_list's to be 'Is_Same_As'.");
   end Run;

   ----------------------------------------------------------------------------
   type Can_Disappear_3_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Can_Disappear_3_Test) is
      Rule : kv.apg.rules.Rule_Pointer;
      Pp : kv.apg.rules.Production_Pointer;
      use kv.apg.rules;
   begin
      Add_ABG_Enum(T);
      Run_Basic_Grammar_Test(T, 4,
         (01 => To_String_Type("rule program = start"),
          02 => To_String_Type(" | alpha_list Gamma => «null;»"),
          03 => To_String_Type(" ;"),
          04 => To_String_Type("rule alpha_list ="),
          05 => To_String_Type(" | alpha_list Alpha => «null;»"),
          06 => To_String_Type(" | beta_list => «null;»"),
          07 => To_String_Type(" ;"),
          08 => To_String_Type("rule beta_list ="),
          09 => To_String_Type(" | beta_list Beta => «null;»"),
          10 => To_String_Type(" | gamma_list => «null;»"),
          11 => To_String_Type(" ;"),
          12 => To_String_Type("rule gamma_list ="),
          13 => To_String_Type(" | beta_list => «null;»"),
          14 => To_String_Type(" ;")
          ));
      T.Grammar.Resolve_Rules(T.Logger'UNCHECKED_ACCESS);
      T.Grammar.Resolve_Productions(T.Logger'UNCHECKED_ACCESS);
      T.Grammar.Validate(T.Logger'UNCHECKED_ACCESS);

      T.Assert(T.Grammar.Get_Error_Count = 3, "Expected 3 resolve/validate errors got" & Natural'IMAGE(T.Grammar.Get_Error_Count));
      T.Assert(T.Buffer.Line_Count = 3, "Expected 3 error messages, got " & Natural'IMAGE(T.Buffer.Line_Count));

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("gamma_list"));
      Pp := Rule.Get_Production(1);
      T.Assert(not Pp.Can_Disappear, "Rule 'gamma_list' production 1 should not be able to disappear");
   end Run;


   ----------------------------------------------------------------------------
   -- Grammar 4.11 from the Dragon Book (page 189)
   procedure Set_Up_Alternate_ETF_Grammar
      (T : in out Grammar_Test_Class'CLASS) is
      Add_EOF : constant Boolean := True;
   begin
      Add_ETF_Enum(T);
      Run_Basic_Grammar_Test(T, 5,
         (01 => To_String_Type("rule E = start"),
          02 => To_String_Type(" | T E2 => «null;»"),
          03 => To_String_Type(" ;"),
          04 => To_String_Type("rule E2 ="),
          05 => To_String_Type(" | plus T E2 => «null;»"),
          06 => To_String_Type(" | => «null;»"),
          07 => To_String_Type(" ;"),
          08 => To_String_Type("rule T ="),
          09 => To_String_Type(" | F T2 => «null;»"),
          10 => To_String_Type(" ;"),
          11 => To_String_Type("rule T2 ="),
          12 => To_String_Type(" | times F T2 => «null;»"),
          13 => To_String_Type(" | => «null;»"),
          14 => To_String_Type(" ;"),
          15 => To_String_Type("rule F ="),
          16 => To_String_Type(" | open_paren E close_paren => «null;»"),
          17 => To_String_Type(" | id => «null;»"),
          18 => To_String_Type(" ;")));
      T.Grammar.Resolve_Rules(T.Logger'UNCHECKED_ACCESS);
      T.Grammar.Resolve_Productions(T.Logger'UNCHECKED_ACCESS);
      T.Grammar.Resolve_Firsts(T.Logger'UNCHECKED_ACCESS);
      T.Grammar.Resolve_Follows(Add_EOF, T.Logger'UNCHECKED_ACCESS);
      T.Grammar.Validate(T.Logger'UNCHECKED_ACCESS);
      T.Assert(T.Grammar.Get_Error_Count = 0, "Expected 0 resolve/validate errors got" & Natural'IMAGE(T.Grammar.Get_Error_Count));
   end Set_Up_Alternate_ETF_Grammar;


   type Terminal_Tuple_Type is
      record
         V : kv.apg.rules.Terminal_Index_Type;
         N : String_Type;
      end record;

   type Terminal_Tuple_Array is array (Positive range <>) of Terminal_Tuple_Type;

   ----------------------------------------------------------------------------
   procedure Test_First
      (T         : in out Grammar_Test_Class'CLASS;
       Rule_Name : in     String;
       Included  : in     Terminal_Tuple_Array) is

      use kv.apg.rules;
      use kv.apg.rules.Terminal_Sets;
      use Ada.Containers;

      Rule : kv.apg.rules.Rule_Pointer;
      Answer : Set;

   begin
      Rule := T.Grammar.Find_Non_Terminal(To_String_Type(Rule_Name));
      Answer := Rule.First;
      T.Assert(Answer.Length = Included'LENGTH, "Expected "&Positive'IMAGE(Included'LENGTH)&" terminals in the First of "&Rule_Name&", got" & Count_Type'IMAGE(Length(Answer)));
      for TT of Included loop
         T.Assert(Answer.Contains(TT.V), "Expected First of "&Rule_Name&" to contain terminal " & To_String(TT.N));
      end loop;
   end Test_First;

   Open_Tuple : constant Terminal_Tuple_Type := (Terminal_open_paren, To_String_Type("open_paren"));
   Id_Tuple : constant Terminal_Tuple_Type := (Terminal_id, To_String_Type("id"));
   Times_Tuple : constant Terminal_Tuple_Type := (Terminal_times, To_String_Type("times"));
   Plus_Tuple : constant Terminal_Tuple_Type := (Terminal_plus, To_String_Type("plus"));
   Epsilon_Tuple : constant Terminal_Tuple_Type := (kv.apg.rules.Epsilon, To_String_Type("Epsilon"));

   ----------------------------------------------------------------------------
   type First_1_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out First_1_Test) is
   begin
      Set_Up_Alternate_ETF_Grammar(T);

      Test_First(T, "E", (1 => Open_Tuple, 2 => Id_Tuple));
      Test_First(T, "T", (1 => Open_Tuple, 2 => Id_Tuple));
      Test_First(T, "F", (1 => Open_Tuple, 2 => Id_Tuple));
      Test_First(T, "E2", (1 => Plus_Tuple, 2 => Epsilon_Tuple));
      Test_First(T, "T2", (1 => Times_Tuple, 2 => Epsilon_Tuple));
   end Run;

   Alpha_Tuple : constant Terminal_Tuple_Type := (1, To_String_Type("Alpha"));
   Beta_Tuple : constant Terminal_Tuple_Type := (2, To_String_Type("Beta"));
   Gamma_Tuple : constant Terminal_Tuple_Type := (3, To_String_Type("Gamma"));

   ----------------------------------------------------------------------------
   type First_2_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out First_2_Test) is
   begin
      Set_Up_ABG_Grammar(T);

      Test_First(T, "program", (1 => Alpha_Tuple, 2 => Beta_Tuple, 3 => Gamma_Tuple));
      Test_First(T, "beta_list", (1 => Epsilon_Tuple, 2 => Beta_Tuple, 3 => Gamma_Tuple));
      Test_First(T, "gamma_list", (1 => Epsilon_Tuple, 2 => Gamma_Tuple));
   end Run;

   ----------------------------------------------------------------------------
   procedure Test_Follow
      (T         : in out Grammar_Test_Class'CLASS;
       Rule_Name : in     String;
       Included  : in     Terminal_Tuple_Array) is

      use kv.apg.rules;
      use kv.apg.rules.Terminal_Sets;
      use Ada.Containers;

      Rule : kv.apg.rules.Rule_Pointer;
      Answer : Set;

   begin
      Rule := T.Grammar.Find_Non_Terminal(To_String_Type(Rule_Name));
      Answer := Rule.Follow;
      T.Assert(Answer.Length = Included'LENGTH, "Expected "&Positive'IMAGE(Included'LENGTH)&" terminals in the Follow of "&Rule_Name&", got" & Count_Type'IMAGE(Length(Answer)));
      for TT of Included loop
         T.Assert(Answer.Contains(TT.V), "Expected Follow of "&Rule_Name&" to contain terminal " & To_String(TT.N));
      end loop;
   end Test_Follow;


   Close_Tuple : constant Terminal_Tuple_Type := (Termianl_close_paren, To_String_Type("close_paren"));
   EOF_Tuple : constant Terminal_Tuple_Type := (kv.apg.rules.End_Of_File, To_String_Type("end_of_file"));

   ----------------------------------------------------------------------------
   type Follow_1_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Follow_1_Test) is
   begin
      Set_Up_Alternate_ETF_Grammar(T);

      Test_Follow(T, "E", (1 => Close_Tuple, 2 => EOF_Tuple));
      Test_Follow(T, "E2", (1 => Close_Tuple, 2 => EOF_Tuple));

      Test_Follow(T, "T", (1 => Plus_Tuple, 2 => Close_Tuple, 3 => EOF_Tuple));
      Test_Follow(T, "T2", (1 => Plus_Tuple, 2 => Close_Tuple, 3 => EOF_Tuple));

      Test_Follow(T, "F", (1 => Plus_Tuple, 2 => Times_Tuple, 3 => Close_Tuple, 4 => EOF_Tuple));
   end Run;

   ----------------------------------------------------------------------------
   type Follow_2_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Follow_2_Test) is
   begin
      Set_Up_ABG_Grammar(T);

      Test_Follow(T, "program", (1 => EOF_Tuple));
      Test_Follow(T, "alpha_list", (1 => Alpha_Tuple, 2 => Gamma_Tuple));
      Test_Follow(T, "beta_list", (1 => Beta_Tuple));
      Test_Follow(T, "gamma_list", (1 => Gamma_Tuple));
   end Run;



   ----------------------------------------------------------------------------
   type Item_Neg_1_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Item_Neg_1_Test) is
      use kv.apg.rules;
      Rule : Rule_Pointer;
      Production : Production_Pointer;
      Item : Constant_Item_Pointer;
   begin
      Set_Up_ABG_Grammar(T);
      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("alpha_list"));
      Production := Rule.Get_Production(1);
      begin
         Item := New_Item_Class(Constant_Production_Pointer(Production), 0, Production.Get_Symbol(1));
         T.Fail("Expected Terminal_Expected_Error exception");
      exception
         when Terminal_Expected_Error =>
            null; -- Expected path
         when others =>
            T.Fail("Incorrect exception");
      end;
   end Run;

   ----------------------------------------------------------------------------
   type Item_Neg_2_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Item_Neg_2_Test) is
      use kv.apg.rules;
      Rule : Rule_Pointer;
      Production : Production_Pointer;
      Item : Constant_Item_Pointer;
   begin
      Set_Up_ABG_Grammar(T);
      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("alpha_list"));
      Production := Rule.Get_Production(1);
      begin
         Item := New_Item_Class(Constant_Production_Pointer(Production), 3, Production.Get_Symbol(2));
         T.Fail("Expected Dot_Position_Error exception");
      exception
         when Dot_Position_Error =>
            null; -- Expected path
         when others =>
            T.Fail("Incorrect exception");
      end;
   end Run;

   ----------------------------------------------------------------------------
   type Item_Neg_3_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Item_Neg_3_Test) is
      use kv.apg.rules;
      Rule : Rule_Pointer;
      Production : Production_Pointer;
      Item : Constant_Item_Pointer;
      Terminal : Constant_Symbol_Pointer;
   begin
      Set_Up_ABG_Grammar(T);
      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("alpha_list"));
      Production := Rule.Get_Production(1);
      Terminal := T.Grammar.Find_Non_Terminal(To_String_Type("beta_list")).Get_Production(1).Get_Symbol(2); -- Beta
      T.Assert(Terminal.Name = To_String_Type("Beta"), "Expected Terminal to be Beta");
      begin
         Item := New_Item_Class(Constant_Production_Pointer(Production), 0, Terminal);
         T.Fail("Expected Non_Following_Terminal_Error exception");
      exception
         when Non_Following_Terminal_Error =>
            null; -- Expected path
         when others =>
            T.Fail("Incorrect exception");
      end;
   end Run;

   ----------------------------------------------------------------------------
   type Item_Image_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Item_Image_Test) is
      use kv.apg.rules;
      Rule : Rule_Pointer;
      Production : Production_Pointer;
      Item : Constant_Item_Pointer;
      Expected : constant String_Type := To_String_Type("[alpha_list -> . alpha_list Alpha, Alpha]");
   begin
      Set_Up_ABG_Grammar(T);
      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("alpha_list"));
      Production := Rule.Get_Production(1);
      Item := New_Item_Class(Constant_Production_Pointer(Production), 0, Production.Get_Symbol(2));
      T.Assert(Item.Image = Expected, "Expected Item.Image to be <" &To_String(Expected)& ">, got <"&To_String(Item.Image)&">");
      Free(Item);
   end Run;

   ----------------------------------------------------------------------------
   type Item_Closure_Support_Functions_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Item_Closure_Support_Functions_Test) is
      use kv.apg.rules;
      Rule : Rule_Pointer;
      Production : Production_Pointer;
      Item : Constant_Item_Pointer;
      Terminal : Constant_Symbol_Pointer;
   begin
      Set_Up_ABG_Grammar(T);
      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("alpha_list"));
      Production := Rule.Get_Production(1);
      Terminal := Production.Get_Symbol(2);
      Item := New_Item_Class(Constant_Production_Pointer(Production), 0, Terminal);
      -- Item := "[alpha_list -> . alpha_list Alpha, Alpha]"
      T.Assert(Item.Get_Big_A = Rule, "Expected Big A of Item to be alpha_list, got " & To_String(Item.Get_Big_A.Get_Name));
      T.Assert(Item.Get_Big_B = Production.Get_Symbol(1), "Expected Item.Get_Big_B to be " & To_String(Production.Get_Symbol(1).Name) & ", got " & To_String(Item.Get_Big_B.Name));
      T.Assert(Item.Get_Little_A = Terminal, "Expected Item.Get_Little_A yo be Alpha terminal, got " & To_String(Item.Get_Little_A.Name));
      T.Assert(Item.Get_Little_Alpha = null, "Expected Item.Get_Little_Alpha to be ɛ (null), got something else");
      T.Assert(Item.Get_Little_Beta = Production.Get_Symbol(2), "Expected Item.Get_Little_Beta to be Alpha, got something else");
      Free(Item);
   end Run;

   ----------------------------------------------------------------------------
   type Item_Closure_Support_Functions_2_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Item_Closure_Support_Functions_2_Test) is
      use kv.apg.rules;
      Rule : Rule_Pointer;
      Production : Production_Pointer;
      Item : Constant_Item_Pointer;
      Terminal : Constant_Symbol_Pointer;
   begin
      Set_Up_ABG_Grammar(T);
      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("alpha_list"));
      Production := Rule.Get_Production(1);
      Terminal := T.Grammar.Find_Non_Terminal(To_String_Type("program")).Get_Production(1).Get_Symbol(2); -- Gamma
      T.Assert(Terminal.Name = To_String_Type("Gamma"), "Expected Terminal to be Gamma");
      Item := New_Item_Class(Constant_Production_Pointer(Production), 1, Terminal);
      -- Item := "[alpha_list -> alpha_list . Alpha, Gamma]"
      T.Assert(Item.Get_Big_A = Rule, "Expected Big A of Item to be alpha_list, got " & To_String(Item.Get_Big_A.Get_Name));
      T.Assert(Item.Get_Big_B = Production.Get_Symbol(2), "Expected Item.Get_Big_B to be " & To_String(Production.Get_Symbol(1).Name) & ", got " & To_String(Item.Get_Big_B.Name));
      T.Assert(Item.Get_Little_A = Terminal, "Expected Item.Get_Little_A yo be Gamma terminal, got " & To_String(Item.Get_Little_A.Name));
      T.Assert(Item.Get_Little_Alpha = Production.Get_Symbol(1), "Expected Item.Get_Little_Alpha to be alpha_list, got something else");
      T.Assert(Item.Get_Little_Beta = null, "Expected Item.Get_Little_Beta to be ɛ (null), got something else");
      Free(Item);
   end Run;

   ----------------------------------------------------------------------------
   type Item_Closure_Support_Functions_3_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Item_Closure_Support_Functions_3_Test) is
      use kv.apg.rules;
      Rule : Rule_Pointer;
      Production : Production_Pointer;
      Item : Constant_Item_Pointer;
      Terminal : Constant_Symbol_Pointer;
   begin
      Set_Up_ABG_Grammar(T);
      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("alpha_list"));
      Production := Rule.Get_Production(1);
      Terminal := T.Grammar.Find_Non_Terminal(To_String_Type("program")).Get_Production(1).Get_Symbol(2); -- Gamma
      T.Assert(Terminal.Name = To_String_Type("Gamma"), "Expected Terminal to be Gamma");
      Item := New_Item_Class(Constant_Production_Pointer(Production), 2, Terminal);
      T.Assert(Item.Image = To_String_Type("[alpha_list -> alpha_list Alpha ., Gamma]"), "Item.Image is wrong, got " & To_String(Item.Image));
      T.Assert(Item.Get_Big_A = Rule, "Expected Big A of Item to be alpha_list, got " & To_String(Item.Get_Big_A.Get_Name));
      T.Assert(Item.Get_Big_B = null, "Expected Item.Get_Big_B to be  ɛ (null), got something else");
      T.Assert(Item.Get_Little_A = Terminal, "Expected Item.Get_Little_A yo be Gamma terminal, got " & To_String(Item.Get_Little_A.Name));
      T.Assert(Item.Get_Little_Alpha = Production.Get_Symbol(2), "Expected Item.Get_Little_Alpha to be Alpha, got something else");
      T.Assert(Item.Get_Little_Beta = null, "Expected Item.Get_Little_Beta to be ɛ (null), got something else");
      Free(Item);
   end Run;

   ----------------------------------------------------------------------------
   type Table_Support_Functions_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Table_Support_Functions_Test) is
      use kv.apg.rules;
      Rule : Rule_Pointer;
      Production : Production_Pointer;
   begin
      Set_Up_ABG_Grammar(T);

      T.Assert(T.Grammar.Rule_Number_Lo = 1, "Expected Rule_Number_Lo to be 1");
      T.Assert(T.Grammar.Rule_Number_Hi = 4, "Expected Rule_Number_Hi to be 4");
      T.Assert(T.Grammar.Production_Number_Lo = 1, "Expected Production_Number_Lo to be 1");
      T.Assert(T.Grammar.Production_Number_Hi = 7, "Expected Production_Number_Hi to be 7");
      T.Assert(T.Grammar.Terminal_Lo = -1, "Expected Terminal_Lo to be -1");
      T.Assert(T.Grammar.Terminal_Hi = 3, "Expected Terminal_Hi to be 3");

      Rule := T.Grammar.Get_Rule(2);
      T.Assert(Rule.Get_Number = 2, "Expected rule number to be 2");
      Production := T.Grammar.Get_Production(3);
      T.Assert(Production.Get_Number = 3, "Expected production number to be 3");
   end Run;

   ----------------------------------------------------------------------------
   type Kernels_1_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Kernels_1_Test) is
      use kv.apg.rules;
      use Ada.Containers;
      I0 : Item_Sets.Set;
      Item : Constant_Item_Pointer;
      Expected : constant String_Type := To_String_Type("[program -> . alpha_list Gamma]");
   begin
      Set_Up_ABG_Grammar(T);
      I0 := T.Grammar.First_Kernel_Set;
      T.Assert(I0.Length = 1, "Expected I0's size to be 1");
      Item := I0.First_Element;
      T.Assert(Item.Image = Expected, "Wrong image, expected <"&To_String(Expected)&">, got <"&To_String(Item.Image)&">.");
   end Run;

   ----------------------------------------------------------------------------
   procedure Add_SLR_Enum(T : in out Grammar_Test_Class'CLASS) is
   begin
      T.Enum.Initialize(+"Enum_Type");
      T.Enum.Append(+"equal");        -- 1
      T.Enum.Append(+"star");       -- 2
      T.Enum.Append(+"id");          -- 3
      T.Grammar.Initialize(T.Enum);
   end Add_SLR_Enum;

   ----------------------------------------------------------------------------
   -- Grammar from example 4.46 the Dragon Book (page 241)
   procedure Set_Up_SLR_Grammar
      (T : in out Grammar_Test_Class'CLASS;
       Logger : in kv.apg.logger.Safe_Logger_Pointer) is
      Add_EOF : constant Boolean := True;
   begin
      Add_SLR_Enum(T);
      Run_Basic_Grammar_Test(T, 3,
         (01 => To_String_Type("rule S = start"),
          02 => To_String_Type(" | L equal R => «null;»"),
          03 => To_String_Type(" | R => «null;»"),
          04 => To_String_Type(" ;"),
          05 => To_String_Type("rule L ="),
          06 => To_String_Type(" | star R => «null;»"),
          07 => To_String_Type(" | id => «null;»"),
          08 => To_String_Type(" ;"),
          09 => To_String_Type("rule R ="),
          10 => To_String_Type(" | L => «null;»"),
          11 => To_String_Type(" ;")));
      T.Grammar.Add_Meta_Rule(Logger);
      T.Grammar.Resolve_Rules(Logger);
      T.Grammar.Resolve_Productions(Logger);
      T.Grammar.Resolve_Firsts(Logger);
      T.Grammar.Resolve_Follows(not Add_EOF, Logger);
      T.Grammar.Validate(Logger);
      T.Assert(T.Grammar.Get_Error_Count = 0, "Expected 0 resolve/validate errors got" & Natural'IMAGE(T.Grammar.Get_Error_Count));
   end Set_Up_SLR_Grammar;

   ----------------------------------------------------------------------------
   procedure Check_Kernel_Set_First
      (T : in out Grammar_Test_Class'CLASS;
       I : in     kv.apg.rules.Item_Sets.Set;
       S : in     String;
       E : in     String_Type) is

      use kv.apg.rules;
      use Ada.Containers;

      Item : Constant_Item_Pointer;

   begin
      T.Assert(I.Length = 1, "Expected "&S&"'s size to be 1");
      Item := I.First_Element;
      T.Assert(Item.Image = E, "Wrong "&S&" image, expected <"&To_String(E)&">, got <"&To_String(Item.Image)&">.");
   end Check_Kernel_Set_First;

   ----------------------------------------------------------------------------
   function Checked_Get_Symbol
      (T : in out Grammar_Test_Class'CLASS;
       S : in     String;
       R : in     String;
       P : in     Positive;
       O : in     Positive) return kv.apg.rules.Constant_Symbol_Pointer is
      use kv.apg.rules;
      Symbol : Constant_Symbol_Pointer;
   begin
      Symbol := T.Grammar.Get_Symbol(To_String_Type(R), P, O);
      T.Assert(Symbol.Name = To_String_Type(S), "Expected symbol " & S);
      return Symbol;
   end Checked_Get_Symbol;

   ----------------------------------------------------------------------------
   type Kernels_2_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Kernels_2_Test) is
      -- This test checks the states listed in Figure 4.42 of the Dragon Book (page 241).
      use kv.apg.rules;
      use Ada.Containers;
      I0 : Item_Sets.Set;
      I1 : Item_Sets.Set;
      I2 : Item_Sets.Set;
      I3 : Item_Sets.Set;
      I4 : Item_Sets.Set;
      I5 : Item_Sets.Set;
      I6 : Item_Sets.Set;
      I7 : Item_Sets.Set;
      I8 : Item_Sets.Set;
      I9 : Item_Sets.Set;
      I0_Expected : constant String_Type := To_String_Type("[Meta_Start_Rule -> . S End_Of_File]");
      I1_Expected : constant String_Type := To_String_Type("[Meta_Start_Rule -> S . End_Of_File]");
      I2b_Expected : constant String_Type := To_String_Type("[S -> L . equal R]");
      I2a_Expected : constant String_Type := To_String_Type("[R -> L .]");
      I3_Expected : constant String_Type := To_String_Type("[S -> R .]");
      I4_Expected : constant String_Type := To_String_Type("[L -> star . R]");
      I5_Expected : constant String_Type := To_String_Type("[L -> id .]");
      I6_Expected : constant String_Type := To_String_Type("[S -> L equal . R]");
      I7_Expected : constant String_Type := To_String_Type("[L -> star R .]");
      I8_Expected : constant String_Type := To_String_Type("[R -> L .]");
      I9_Expected : constant String_Type := To_String_Type("[S -> L equal R .]");
      Item : Constant_Item_Pointer;
      Symbol : Constant_Symbol_Pointer;

      CW : aliased kv.apg.writer.console.Console_Writer_Class;
      CL : aliased kv.apg.logger.writer.Writer_Logger_Class;
      Logger : kv.apg.logger.Safe_Logger_Pointer := T.Logger'UNCHECKED_ACCESS;
   begin
      CL.Initialize
         (Writer => CW'UNCHECKED_ACCESS,
          Level  => Debug);
      --Logger := CL'UNCHECKED_ACCESS;

      Set_Up_SLR_Grammar(T, Logger);
      I0 := T.Grammar.First_Kernel_Set;
      Check_Kernel_Set_First(T, I0, "I0", I0_Expected);

      Item := I0.First_Element;
      I1 := T.Grammar.Goto_Step_Over(I0, 0, Item.Get_Big_B, Logger);
      Check_Kernel_Set_First(T, I1, "I1", I1_Expected);

      Symbol := Checked_Get_Symbol(T, "L", "S", 1, 1);
      I2 := T.Grammar.Goto_Step_Into(I0, 0, Symbol, Logger);
      T.Assert(I2.Length = 2, "Expected I2's size to be 2");
      Item := I2.First_Element;
      T.Assert(Item.Image = I2a_Expected, "Wrong I2a image, expected <"&To_String(I2a_Expected)&">, got <"&To_String(Item.Image)&">.");
      Item := I2.Last_Element;
      T.Assert(Item.Image = I2b_Expected, "Wrong I2b image, expected <"&To_String(I2b_Expected)&">, got <"&To_String(Item.Image)&">.");

      Symbol := Checked_Get_Symbol(T, "R", "S", 2, 1);
      I3 := T.Grammar.Goto_Step_Into(I0, 0, Symbol, Logger);
      Check_Kernel_Set_First(T, I3, "I3", I3_Expected);

      Symbol := Checked_Get_Symbol(T, "star", "L", 1, 1);
      I4 := T.Grammar.Goto_Step_Into(I0, 0, Symbol, Logger);
      Check_Kernel_Set_First(T, I4, "I4", I4_Expected);

      Symbol := Checked_Get_Symbol(T, "id", "L", 2, 1);
      I5 := T.Grammar.Goto_Step_Into(I0, 0, Symbol, Logger);
      Check_Kernel_Set_First(T, I5, "I5", I5_Expected);

      Symbol := Checked_Get_Symbol(T, "equal", "S", 1, 2);
      I6 := T.Grammar.Goto_Step_Over(I2, 2, Symbol, Logger);
      Check_Kernel_Set_First(T, I6, "I6", I6_Expected);

      Symbol := Checked_Get_Symbol(T, "R", "S", 2, 1);
      I7 := T.Grammar.Goto_Step_Over(I4, 4, Symbol, Logger);
      Check_Kernel_Set_First(T, I7, "I7", I7_Expected);

      Symbol := Checked_Get_Symbol(T, "L", "S", 1, 1);
      I8 := T.Grammar.Goto_Step_Into(I6, 6, Symbol, Logger);
      Check_Kernel_Set_First(T, I8, "I8", I8_Expected);

      Symbol := Checked_Get_Symbol(T, "R", "S", 2, 1);
      I9 := T.Grammar.Goto_Step_Over(I6, 6, Symbol, Logger);
      Check_Kernel_Set_First(T, I9, "I9", I9_Expected);
   end Run;

   ----------------------------------------------------------------------------
   procedure Debug_Print_States
      (T : in     Grammar_Test_Class'CLASS;
       All_Symbols : kv.apg.rules.Symbol_Vectors.Vector;
       State_Info : kv.apg.rules.State_Information_Type) is

      use kv.apg.rules;
      use Ada.Containers;

      function Safe_Name(S : Constant_Symbol_Pointer) return String is
      begin
         if S = null then
            return "null";
         end if;
         return To_String(S.Name);
      exception
         when X: others =>
            return Exception_Name(X) & "+" & Exception_Message(X);
      end Safe_Name;

   begin
      for S of All_Symbols loop
         Put_Line(To_String(S.Name));
      end loop;
      for P in T.Grammar.Production_Number_Lo .. T.Grammar.Production_Number_Hi loop
         Put_Line("Production "&Production_Index_Type'IMAGE(P)&" = " & To_String(T.Grammar.Get_Production(P).Image));
      end loop;
      for S of State_Info.States loop
         Put_Line("----- " & Img(S.Index));
         for K of S.Kernels loop
            Put_Line(To_String(K.Image));
         end loop;
      end loop;
      for H of State_Info.Hints loop
         Put_Line("Symbol "&Safe_Name(H.Symbol)&
                  ", From "&Img(H.From_State)&
                  ", To "&Img(H.To_State));
      end loop;
   end Debug_Print_States;

   ----------------------------------------------------------------------------
   type Kernels_3_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Kernels_3_Test) is
      -- This test checks the states listed in Figure 4.42 of the Dragon Book (page 241).
      use kv.apg.rules;
      use Ada.Containers;
      All_Symbols : Symbol_Vectors.Vector;
      State_Info : State_Information_Type;
      Logger : kv.apg.logger.Safe_Logger_Pointer := T.Logger'UNCHECKED_ACCESS;
      CW : aliased kv.apg.writer.console.Console_Writer_Class;
      CL : aliased kv.apg.logger.writer.Writer_Logger_Class;
   begin
      CL.Initialize
         (Writer => CW'UNCHECKED_ACCESS,
          Level  => Debug);
      --Logger := CL'UNCHECKED_ACCESS;

      Set_Up_SLR_Grammar(T, Logger);
      All_Symbols := T.Grammar.Grammar_Symbols;
      T.Assert(All_Symbols.Length = 7, "Expected 7 symbols in the SLR grammar.");
      State_Info := T.Grammar.Generate_Parser_States(Logger);
      T.Assert(State_Info.States.Length = 11, "Expected 11 parser states in the SLR grammar, got " & Natural'IMAGE(Natural(State_Info.States.Length)));
      --Debug_Print_States(T, All_Symbols, State_Info);
   end Run;

   ----------------------------------------------------------------------------
   procedure Add_4_22_Enum(T : in out Grammar_Test_Class'CLASS) is
   begin
      T.Enum.Initialize(+"Enum_Type");
      T.Enum.Append(+"plus");        -- 1
      T.Enum.Append(+"times");       -- 2
      T.Enum.Append(+"id");          -- 3
      T.Enum.Append(+"open");          -- 4
      T.Enum.Append(+"close");          -- 5
      T.Grammar.Initialize(T.Enum);
   end Add_4_22_Enum;

   ----------------------------------------------------------------------------
   -- Grammar from example 4.46 the Dragon Book (page 241)
   procedure Set_Up_4_22_Grammar
      (T : in out Grammar_Test_Class'CLASS;
       Logger : in kv.apg.logger.Safe_Logger_Pointer) is
      Add_EOF : constant Boolean := True;
   begin
      Add_4_22_Enum(T);
      Run_Basic_Grammar_Test(T, 1,
         (01 => To_String_Type("rule E = start"),
          02 => To_String_Type(" | E plus E => «null;»"),
          03 => To_String_Type(" | E times E => «null;»"),
          04 => To_String_Type(" | open E close => «null;»"),
          05 => To_String_Type(" | id => «null;»"),
          06 => To_String_Type(" ;")));
      T.Grammar.Add_Meta_Rule(Logger);
      T.Grammar.Resolve_Rules(Logger);
      T.Grammar.Resolve_Productions(Logger);
      T.Grammar.Resolve_Firsts(Logger);
      T.Grammar.Resolve_Follows(not Add_EOF, Logger);
      T.Grammar.Validate(Logger);
      T.Assert(T.Grammar.Get_Error_Count = 0, "Expected 0 resolve/validate errors got" & Natural'IMAGE(T.Grammar.Get_Error_Count));
   end Set_Up_4_22_Grammar;

   ----------------------------------------------------------------------------
   type Kernels_4_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Kernels_4_Test) is
      -- This test checks the states listed in Figure 4.46 of the Dragon Book (page 248).
      use kv.apg.rules;
      use Ada.Containers;
      All_Symbols : Symbol_Vectors.Vector;
      State_Info : State_Information_Type;
      Logger : kv.apg.logger.Safe_Logger_Pointer := T.Logger'UNCHECKED_ACCESS;
      CW : aliased kv.apg.writer.console.Console_Writer_Class;
      CL : aliased kv.apg.logger.writer.Writer_Logger_Class;

   begin
      CL.Initialize
         (Writer => CW'UNCHECKED_ACCESS,
          Level  => Debug);
      --Logger := CL'UNCHECKED_ACCESS;

      Set_Up_4_22_Grammar(T, Logger);
      All_Symbols := T.Grammar.Grammar_Symbols;
      T.Assert(All_Symbols.Length = 7, "Expected 7 symbols in the 4.22 grammar.");
      State_Info := T.Grammar.Generate_Parser_States(Logger);
      T.Assert(State_Info.States.Length = 11, "Expected 11 parser states in the 4.22 grammar, got " & Natural'IMAGE(Natural(State_Info.States.Length)));
      --Debug_Print_States(T, All_Symbols, State_Info);
   end Run;


   ----------------------------------------------------------------------------
   type Kernels_5_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Kernels_5_Test) is
      use kv.apg.rules;
      use Ada.Containers;
      All_Symbols : Symbol_Vectors.Vector;
      State_Info : State_Information_Type;

      Add_EOF : constant Boolean := True;

      Logger : kv.apg.logger.Safe_Logger_Pointer := T.Logger'UNCHECKED_ACCESS;
      CW : aliased kv.apg.writer.console.Console_Writer_Class;
      CL : aliased kv.apg.logger.writer.Writer_Logger_Class;
   begin
      CL.Initialize
         (Writer => CW'UNCHECKED_ACCESS,
          Level  => Debug);
      --Logger := CL'UNCHECKED_ACCESS;

      Set_Up_ETF_Grammar(T);
      T.Grammar.Add_Meta_Rule(Logger);
      T.Grammar.Resolve_Rules(Logger);
      T.Grammar.Resolve_Productions(Logger);
      T.Grammar.Resolve_Firsts(Logger);
      T.Grammar.Resolve_Follows(not Add_EOF, Logger);
      T.Grammar.Validate(Logger);
      T.Assert(T.Grammar.Get_Error_Count = 0, "Expected 0 resolve/validate errors got" & Natural'IMAGE(T.Grammar.Get_Error_Count));

      All_Symbols := T.Grammar.Grammar_Symbols;
      T.Assert(All_Symbols.Length = 9, "Expected 9 symbols in the ETF grammar, got " & Natural'IMAGE(Natural(All_Symbols.Length)));
      State_Info := T.Grammar.Generate_Parser_States(Logger);
      T.Assert(State_Info.States.Length = 13, "Expected 13 parser states in the ETF grammar, got " & Natural'IMAGE(Natural(State_Info.States.Length)));
--      Debug_Print_States(T, All_Symbols, State_Info);
   end Run;






   ----------------------------------------------------------------------------
   type Action_Table_1_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out Action_Table_1_Test) is
      use kv.apg.rules;
      Table : Action_Table_Class;
      Action : Action_Entry_Type;
   begin
      Table.Initialize(5, kv.apg.rules.End_Of_File, 5);
      Action := (What => Shift, Where => 2);
      Table.Set_Action(Action, 0, 0, T.Logger'UNCHECKED_ACCESS);
      Action := Table.Get_Action(0, 0);
      T.Assert(Action.What = Shift, "Expected Shift");
      T.Assert(Action.Where = 2, "Expected 2");
      Action := Table.Get_Action(1, 1);
      T.Assert(Action.What = Error, "Expected Error");
   end Run;

   ----------------------------------------------------------------------------
   type State_Stack_1_Test is new Grammar_Test_Class with null record;
   procedure Run(T : in out State_Stack_1_Test) is
      use kv.apg.rules;
      Stack : Stack_Class;
      State : State_Entry_Type;
   begin
      State.State := 1;
      Stack.Push_State(State);
      T.Assert(Stack.Top_State = 1, "Expected 1");
      State.State := 5;
      Stack.Push_State(State);
      State.State := 9;
      Stack.Push_State(State);
      T.Assert(Stack.Top_State = 9, "Expected 9");
      State := Stack.Pop_State;
      State := Stack.Pop_State;
      T.Assert(State.State = 5, "Expected 5");
   end Run;



   ----------------------------------------------------------------------------
   type Parser_Engine_Test_Class is abstract new Grammar_Test_Class with
      record
         Engine : aliased kv.apg.rules.Parser_Engine_Class;
         CW : aliased kv.apg.writer.console.Console_Writer_Class;
         CL : aliased kv.apg.logger.writer.Writer_Logger_Class;
      end record;

   ----------------------------------------------------------------------------
   overriding procedure Set_Up(T : in out Parser_Engine_Test_Class) is
      use kv.apg.rules;
   begin
      Set_Up(Grammar_Test_Class(T));
      T.CL.Initialize
         (Writer => T.CW'UNCHECKED_ACCESS,
          Level  => Debug);
   end Set_Up;


   ----------------------------------------------------------------------------
   procedure Load_ETF_Into_Engine
      (T      : in out Parser_Engine_Test_Class'CLASS;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is

      Add_EOF : constant Boolean := True;

   begin
      Set_Up_ETF_Grammar(T);
      T.Grammar.Add_Meta_Rule(Logger);
      T.Grammar.Resolve_Rules(Logger);
      T.Grammar.Resolve_Productions(Logger);
      T.Grammar.Resolve_Firsts(Logger);
      T.Grammar.Resolve_Follows(not Add_EOF, Logger);
      T.Grammar.Validate(Logger);
      T.Assert(T.Grammar.Get_Error_Count = 0, "Expected 0 resolve/validate errors got" & Natural'IMAGE(T.Grammar.Get_Error_Count));
      T.Engine.Initialize(T.Grammar'UNCHECKED_ACCESS, Logger);
   end Load_ETF_Into_Engine;

   package ETF_Tokens is
      eof         : constant := -1;
      plus        : constant kv.apg.rules.Terminal_Index_Type := 1;
      times       : constant kv.apg.rules.Terminal_Index_Type := 2;
      id          : constant kv.apg.rules.Terminal_Index_Type := 3;
      open_paren  : constant kv.apg.rules.Terminal_Index_Type := 4;
      close_paren : constant kv.apg.rules.Terminal_Index_Type := 5;
   end ETF_Tokens;

   type Token_Array_Type is array (Positive range <>) of kv.apg.rules.Terminal_Index_Type;

   ----------------------------------------------------------------------------
   procedure Load_Tokens_Into_Engine
      (T      : in out Parser_Engine_Test_Class'CLASS;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer;
       Tokens : in     Token_Array_Type) is
   begin
      for Token of Tokens loop
         T.Engine.Parse_Token(Token, Logger);
      end loop;
   end Load_Tokens_Into_Engine;


   ----------------------------------------------------------------------------
   type Parser_Engine_1_Test is new Parser_Engine_Test_Class with null record;
   procedure Run(T : in out Parser_Engine_1_Test) is
      use kv.apg.rules;
      Logger : kv.apg.logger.Safe_Logger_Pointer := T.Logger'UNCHECKED_ACCESS;
   begin
      --Logger := T.CL'UNCHECKED_ACCESS;
      Load_ETF_Into_Engine(T, Logger);
      --Put_Line("==========================================");
      --Logger := T.CL'UNCHECKED_ACCESS;
      Load_Tokens_Into_Engine(T, Logger, (ETF_Tokens.id, ETF_Tokens.plus, ETF_Tokens.id, ETF_Tokens.eof));
      T.Assert(T.Engine.Has_Accepted, "Expected the token sequence to be accepted.");
      T.Assert(T.Engine.Error_Count = 0, "Expected the error count to be 0, got" & Natural'IMAGE(T.Engine.Error_Count));
   end Run;

   ----------------------------------------------------------------------------
   type Parser_Engine_2_Test is new Parser_Engine_Test_Class with null record;
   procedure Run(T : in out Parser_Engine_2_Test) is
      use kv.apg.rules;
      Logger : kv.apg.logger.Safe_Logger_Pointer := T.Logger'UNCHECKED_ACCESS;
   begin
      --Logger := T.CL'UNCHECKED_ACCESS;
      Load_ETF_Into_Engine(T, Logger);
      --Put_Line("==========================================");
      --Logger := T.CL'UNCHECKED_ACCESS;
      Load_Tokens_Into_Engine(T, Logger, (ETF_Tokens.id, ETF_Tokens.plus, ETF_Tokens.id, ETF_Tokens.times, ETF_Tokens.id, ETF_Tokens.eof));
      T.Assert(T.Engine.Has_Accepted, "Expected the token sequence to be accepted.");
      T.Assert(T.Engine.Error_Count = 0, "Expected the error count to be 0, got" & Natural'IMAGE(T.Engine.Error_Count));
   end Run;

   ----------------------------------------------------------------------------
   type Parser_Engine_3_Test is new Parser_Engine_Test_Class with null record;
   procedure Run(T : in out Parser_Engine_3_Test) is
      use kv.apg.rules;
      use ETF_Tokens;
      Logger : kv.apg.logger.Safe_Logger_Pointer := T.Logger'UNCHECKED_ACCESS;
   begin
      --Logger := T.CL'UNCHECKED_ACCESS;
      Load_ETF_Into_Engine(T, Logger);
      --Put_Line("==========================================");
      --Logger := T.CL'UNCHECKED_ACCESS;
      Load_Tokens_Into_Engine(T, Logger, (open_paren, id, plus, id, close_paren, times, id, eof));
      T.Assert(T.Engine.Has_Accepted, "Expected the token sequence to be accepted.");
      T.Assert(T.Engine.Error_Count = 0, "Expected the error count to be 0, got" & Natural'IMAGE(T.Engine.Error_Count));
   end Run;

   ----------------------------------------------------------------------------
   type Parser_Engine_4_Test is new Parser_Engine_Test_Class with null record;
   procedure Run(T : in out Parser_Engine_4_Test) is
      use kv.apg.rules;
      use ETF_Tokens;
      Logger : kv.apg.logger.Safe_Logger_Pointer := T.Logger'UNCHECKED_ACCESS;
   begin
      --Logger := T.CL'UNCHECKED_ACCESS;
      Load_ETF_Into_Engine(T, Logger);
      --Put_Line("==========================================");
      --Logger := T.CL'UNCHECKED_ACCESS;
      Load_Tokens_Into_Engine(T, Logger, (id, plus, times, id, eof));
      T.Assert(not T.Engine.Has_Accepted, "Expected the token sequence to be fail.");
      T.Assert(T.Engine.Error_Count = 1, "Expected the error count to be 1, got" & Natural'IMAGE(T.Engine.Error_Count));
   end Run;

   ----------------------------------------------------------------------------
   procedure Load_4_22_Into_Engine
      (T      : in out Parser_Engine_Test_Class'CLASS;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is
   begin
      Set_Up_4_22_Grammar(T, Logger);
      T.Engine.Initialize(T.Grammar'UNCHECKED_ACCESS, Logger);
   end Load_4_22_Into_Engine;

   package Grammar_4_22_Tokens is
      eof   : constant := -1;
      plus  : constant kv.apg.rules.Terminal_Index_Type := 1;
      times : constant kv.apg.rules.Terminal_Index_Type := 2;
      id    : constant kv.apg.rules.Terminal_Index_Type := 3;
      open  : constant kv.apg.rules.Terminal_Index_Type := 4;
      close : constant kv.apg.rules.Terminal_Index_Type := 5;
   end Grammar_4_22_Tokens;

   ----------------------------------------------------------------------------
   type Parser_Engine_5_Test is new Parser_Engine_Test_Class with null record;
   procedure Run(T : in out Parser_Engine_5_Test) is
      use kv.apg.rules;
      use Grammar_4_22_Tokens;
      Logger : kv.apg.logger.Safe_Logger_Pointer := T.Logger'UNCHECKED_ACCESS;
   begin
      Logger := T.CL'UNCHECKED_ACCESS;
      Load_4_22_Into_Engine(T, Logger);
      --Put_Line("==========================================");
      --Logger := T.CL'UNCHECKED_ACCESS;
      Load_Tokens_Into_Engine(T, Logger, (id, plus, id, times, id, eof));
      T.Assert(T.Engine.Has_Accepted, "Expected the token sequence to be accepted.");
      T.Assert(T.Engine.Error_Count = 0, "Expected the error count to be 0, got" & Natural'IMAGE(T.Engine.Error_Count));
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
      suite.register(new Grammar_Prime_1_Test, "Grammar_Prime_1_Test");
--      suite.register(new XXX, "XXX");
      suite.register(new Can_Disappear_1_Test, "Can_Disappear_1_Test");
      suite.register(new Can_Disappear_2_Test, "Can_Disappear_2_Test");
      suite.register(new Can_Disappear_3_Test, "Can_Disappear_3_Test");
      suite.register(new First_1_Test, "First_1_Test");
      suite.register(new First_2_Test, "First_2_Test");
      suite.register(new Follow_1_Test, "Follow_1_Test");
      suite.register(new Follow_2_Test, "Follow_2_Test");
      suite.register(new Item_Neg_1_Test, "Item_Neg_1_Test");
      suite.register(new Item_Neg_2_Test, "Item_Neg_2_Test");
      suite.register(new Item_Neg_3_Test, "Item_Neg_3_Test");
      suite.register(new Item_Image_Test, "Item_Image_Test");
      suite.register(new Item_Closure_Support_Functions_Test, "Item_Closure_Support_Functions_Test");
      suite.register(new Item_Closure_Support_Functions_2_Test, "Item_Closure_Support_Functions_2_Test");
      suite.register(new Item_Closure_Support_Functions_3_Test, "Item_Closure_Support_Functions_3_Test");
      suite.register(new Table_Support_Functions_Test, "Table_Support_Functions_Test");
      suite.register(new Kernels_1_Test, "Kernels_1_Test");
      suite.register(new Kernels_2_Test, "Kernels_2_Test");
      suite.register(new Kernels_3_Test, "Kernels_3_Test");
      suite.register(new Kernels_4_Test, "Kernels_4_Test");
      suite.register(new Kernels_5_Test, "Kernels_5_Test");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");

      suite.register(new Action_Table_1_Test, "Action_Table_1_Test");
      suite.register(new State_Stack_1_Test, "State_Stack_1_Test");
      suite.register(new Parser_Engine_1_Test, "Parser_Engine_1_Test");
      suite.register(new Parser_Engine_2_Test, "Parser_Engine_2_Test");
      suite.register(new Parser_Engine_3_Test, "Parser_Engine_3_Test");
      suite.register(new Parser_Engine_4_Test, "Parser_Engine_4_Test");
      suite.register(new Parser_Engine_5_Test, "Parser_Engine_5_Test");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");
   end register;

end kv.apg.tests.parse;
