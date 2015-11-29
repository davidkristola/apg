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
      T.Grammar.Resolve_Follows(T.Logger'UNCHECKED_ACCESS);
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

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("alpha_list"));
      T.Assert(Rule.Production_Count = 2, "Expected 2 productions for rule 'alpha_list'");
      T.Assert(not Rule.Has_An_Empty_Sequence, "Rule 'alpha_list' should not have an empty sequence");
      T.Assert(Rule.Can_Disappear, "Rule 'alpha_list' should be able to disappear");
      Pp := Rule.Get_Production(1);
      T.Assert(not Pp.Can_Disappear, "Rule 'alpha_list' production 1 should not be able to disappear");
      T.Assert(not Pp.Matches_An_Empty_Sequence, "Rule 'alpha_list' production 1 should not match an empty sequence");
      T.Assert(Pp.Has_A_Terminal, "Rule 'alpha_list' production 1 should have a terminal");
      Pp := Rule.Get_Production(2);
      T.Assert(Pp.Can_Disappear, "Rule 'alpha_list' production 2 should be able to disappear");
      T.Assert(not Pp.Matches_An_Empty_Sequence, "Rule 'alpha_list' production 2 should not match an empty sequence");
      T.Assert(not Pp.Has_A_Terminal, "Rule 'alpha_list' production 2 should not have a terminal");

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("beta_list"));
      T.Assert(Rule.Production_Count = 2, "Expected 2 productions for rule 'beta_list'");
      T.Assert(not Rule.Has_An_Empty_Sequence, "Rule 'beta_list' should not have an empty sequence");
      T.Assert(Rule.Can_Disappear, "Rule 'beta_list' should be able to disappear");
      Pp := Rule.Get_Production(1);
      T.Assert(not Pp.Can_Disappear, "Rule 'beta_list' production 1 should not be able to disappear");
      T.Assert(not Pp.Matches_An_Empty_Sequence, "Rule 'beta_list' production 1 should not match an empty sequence");
      T.Assert(Pp.Has_A_Terminal, "Rule 'beta_list' production 1 should have a terminal");
      Pp := Rule.Get_Production(2);
      T.Assert(Pp.Can_Disappear, "Rule 'beta_list' production 2 should be able to disappear");
      T.Assert(not Pp.Matches_An_Empty_Sequence, "Rule 'beta_list' production 2 should not match an empty sequence");
      T.Assert(not Pp.Has_A_Terminal, "Rule 'beta_list' production 2 should not have a terminal");

      Rule := T.Grammar.Find_Non_Terminal(To_String_Type("gamma_list"));
      T.Assert(Rule.Production_Count = 2, "Expected 2 productions for rule 'gamma_list'");
      T.Assert(Rule.Has_An_Empty_Sequence, "Rule 'gamma_list' should have an empty sequence");
      T.Assert(Rule.Can_Disappear, "Rule 'gamma_list' should be able to disappear");
      Pp := Rule.Get_Production(1);
      T.Assert(not Pp.Can_Disappear, "Rule 'gamma_list' production 1 should not be able to disappear");
      T.Assert(not Pp.Matches_An_Empty_Sequence, "Rule 'gamma_list' production 1 should not match an empty sequence");
      T.Assert(Pp.Has_A_Terminal, "Rule 'gamma_list' production 1 should have a terminal");
      Pp := Rule.Get_Production(2);
      T.Assert(Pp.Can_Disappear, "Rule 'gamma_list' production 2 should be able to disappear");
      T.Assert(Pp.Matches_An_Empty_Sequence, "Rule 'gamma_list' production 2 should match an empty sequence");
      T.Assert(not Pp.Has_A_Terminal, "Rule 'gamma_list' production 2 should not have a terminal");

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
      T.Grammar.Resolve_Follows(T.Logger'UNCHECKED_ACCESS);
      T.Grammar.Validate(T.Logger'UNCHECKED_ACCESS);
      T.Assert(T.Grammar.Get_Error_Count = 0, "Expected 0 resolve/validate errors got" & Natural'IMAGE(T.Grammar.Get_Error_Count));
   end Set_Up_Alternate_ETF_Grammar;


   type Terminal_Tuple_Type is
      record
         V : kv.apg.rules.Terminal_Type;
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
      suite.register(new Can_Disappear_1_Test, "Can_Disappear_1_Test");
      suite.register(new Can_Disappear_2_Test, "Can_Disappear_2_Test");
      suite.register(new Can_Disappear_3_Test, "Can_Disappear_3_Test");
      suite.register(new First_1_Test, "First_1_Test");
      suite.register(new First_2_Test, "First_2_Test");
      suite.register(new Follow_1_Test, "Follow_1_Test");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");
   end register;

end kv.apg.tests.parse;
