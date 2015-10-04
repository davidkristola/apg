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
with kv.apg.lexgen;
with kv.apg.enum;
with kv.apg.writer.buffer;
with kv.apg.rewriter;
with kv.apg.config;

with kv.apg.tests.lex_lex;
with kv.apg.tests.lex_parse;

package body kv.apg.tests.lex_gen is

   use Ada.Characters.Conversions;
   use Ada.Strings.Wide_Wide_Unbounded;
   use kv.core.wwstr;
   use kv.apg.lex;
   use kv.apg.tokens;
   use kv.apg.fast;
   use kv.apg.tests.lex_lex;
   use kv.apg.tests.lex_parse;






   ----------------------------------------------------------------------------
   type Base_Lexgen_Test_Class is abstract new Parser_Test_Class with
      record
         -- part of base class: Lexer : kv.apg.lex.Lexer_Class;
         -- part of base class: Parser : aliased kv.apg.parse.Parser_Class;
         Generator : kv.apg.lexgen.Generator_Class;
         Buff : kv.apg.writer.buffer.Buffer_Writer_Class;
      end record;

   ----------------------------------------------------------------------------
   procedure Test_Line
      (T      : in out Base_Lexgen_Test_Class'CLASS;
       Number : in     Positive;
       Line   : in     String) is
   begin
      if T.Buff.Line_Count < Number then
         T.Fail("Too few lines in the buffer!");
      else
         T.Assert(T.Buff.Get_Line(Number) = To_String_Type(Line), "Should be '"&Line&"', is '" & To_UTF(+T.Buff.Get_Line(Number)) & "'");
      end if;
   end Test_Line;


   ----------------------------------------------------------------------------
   type Lexgen_Count_Tokens_Test is new Base_Lexgen_Test_Class with null record;
   procedure Run(T : in out Lexgen_Count_Tokens_Test) is
      use kv.apg.lexgen;
   begin
      Parse_This(T, "token one = 'a' 'b' 'c';");
      T.Generator.Initialize(T.Parser'UNCHECKED_ACCESS, +"My_Package");
      T.Assert(T.Generator.Token_Count = 1, "Should have 1 token");
   end Run;

   ----------------------------------------------------------------------------
   type Lexgen_Package_Name_Test is new Base_Lexgen_Test_Class with null record;
   procedure Run(T : in out Lexgen_Package_Name_Test) is
      use kv.apg.lexgen;
      Template_Line : constant String := "package_name";
      Expected_Line : constant String := "my_lex_example";
      empty : String_Type;
      Buf : kv.apg.writer.buffer.Buffer_Writer_Class;
   begin
      T.Generator.Initialize(T.Parser'UNCHECKED_ACCESS, +"my_lex_example");
      Buf := kv.apg.writer.buffer.Buffer_Writer_Class(T.Generator.Convert(empty, empty, To_String_Type(Template_Line)));
      T.Assert(Buf.Get_Line(1) = To_String_Type(Expected_Line), "Produced line was wrong, got:" & To_UTF(+Buf.Get_Line(1)));
   end Run;

   ----------------------------------------------------------------------------
   type Lexgen_One_Token_Recognizer_Test is new Base_Lexgen_Test_Class with null record;
   procedure Run(T : in out Lexgen_One_Token_Recognizer_Test) is
      use kv.apg.lexgen;
      Line_0 : constant String := ""; -- Empty lines
      Line_1 : constant String := "-- This file is machine generated. Do not edit.";
      Line_3 : constant String := "package my_lex_example is";
   begin
      Parse_This(T, "token One = 'a' 'b' 'c';");
      T.Generator.Initialize(T.Parser'UNCHECKED_ACCESS, +"my_lex_example");
      T.Generator.Write_Spec(T.Buff);
      Test_Line(T, 1, Line_1);
      Test_Line(T, 2, Line_0);
      Test_Line(T, 3, Line_3);
   end Run;







   ----------------------------------------------------------------------------
   type Lexgen_Token_Type_Test is new Base_Lexgen_Test_Class with null record;
   procedure Run(T : in out Lexgen_Token_Type_Test) is
      use kv.apg.lexgen;
   begin
      Parse_This(T, "token One = 'a' 'b' 'c';");
      Parse_This(T, "token Two = 'a' 'q' 'z';");
      Parse_This(T, "token Three = 'e' 'f' 'g';");
      T.Generator.Initialize(T.Parser'UNCHECKED_ACCESS, +"My_Package");
      T.Assert(T.Generator.Token_Count = 3, "Should have 3 token");
      T.Generator.Insert_Token_Type(T.Buff);
      T.Buff.New_Line;
      Test_Line(T, 1, "   type Token_Type is");
      Test_Line(T, 2, "      (Invalid,");
      Test_Line(T, 3, "       One,");
      Test_Line(T, 4, "       Two,");
      Test_Line(T, 5, "       Three);");
   end Run;


   ----------------------------------------------------------------------------
   type Lexgen_State_List_Test is new Base_Lexgen_Test_Class with null record;
   procedure Run(T : in out Lexgen_State_List_Test) is
      use kv.apg.fast;
      State_List_Image : constant String := "[1{97=>2,97=>5,101=>8}/2{98=>3}/3{99=>4}/(4:1){}/5{113=>6}/6{122=>7}/(7:2){}/8{102=>9}/9{103=>10}/(10:3){}]";
      Cnfa : aliased kv.apg.fa.nfa.Nfa_Class;
   begin
      Parse_This(T, "token One = 'a' 'b' 'c';");
      Parse_This(T, "token Two = 'a' 'q' 'z';");
      Parse_This(T, "token Three = 'e' 'f' 'g';");
      T.Generator.Initialize(T.Parser'UNCHECKED_ACCESS, +"My_Package");
      Cnfa := T.Generator.Get_Cnfa;
      T.Assert(Cnfa.Image = State_List_Image, "Wrong Clean NFA image! Expected <"&State_List_Image&">, got <" & Cnfa.Image & ">");
   end Run;


   ----------------------------------------------------------------------------
   type Lexgen_State_List_Source_Code_Test is new Base_Lexgen_Test_Class with null record;
   procedure Run(T : in out Lexgen_State_List_Source_Code_Test) is
      use kv.apg.fast;
      Expected_1 : constant String := "(1 => (MATCH, 2, WWC'VAL(97)), 2 => (MATCH, 5, WWC'VAL(97)), 3 => (MATCH, 8, WWC'VAL(101)))";
      Expected_2 : constant String := "(1 => (MATCH, 3, WWC'VAL(98)))";
      Expected_3 : constant String := "(1 => (MATCH, 4, WWC'VAL(99)))";
      Expected_5 : constant String := "(1 => (MATCH, 6, WWC'VAL(113)))";
      Expected_6 : constant String := "(1 => (MATCH, 7, WWC'VAL(122)))";
      Expected_8 : constant String := "(1 => (MATCH, 9, WWC'VAL(102)))";
      Expected_9 : constant String := "(1 => (MATCH, 10, WWC'VAL(103)))";
      Expected_10 : constant String := "1 => (Invalid, T1'ACCESS),";
      Expected_11 : constant String := "2 => (Invalid, T2'ACCESS),";
      Expected_12 : constant String := "3 => (Invalid, T3'ACCESS),";
      Expected_13 : constant String := "4 => (One, null),";
      Expected_14 : constant String := "5 => (Invalid, T5'ACCESS),";
      Expected_15 : constant String := "6 => (Invalid, T6'ACCESS),";
      Expected_16 : constant String := "7 => (Two, null),";
      Expected_17 : constant String := "8 => (Invalid, T8'ACCESS),";
      Expected_18 : constant String := "9 => (Invalid, T9'ACCESS),";
      Expected_19 : constant String := "10 => (Three, null));";
   begin
      Parse_This(T, "token One = 'a' 'b' 'c';");
      Parse_This(T, "token Two = 'a' 'q' 'z';");
      Parse_This(T, "token Three = 'e' 'f' 'g';");
      T.Generator.Initialize(T.Parser'UNCHECKED_ACCESS, +"My_Package");
      kv.apg.lexgen.Source_Code_States(T.Generator.Get_States, T.Generator.Get_Tokens, T.Buff);
      Test_Line(T, 1, "   T1 : aliased constant Transition_List_Type := ("&Expected_1&");"); -- {97=>2,97=>5,101=>8}
      Test_Line(T, 2, "   T2 : aliased constant Transition_List_Type := ("&Expected_2&");"); -- {98=>3}
      Test_Line(T, 3, "   T3 : aliased constant Transition_List_Type := ("&Expected_3&");"); -- {99=>4}
      -- Transition 4 is empty and skipped
      Test_Line(T, 4, "   T5 : aliased constant Transition_List_Type := ("&Expected_5&");"); -- {113=>6}
      Test_Line(T, 5, "   T6 : aliased constant Transition_List_Type := ("&Expected_6&");"); -- {122=>7}
      -- Transition 7 is empty and skipped
      Test_Line(T, 6, "   T8 : aliased constant Transition_List_Type := ("&Expected_8&");"); -- {102=>9}
      Test_Line(T, 7, "   T9 : aliased constant Transition_List_Type := ("&Expected_9&");"); -- {103=>10}
      -- Transition 10 is empty and skipped
      Test_Line(T, 8, "   State_List : aliased constant State_List_Type := (");
      Test_Line(T, 9, "   " & Expected_10);
      Test_Line(T, 10, "   " & Expected_11);
      Test_Line(T, 11, "   " & Expected_12);
      Test_Line(T, 12, "   " & Expected_13);
      Test_Line(T, 13, "   " & Expected_14);
      Test_Line(T, 14, "   " & Expected_15);
      Test_Line(T, 15, "   " & Expected_16);
      Test_Line(T, 16, "   " & Expected_17);
      Test_Line(T, 17, "   " & Expected_18);
      Test_Line(T, 18, "   " & Expected_19);
      Test_Line(T, 19, "   Nfa_Definition : aliased constant Nfa_Class := (Start => 1, States => State_List'ACCESS);");
   end Run;


   ----------------------------------------------------------------------------
   type Lexgen_Insert_State_List_Test is new Base_Lexgen_Test_Class with null record;
   procedure Run(T : in out Lexgen_Insert_State_List_Test) is
   begin
      Parse_This(T, "token One = 'a' 'b' 'c';");
      Parse_This(T, "token Two = 'a' 'q' 'z';");
      Parse_This(T, "token Three = 'e' 'f' 'g';");
      T.Generator.Initialize(T.Parser'UNCHECKED_ACCESS, +"My_Package");
      T.Generator.Insert_State_List(T.Buff);
      Test_Line(T, 19, "   Nfa_Definition : aliased constant Nfa_Class := (Start => 1, States => State_List'ACCESS);");
   end Run;

   ----------------------------------------------------------------------------
   type Lexgen_With_Set_Test is new Base_Lexgen_Test_Class with null record;
   procedure Run(T : in out Lexgen_With_Set_Test) is
   begin
      Parse_This(T, "set package_name = ""My_Package"";");
      Parse_This(T, "token One = 'a' 'b' 'c';");
      Parse_This(T, "token Two = 'a' 'q' 'z';");
      Parse_This(T, "token Three = 'e' 'f' 'g';");
      T.Assert(T.Parser.Error_Count = 0, "Parser error detected!");
      T.Generator.Initialize(T.Parser'UNCHECKED_ACCESS, +"My_Package");
      T.Generator.Insert_State_List(T.Buff);
      Test_Line(T, 19, "   Nfa_Definition : aliased constant Nfa_Class := (Start => 1, States => State_List'ACCESS);");
   end Run;


   ----------------------------------------------------------------------------
   type Base_Config_Test_Class is abstract new Parser_Test_Class with
      record
         -- part of base class: Lexer : kv.apg.lex.Lexer_Class;
         -- part of base class: Parser : aliased kv.apg.parse.Parser_Class;
         Config : kv.apg.config.Key_Value_Map_Class;
      end record;

   ----------------------------------------------------------------------------
   type Basic_Config_Test is new Base_Config_Test_Class with null record;
   procedure Run(T : in out Basic_Config_Test) is
   begin
      Parse_This(T, "set package_name = ""My_Package"";");
      Parse_This(T, "token One = 'a' 'b' 'c';");
      T.Assert(T.Parser.Error_Count = 0, "Parser error detected!");
      T.Config.Initialize(T.Parser'UNCHECKED_ACCESS);
      T.Assert(not T.Config.Has_Key("foo_bar"), "Has_Key(foo_bar) should be False!");
      T.Assert(    T.Config.Has_Key("package_name"), "Has_Key(package_name) should be True!");
      T.Assert(T.Config.Get_Value("package_name") = "My_Package", "Has_Value(package_name) should be My_Package, was <"&T.Config.Get_Value("package_name")&">!");
   end Run;


   ----------------------------------------------------------------------------
   procedure register(suite : in kv.core.ut.Suite_Pointer_Type) is
   begin
      suite.register(new Lexgen_Count_Tokens_Test, "Lexgen_Count_Tokens_Test");
      suite.register(new Lexgen_Package_Name_Test, "Lexgen_Package_Name_Test");
      suite.register(new Lexgen_One_Token_Recognizer_Test, "Lexgen_One_Token_Recognizer_Test");
      suite.register(new Lexgen_Token_Type_Test, "Lexgen_Token_Type_Test");
      suite.register(new Lexgen_State_List_Test, "Lexgen_State_List_Test");
      suite.register(new Lexgen_State_List_Source_Code_Test, "Lexgen_State_List_Source_Code_Test");
      suite.register(new Lexgen_Insert_State_List_Test, "Lexgen_Insert_State_List_Test");
      suite.register(new Lexgen_With_Set_Test, "Lexgen_With_Set_Test");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");

      suite.register(new Basic_Config_Test, "Basic_Config_Test");
   end register;

end kv.apg.tests.lex_gen;
