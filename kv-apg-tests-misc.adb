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
with kv.apg.locations;
with kv.apg.incidents;
with kv.apg.writer.console;
with kv.apg.logger.writer;

package body kv.apg.tests.misc is

   use Ada.Characters.Conversions;
   use Ada.Strings.Wide_Wide_Unbounded;
   use kv.core.wwstr;


   ----------------------------------------------------------------------------
   type Buffer_Writer_Test_Class is abstract new kv.core.ut.Test_Class with
      record
         Buff : aliased kv.apg.writer.buffer.Buffer_Writer_Class;
      end record;

   ----------------------------------------------------------------------------
   procedure Test_Line
      (T      : in out Buffer_Writer_Test_Class'CLASS;
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
   type Write_Line_Buffer_Writer_Test is new Buffer_Writer_Test_Class with null record;
   procedure Run(T : in out Write_Line_Buffer_Writer_Test) is
   begin
      T.Buff.Write_Line("This is a test");
      T.Assert(T.Buff.Line_Count = 1, "Should have 1 line");
      T.Assert(T.Buff.Get_Line(1) = To_String_Type("This is a test"), "Line 1 does not match");
   end Run;

   ----------------------------------------------------------------------------
   type Write_Some_Buffer_Writer_Test is new Buffer_Writer_Test_Class with null record;
   procedure Run(T : in out Write_Some_Buffer_Writer_Test) is
   begin
      T.Buff.Write_Some("This ");
      T.Buff.Write_Some(      "is a");
      T.Buff.Write_Some(          " test");
      T.Assert(T.Buff.Line_Count = 0, "Should have 0 lines (line never completed)");
      T.Buff.New_Line;
      T.Assert(T.Buff.Line_Count = 1, "Should have 1 line");
      T.Assert(T.Buff.Get_Line(1) = To_String_Type("This is a test"), "Line 1 does not match");
   end Run;

   ----------------------------------------------------------------------------
   type Mix_Line_Buffer_Writer_Test is new Buffer_Writer_Test_Class with null record;
   procedure Run(T : in out Mix_Line_Buffer_Writer_Test) is
   begin
      T.Buff.Write_Some("This ");
      T.Buff.Write_Line(      "is a test");
      T.Assert(T.Buff.Line_Count = 1, "Should have 1 line");
      T.Buff.New_Line(2);
      T.Assert(T.Buff.Line_Count = 3, "Should have 3 line");
      T.Assert(T.Buff.Get_Line(1) = To_String_Type("This is a test"), "Line 1 does not match");
   end Run;


   ----------------------------------------------------------------------------
   type Enum_Test_Class is abstract new kv.core.ut.Test_Class with
      record
         Enum : kv.apg.enum.Enumeration_Class;
         Buff : kv.apg.writer.buffer.Buffer_Writer_Class;
      end record;

   ----------------------------------------------------------------------------
   type Init_Enum_Test is new Enum_Test_Class with null record;
   procedure Run(T : in out Init_Enum_Test) is
      use kv.apg.enum;
   begin
      T.Enum.Initialize(+"Enum_Type");
      T.Assert(T.Enum.Get_Count = 0, "Should have 0 enumerations");
   end Run;

   ----------------------------------------------------------------------------
   type Append_Count_Enum_Test is new Enum_Test_Class with null record;
   procedure Run(T : in out Append_Count_Enum_Test) is
      use kv.apg.enum;
   begin
      T.Enum.Initialize(+"Enum_Type");
      T.Enum.Append(+"one");
      T.Enum.Append(+"two");
      T.Enum.Append(+"three");
      T.Assert(T.Enum.Get_Count = 3, "Should have 3 enumerations");
   end Run;

   ----------------------------------------------------------------------------
   type Write_Enum_Test is new Enum_Test_Class with null record;
   procedure Run(T : in out Write_Enum_Test) is
      use kv.apg.enum;
   begin
      T.Enum.Initialize(+"Enum_Type");
      T.Enum.Append(+"Alpha");
      T.Enum.Append(+"Beta");
      T.Enum.Append(+"Gamma");
      T.Enum.Write(T.Buff);
      T.Assert(T.Buff.Line_Count = 4, "Should have 4 line");
      T.Assert(T.Buff.Get_Line(1) = To_String_Type("   type Enum_Type is"), "Should be type dec, is '" & To_UTF(+T.Buff.Get_Line(1)) & "'");
      T.Assert(T.Buff.Get_Line(2) = To_String_Type("      (Alpha,"), "Should be Alpha, is '" & To_UTF(+T.Buff.Get_Line(2)) & "'");
      T.Assert(T.Buff.Get_Line(3) = To_String_Type("       Beta,"), "Should be Alpha, is '" & To_UTF(+T.Buff.Get_Line(3)) & "'");
      T.Assert(T.Buff.Get_Line(4) = To_String_Type("       Gamma);"), "Should be Alpha, is '" & To_UTF(+T.Buff.Get_Line(4)) & "'");
   end Run;


   ----------------------------------------------------------------------------
   type Get_Enum_Test is new Enum_Test_Class with null record;
   procedure Run(T : in out Get_Enum_Test) is
      use kv.apg.enum;
   begin
      T.Enum.Initialize(+"Enum_Type");
      T.Enum.Append(+"Alpha");
      T.Enum.Append(+"Beta");
      T.Enum.Append(+"Gamma");
      T.Enum.Append(+"Epsilon");
      T.Assert(T.Enum.Get(3) = +"Gamma", "Get should return Gamma, got <" & To_UTF(+T.Enum.Get(3)) & ">");
   end Run;




   --<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   package foo_to_bar is
      type Foo_Bar_Class is new kv.apg.rewriter.Text_Converter_Class with null record;
      overriding function Convert
         (Self      : in out Foo_Bar_Class;
          Prefix    : in     String_Type;
          Postfix   : in     String_Type;
          Template  : in     String_Type) return kv.apg.writer.buffer.Buffer_Class'CLASS;
   end foo_to_bar;
   package body foo_to_bar is
      overriding function Convert
         (Self      : in out Foo_Bar_Class;
          Prefix    : in     String_Type;
          Postfix   : in     String_Type;
          Template  : in     String_Type) return kv.apg.writer.buffer.Buffer_Class'CLASS is
         Answer : kv.apg.writer.buffer.Buffer_Writer_Class;
         Indent : Natural;
         function Spaces(Indent : Natural) return String is
            Answer : constant String(1..Indent) := (others => ' ');
         begin
            return Answer;
         end Spaces;
      begin
         Answer.Write_Some(Prefix);
         if Template = +"Multi" then
            Answer.Write_Line("One");
            Answer.Write_Line("Two");
            Answer.Write_Some("Three");
         elsif Template = +"RecursiveMulti" then
            Indent := Length(Prefix);
            Answer.Write_Line("1");
            Answer.Write_Line(Spaces(Indent)&"«Foo»");
            Answer.Write_Line(Spaces(Indent)&"«3»");
            Answer.Write_Some(Spaces(Indent)&"4");
         elsif Template = +"Foo" then
            Answer.Write_Some("Bar");
         elsif Template = +"3" then
            Answer.Write_Some("«Foo»3«Unk»");
         else
            Answer.Write_Some("Zing");
         end if;
         Answer.Write_Line(Postfix);
         return kv.apg.writer.buffer.Buffer_Class'CLASS(Answer);
      end Convert;
   end foo_to_bar;
   -->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


   ----------------------------------------------------------------------------
   type Rewriter_No_Change_Test is new Buffer_Writer_Test_Class with null record;
   procedure Run(T : in out Rewriter_No_Change_Test) is
      Cnv : foo_to_bar.Foo_Bar_Class;
      Rew : kv.apg.rewriter.Rewriter_Class;
      Buf : kv.apg.writer.buffer.Buffer_Writer_Class;
      Line_1 : constant String := "This is a test";
   begin
      Buf.Write_Line(Line_1);
      Rew.Apply(Buf, Cnv, T.Buff);
      Test_Line(T, 1, Line_1);
   end Run;

   ----------------------------------------------------------------------------
   type Rewriter_Change_Test is new Buffer_Writer_Test_Class with null record;
   procedure Run(T : in out Rewriter_Change_Test) is
      Cnv : foo_to_bar.Foo_Bar_Class;
      Rew : kv.apg.rewriter.Rewriter_Class;
      Buf : kv.apg.writer.buffer.Buffer_Writer_Class;
      Line_1_o : constant String := "This is a test";
      Line_2_o : constant String := "Change «Foo» into Bar";
      Line_3_o : constant String := "And change «Unk» into Zing.";
      Line_2_e : constant String := "Change Bar into Bar";
      Line_3_e : constant String := "And change Zing into Zing.";
   begin
      Buf.Write_Line(Line_1_o);
      Buf.Write_Line(Line_2_o);
      Buf.Write_Line(Line_3_o);
      Rew.Apply(Buf, Cnv, T.Buff);
      Test_Line(T, 1, Line_1_o);
      Test_Line(T, 2, Line_2_e);
      Test_Line(T, 3, Line_3_e);
   end Run;

   ----------------------------------------------------------------------------
   type Rewriter_Recursive_Test is new Buffer_Writer_Test_Class with null record;
   procedure Run(T : in out Rewriter_Recursive_Test) is
      Cnv : foo_to_bar.Foo_Bar_Class;
      Rew : kv.apg.rewriter.Rewriter_Class;
      Buf : kv.apg.writer.buffer.Buffer_Writer_Class;
      Line_1 : constant String := "This is a «Foo» test of «Unk» and «3»";
      Expected : constant String := "This is a Bar test of Zing and Bar3Zing";
   begin
      Buf.Write_Line(Line_1);
      Rew.Apply(Buf, Cnv, T.Buff);
      Test_Line(T, 1, Expected);
   end Run;

   ----------------------------------------------------------------------------
   type Rewriter_Multi_Change_Test is new Buffer_Writer_Test_Class with null record;
   procedure Run(T : in out Rewriter_Multi_Change_Test) is
      Cnv : foo_to_bar.Foo_Bar_Class;
      Rew : kv.apg.rewriter.Rewriter_Class;
      Buf : kv.apg.writer.buffer.Buffer_Writer_Class;
      Line_1_o : constant String := "This is a test";
      Line_2_o : constant String := "«Multi»";
      Line_3_o : constant String := "And change «Unk» into Zing.";
      Line_2_e : constant String := "One";
      Line_5_e : constant String := "And change Zing into Zing.";
   begin
      Buf.Write_Line(Line_1_o);
      Buf.Write_Line(Line_2_o);
      Buf.Write_Line(Line_3_o);
      Rew.Apply(Buf, Cnv, T.Buff);
      Test_Line(T, 1, Line_1_o);
      Test_Line(T, 2, Line_2_e);
      Test_Line(T, 5, Line_5_e);
   end Run;

   ----------------------------------------------------------------------------
   type Rewriter_Recursive_Multi_Change_Test is new Buffer_Writer_Test_Class with null record;
   procedure Run(T : in out Rewriter_Recursive_Multi_Change_Test) is
      Cnv : foo_to_bar.Foo_Bar_Class;
      Rew : kv.apg.rewriter.Rewriter_Class;
      Buf : kv.apg.writer.buffer.Buffer_Writer_Class;
      Line_1_t : constant String := "This is a «RecursiveMulti» test";
      Line_1_e : constant String := "This is a 1";
      Line_2_e : constant String := "          Bar";
      Line_3_e : constant String := "          Bar3Zing";
      Line_4_e : constant String := "          4 test";
   begin
      Buf.Write_Line(Line_1_t);
      Rew.Apply(Buf, Cnv, T.Buff);
      Test_Line(T, 1, Line_1_e);
      Test_Line(T, 2, Line_2_e);
      Test_Line(T, 3, Line_3_e);
      Test_Line(T, 4, Line_4_e);
   end Run;

   ----------------------------------------------------------------------------
   type Rewriter_Helper_Test is new Buffer_Writer_Test_Class with null record;
   procedure Run(T : in out Rewriter_Helper_Test) is
      a : constant String := "This is a ";
      b : constant String :=            "Foo";
      c : constant String :=                " test";
      Line_1 : constant String := a & "«" & b & "»" & c;
      Temp_Line : kv.apg.rewriter.Template_Line_Class;
   begin
      Temp_Line.Initialize(To_String_Type(Line_1));
      T.Assert(Temp_Line.Has_Template, "Expected Has_Template true");
      T.Assert(Temp_Line.Get_Before = To_String_Type(a), "Expected Get_Before='"&a&"', got '"&To_UTF(+Temp_Line.Get_Before)&"'.");
      T.Assert(Temp_Line.Get_Template = To_String_Type(b), "Expected Get_Template='"&b&"', got '"&To_UTF(+Temp_Line.Get_Template)&"'.");
      T.Assert(Temp_Line.Get_After = To_String_Type(c), "Expected Get_After='"&c&"', got '"&To_UTF(+Temp_Line.Get_After)&"'.");
      T.Assert(Temp_Line.Get_All = To_String_Type(Line_1), "Expected Get_All='"&Line_1&"', got '"&To_UTF(+Temp_Line.Get_All)&"'.");
   end Run;




   ----------------------------------------------------------------------------
   type Location_Test_Class is abstract new Buffer_Writer_Test_Class with
      record
         Factory : aliased kv.apg.locations.File_Location_Factory_Class;
      end record;

   ----------------------------------------------------------------------------
   overriding procedure Set_Up(T : in out Location_Test_Class) is
   begin
      T.Factory.Initialize("foo");
   end Set_Up;

   ----------------------------------------------------------------------------
   type New_Location_Test is new Location_Test_Class with null record;
   procedure Run(T : in out New_Location_Test) is
      Where : kv.apg.locations.Location_Type;
      Expected : constant String := "File: foo, line 1, column 13";
      use kv.apg.locations;
   begin
      Where := T.Factory.New_Location(Line => 1, Column => 13);
      T.Assert(Image(Where) = Expected, "Image should be <"&Expected&">, was <"&Image(Where)&">.");
   end Run;



   ----------------------------------------------------------------------------
   type Incident_Test_Class is abstract new Location_Test_Class with
      record
         Reporter : aliased kv.apg.incidents.Writer_Report_Class;
      end record;

   ----------------------------------------------------------------------------
   type New_Incident_Test is new Incident_Test_Class with null record;
   procedure Run(T : in out New_Incident_Test) is
      Where : kv.apg.locations.Location_Type;
      What  : kv.apg.incidents.Incident_Class(kv.apg.incidents.Warning);
      Expected : constant String := "WARNING (File: foo, line 1, column 13): because (""citation"").";
   begin
      Where := T.Factory.New_Location(Line => 1, Column => 13);
      What.Initialize(Where, To_String_Type("citation"), To_String_Type("because"));
      T.Assert(What.Image = To_String_Type(Expected), "What.Image should be <"&Expected&">, but is <"&To_String(What.Image)&">.");
   end Run;

   ----------------------------------------------------------------------------
   type Report_Incident_Test is new Incident_Test_Class with null record;
   procedure Run(T : in out Report_Incident_Test) is
      Where : kv.apg.locations.Location_Type;
      What  : kv.apg.incidents.Incident_Class(kv.apg.incidents.Warning);
      Expected : constant String := "WARNING (File: foo, line 1, column 13): because (""citation"").";
   begin
      Where := T.Factory.New_Location(Line => 1, Column => 13);
      What.Initialize(Where, To_String_Type("citation"), To_String_Type("because"));
      T.Reporter.Initialize(T.Buff'UNCHECKED_ACCESS);
      T.Reporter.Note(What);
      Test_Line(T, 1, Expected);
   end Run;

   ----------------------------------------------------------------------------
   type Filter_Test is new Incident_Test_Class with null record;
   procedure Run(T : in out Filter_Test) is
      Where  : kv.apg.locations.Location_Type;
      What_0 : kv.apg.incidents.Incident_Class(kv.apg.incidents.Debug);
      What_1 : kv.apg.incidents.Incident_Class(kv.apg.incidents.Detail);
      What_2 : kv.apg.incidents.Incident_Class(kv.apg.incidents.Information);
      What_3 : kv.apg.incidents.Incident_Class(kv.apg.incidents.Warning);
      What_4 : kv.apg.incidents.Incident_Class(kv.apg.incidents.Error);
   begin
      Where := T.Factory.New_Location(Line => 1, Column => 13);
      What_0.Initialize(Where, To_String_Type("citation"), To_String_Type("Debug"));
      What_1.Initialize(Where, To_String_Type("citation"), To_String_Type("Detail"));
      What_2.Initialize(Where, To_String_Type("citation"), To_String_Type("Information"));
      What_3.Initialize(Where, To_String_Type("citation"), To_String_Type("Warning"));
      What_4.Initialize(Where, To_String_Type("citation"), To_String_Type("Error"));
      T.Reporter.Initialize(T.Buff'UNCHECKED_ACCESS);
      T.Reporter.Note(What_0);
      T.Assert(T.Buff.Line_Count = 1, "Line count should have been 1 (pre-level)");
      T.Reporter.Set_Filter_Level(kv.apg.incidents.Warning);
      T.Reporter.Note(What_0);
      T.Assert(T.Buff.Line_Count = 1, "Line count should still be 1 (level at warning, debug excluded)");
      T.Reporter.Note(What_1);
      T.Assert(T.Buff.Line_Count = 1, "Line count should still be 1 (level at warning, detail excluded)");
      T.Reporter.Note(What_2);
      T.Assert(T.Buff.Line_Count = 1, "Line count should still be 1 (level at warning, info excluded)");
      T.Reporter.Note(What_3);
      T.Assert(T.Buff.Line_Count = 2, "Line count should be 2 (level at warning, warning accepted)");
      T.Reporter.Note(What_4);
      T.Assert(T.Buff.Line_Count = 3, "Line count should be 3 (level at warning, error accepted)");
   end Run;



   ----------------------------------------------------------------------------
   type Logger_Test_Class is abstract new Incident_Test_Class with
      record
         Logger : aliased kv.apg.logger.writer.Writer_Logger_Class;
      end record;

   ----------------------------------------------------------------------------
   type Logger_Init_Test is new Logger_Test_Class with null record;
   procedure Run(T : in out Logger_Init_Test) is
      Citation : String_Type := To_String_Type("token");
      Expected : constant String := "INFORMATION (File: bar, line 2, column 17): processed (""token"").";
   begin
      T.Logger.Initialize("bar", T.Buff'UNCHECKED_ACCESS, kv.apg.incidents.Debug);
      T.Logger.Note_Info(2, 17, Citation, "processed");
      Test_Line(T, 1, Expected);
   end Run;


   ----------------------------------------------------------------------------
   procedure register(suite : in kv.core.ut.Suite_Pointer_Type) is
   begin
      suite.register(new Write_Line_Buffer_Writer_Test, "Write_Line_Buffer_Writer_Test");
      suite.register(new Write_Some_Buffer_Writer_Test, "Write_Some_Buffer_Writer_Test");
      suite.register(new Mix_Line_Buffer_Writer_Test, "Mix_Line_Buffer_Writer_Test");

      suite.register(new Init_Enum_Test, "Init_Enum_Test");
      suite.register(new Append_Count_Enum_Test, "Append_Count_Enum_Test");
      suite.register(new Write_Enum_Test, "Write_Enum_Test");
      suite.register(new Get_Enum_Test, "Get_Enum_Test");

      suite.register(new Rewriter_No_Change_Test, "Rewriter_No_Change_Test");
      suite.register(new Rewriter_Change_Test, "Rewriter_Change_Test");
      suite.register(new Rewriter_Recursive_Test, "Rewriter_Recursive_Test");
      suite.register(new Rewriter_Multi_Change_Test, "Rewriter_Multi_Change_Test");
      suite.register(new Rewriter_Recursive_Multi_Change_Test, "Rewriter_Recursive_Multi_Change_Test");
      suite.register(new Rewriter_Helper_Test, "Rewriter_Helper_Test");

      suite.register(new New_Location_Test, "New_Location_Test");

      suite.register(new New_Incident_Test, "New_Incident_Test");
      suite.register(new Report_Incident_Test, "Report_Incident_Test");
      suite.register(new Filter_Test, "Filter_Test");

      suite.register(new Logger_Init_Test, "Logger_Init_Test");
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");
   end register;

end kv.apg.tests.misc;
