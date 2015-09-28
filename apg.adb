with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Characters.Conversions;
with Ada.Characters.Latin_1;

with kv.core.wwstr;

with kv.apg.lex;
with kv.apg.parse;
with kv.apg.lexgen;
with kv.apg.config;
with kv.apg.writer.buffer;
with kv.apg.rewriter;

procedure apg is

   use kv.core.wwstr;

   Lexer     : kv.apg.lex.Lexer_Class;
   Parser    : aliased kv.apg.parse.Parser_Class;
   Generator : kv.apg.lexgen.Generator_Class;
   Config    : kv.apg.config.Key_Value_Map_Class;

   ----------------------------------------------------------------------------
   procedure Ingest_Line(Line : in     String) is
      use Ada.Characters.Conversions;
      WS : constant Wide_Wide_String := To_Wide_Wide_String(Line & Ada.Characters.Latin_1.LF);
   begin
      for WC of WS loop
         Lexer.Ingest_Character(WC);
      end loop;
   end Ingest_Line;

   ----------------------------------------------------------------------------
   procedure Ingest_File(File_Name : in     String) is
      F : File_Type;
   begin
      Open(F, In_File, File_Name);
      while not End_Of_File(F) loop
         Ingest_Line(Get_Line(F));
      end loop;
      Close(F);
   end Ingest_File;

   ----------------------------------------------------------------------------
   procedure Buffer_File
      (File_Name : in     String;
       Buffer    : in out kv.apg.writer.buffer.Buffer_Writer_Class) is
      F : File_Type;
   begin
      Open(F, In_File, File_Name);
      while not End_Of_File(F) loop
         Buffer.Write_Line(Get_Line(F));
      end loop;
      Close(F);
   end Buffer_File;

   ----------------------------------------------------------------------------
   procedure Parse_Tokens is
   begin
      while Lexer.Tokens_Available > 0 loop
         Parser.Ingest_Token(Lexer.Get_Next_Token);
      end loop;
   end Parse_Tokens;

   ----------------------------------------------------------------------------
   procedure Process_Input_File(File_Name : in     String) is
   begin
      Ingest_File(File_Name);
      Parse_Tokens;
   end Process_Input_File;

   ----------------------------------------------------------------------------
   procedure Process_Config is
   begin
      Config.Initialize(Parser'UNCHECKED_ACCESS);
   end Process_Config;

   ----------------------------------------------------------------------------
   procedure Zip(Template_Name : in     String) is
      Template : kv.apg.writer.buffer.Buffer_Writer_Class;
      Source_Code : kv.apg.writer.buffer.Buffer_Writer_Class;
      Converter : kv.apg.rewriter.Rewriter_Class;
   begin
      Buffer_File(Template_Name, Template);
      Converter.Apply(Template, Generator, Source_Code);
      for I in 1 .. Source_Code.Line_Count loop
         Put_Line(To_UTF(+Source_Code.Get_Line(I)));
      end loop;
   end Zip;

   ----------------------------------------------------------------------------
   procedure Generate_Lex is
      Package_Name : constant String := Config.Get_Value("package_name");
--      Template : kv.apg.writer.buffer.Buffer_Writer_Class;
--      Source_Code : kv.apg.writer.buffer.Buffer_Writer_Class;
--      Converter : kv.apg.rewriter.Rewriter_Class;
   begin
      Generator.Initialize(Parser'UNCHECKED_ACCESS, To_String_Type(Package_Name));
      Zip("lextemplate.ads");
      Zip("lextemplate-static_nfa_definition.ads");
--      Buffer_File("lextemplate.ads", Template);
--      Converter.Apply(Template, Generator, Source_Code);
--      for I in 1 .. Source_Code.Line_Count loop
--         Put_Line(To_UTF(+Source_Code.Get_Line(I)));
--      end loop;
   end Generate_Lex;

begin
   -- Set default execution parameters
   -- Process command line arguments (if any/can we have none? default input file?)
   -- Process the input file (singular and required; standard input from a pipe?)
   Process_Input_File(Argument(1));
   Process_Config;
   -- Generate output files
   Generate_Lex;
end apg;
