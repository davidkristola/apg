with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Characters.Conversions;
with Ada.Characters.Latin_1;

with kv.apg.lex;
with kv.apg.parse;

procedure apg is
   Lexer  : kv.apg.lex.Lexer_Class;
   Parser : kv.apg.parse.Parser_Class;

   procedure Ingest_Line(Line : in     String) is
      use Ada.Characters.Conversions;
      WS : constant Wide_Wide_String := To_Wide_Wide_String(Line & Ada.Characters.Latin_1.LF);
   begin
      for WC of WS loop
         Lexer.Ingest_Character(WC);
      end loop;
   end Ingest_Line;

   procedure Ingest_File(File_Name : in     String) is
      F : File_Type;
   begin
      Open(F, In_File, Argument(1));
      while not End_Of_File(F) loop
         Ingest_Line(Get_Line(F));
      end loop;
      Close(F);
   end Ingest_File;

   procedure Parse_Tokens is
   begin
      while Lexer.Tokens_Available > 0 loop
         Parser.Ingest_Token(Lexer.Get_Next_Token);
      end loop;
   end Parse_Tokens;

   procedure Process_Input_File(File_Name : in     String) is
   begin
      Ingest_File(File_Name);
      Parse_Tokens;
   end Process_Input_File;

begin
   -- Set default execution parameters
   -- Process command line arguments (if any/can we have none? default input file?)
   -- Process the input file (singular and required; standard input from a pipe?)
   Process_Input_File(Argument(1));
   -- Generate output files
end apg;
