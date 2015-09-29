with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Characters.Conversions;
with Ada.Characters.Latin_1;

with lex_test_1.Lexer;

procedure lex_test_1_driver is

   use lex_test_1;

   Lexer  : lex_test_1.Lexer.Nfa_State_Class;
   Theory : lex_test_1.Token_Type := Invalid;

   ----------------------------------------------------------------------------
   procedure Take_Theory is
   begin
      Put_Line(lex_test_1.Token_Type'IMAGE(Theory));
      Theory := Invalid;
   end Take_Theory;

   ----------------------------------------------------------------------------
   procedure Ingest_Line(Line : in     String) is
      use Ada.Characters.Conversions;
      WS : constant Wide_Wide_String := To_Wide_Wide_String(Line & Ada.Characters.Latin_1.LF);
   begin
      for WC of WS loop
         Lexer.Ingest(WC);
         if Lexer.Is_Failed then
            Take_Theory;
            Lexer.Reset;
            Lexer.Ingest(WC);
         end if;
         if Lexer.Is_Accepting then
            Theory := Lexer.Get_Token;
         end if;
         if Lexer.Is_Terminal then
            Take_Theory;
            Lexer.Reset;
         end if;
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
   procedure Process_Input_File(File_Name : in     String) is
   begin
      Ingest_File(File_Name);
   end Process_Input_File;

begin
   Put_Line("running lex_test_1_driver");
   Lexer.Initialize;
   Process_Input_File(Argument(1));
end lex_test_1_driver;
