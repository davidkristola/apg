with Ada.Text_IO; use Ada.Text_IO;

with my_lex_example.Lexer;

with my_lex_example.Static_Parser_Tables;

procedure my_example is
   L : my_lex_example.Lexer.Nfa_State_Class;
begin
   Put_Line("running my_example");
   L.Initialize;
end my_example;
