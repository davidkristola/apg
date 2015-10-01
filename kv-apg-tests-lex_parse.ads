with kv.apg.lex;
with kv.apg.parse;
with kv.apg.tests.lex_lex;

package kv.apg.tests.lex_parse is

   ----------------------------------------------------------------------------
   type Parser_Test_Class is abstract new kv.apg.tests.lex_lex.Lexer_Test_Class with
      record
         Parser : aliased kv.apg.parse.Parser_Class;
      end record;

   overriding procedure Tear_Down(T : in out Parser_Test_Class);
   procedure Parse_This(T : in out Parser_Test_Class'CLASS; S : in String);
   procedure Check_States(T : in out Parser_Test_Class'CLASS; Errors : in Natural; Directives : in Natural);

   procedure register(suite : in kv.core.ut.Suite_Pointer_Type);

end kv.apg.tests.lex_parse;
