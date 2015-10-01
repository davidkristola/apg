with kv.apg.lex;

package kv.apg.tests.lex_lex is

   ----------------------------------------------------------------------------
   type Lexer_Test_Class is abstract new kv.core.ut.Test_Class with
      record
         Lexer : kv.apg.lex.Lexer_Class;
      end record;

   procedure Ingest_All(T : in out Lexer_Test_Class'CLASS; S : in String);

   procedure register(suite : in kv.core.ut.Suite_Pointer_Type);

end kv.apg.tests.lex_lex;
