with kv.apg.tests.lex_lex;
with kv.apg.tests.lex_parse;
with kv.apg.tests.lex_fast;
with kv.apg.tests.lex_xfa;
with kv.apg.tests.lex_gen;
with kv.apg.tests.misc;
with kv.apg.tests.parse;
with kv.apg.tests.yaccgen;

package body kv.apg.tests is

   ----------------------------------------------------------------------------
   procedure register(suite : in kv.core.ut.Suite_Pointer_Type) is
   begin
      kv.apg.tests.lex_lex.register(suite);
      kv.apg.tests.lex_parse.register(suite);
      kv.apg.tests.lex_fast.register(suite);
      kv.apg.tests.lex_xfa.register(suite);
      kv.apg.tests.lex_gen.register(suite);
      kv.apg.tests.misc.register(suite);
      kv.apg.tests.parse.register(suite);
      kv.apg.tests.yaccgen.register(suite);
   end register;

end kv.apg.tests;
