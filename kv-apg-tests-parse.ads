with kv.apg.lex;
with kv.apg.parse;
with kv.apg.logger.writer;
with kv.apg.writer.buffer;

with kv.apg.tests.lex_lex;
with kv.apg.tests.lex_parse;

package kv.apg.tests.parse is

   ----------------------------------------------------------------------------
   type Rule_Test_Class is abstract new kv.apg.tests.lex_parse.Parser_Test_Class with
      record
         Buffer : aliased kv.apg.writer.buffer.Buffer_Writer_Class;
         Logger : aliased kv.apg.logger.writer.Writer_Logger_Class;
      end record;

   procedure Test_Line
      (T      : in out Rule_Test_Class;
       Number : in     Positive;
       Line   : in     String);


   procedure register(suite : in kv.core.ut.Suite_Pointer_Type);

end kv.apg.tests.parse;
