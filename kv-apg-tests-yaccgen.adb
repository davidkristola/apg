with Ada.Characters.Conversions;
with Ada.Characters.Latin_1;
with Ada.Containers;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Tags; use Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Wide_Wide_Characters.Handling;

with kv.core.wwstr;

with kv.apg.enum;
with kv.apg.incidents;
with kv.apg.lex;
with kv.apg.logger.writer;
with kv.apg.parse;
with kv.apg.rules.engines;
with kv.apg.rules.grammars;
with kv.apg.tests.lex_lex;
with kv.apg.tests.lex_parse;
with kv.apg.tokens;
with kv.apg.writer.buffer;
with kv.apg.writer.console;
with kv.apg.yaccgen;

package body kv.apg.tests.yaccgen is

   use Ada.Characters.Conversions;
   use Ada.Containers;
   use Ada.Strings.Wide_Wide_Unbounded;
   use kv.apg.enum;
   use kv.apg.incidents;
   use kv.apg.lex;
   use kv.apg.logger.writer;
   use kv.apg.rules;
   use kv.apg.tokens;
   use kv.apg.writer.buffer;
   use kv.core.wwstr;

   --##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
   type Yacc_Test_Class is abstract new kv.apg.tests.lex_parse.Parser_Test_Class with
      record
         -- part of base class: Lexer : kv.apg.lex.Lexer_Class;
         -- part of base class: Parser : aliased kv.apg.parse.Parser_Class;
         Buffer  : aliased kv.apg.writer.buffer.Buffer_Writer_Class;
         Console : aliased kv.apg.writer.console.Console_Writer_Class;
         Logger  : aliased kv.apg.logger.writer.Writer_Logger_Class;
         CL      : aliased kv.apg.logger.writer.Writer_Logger_Class;

         Grammar : aliased kv.apg.rules.grammars.Grammar_Class;
         Enum    : aliased kv.apg.enum.Enumeration_Class;
         Engine  : aliased kv.apg.rules.engines.Parser_Engine_Class;

         Generator : aliased kv.apg.yaccgen.Generator_Class;
      end record;

   ----------------------------------------------------------------------------
   overriding procedure Set_Up(T : in out Yacc_Test_Class) is
   begin
      T.Logger.Initialize
         (Writer => T.Buffer'UNCHECKED_ACCESS,
          Level  => Debug);
      T.CL.Initialize
         (Writer => T.Console'UNCHECKED_ACCESS,
          Level  => Debug);
   end Set_Up;

   procedure register(suite : in kv.core.ut.Suite_Pointer_Type) is
   begin
   null;
--      suite.register(new XXX, "XXX");
--      suite.register(new XXX, "XXX");
   end register;

end kv.apg.tests.yaccgen;
