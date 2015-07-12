
with kv.apg.parse;
with kv.apg.enum;

package kv.apg.lexgen is

   -- The goal of this generator is to create a collection of Ada source files
   -- that will input a UTF-8 stream of text and output a stream of tokens.
   -- Tokens are represented as they are in this program.
   --
   type Generator_Class is tagged private;

   procedure Initialize
      (Self   : in out Generator_Class;
       Parser : in     kv.apg.parse.Parser_Pointer_Type);

   function Token_Count(Self : Generator_Class) return Natural;

private

   type Generator_Class is tagged
      record
         Parser : kv.apg.parse.Parser_Pointer_Type;
         Tokens : Natural;
      end record;

end kv.apg.lexgen;
