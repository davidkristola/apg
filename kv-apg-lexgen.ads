
with kv.core.wwstr; use kv.core.wwstr;
with kv.apg.parse;
with kv.apg.writer;

package kv.apg.lexgen is

   -- The goal of this generator is to create a collection of Ada source files
   -- that will input a UTF-8 stream of text and output a stream of tokens.
   -- Tokens are represented as they are in this program.
   --
   type Generator_Class is tagged private;

   procedure Initialize
      (Self            : in out Generator_Class;
       Parser          : in     kv.apg.parse.Parser_Pointer_Type;
       Package_Name    : in     String_Type;
       Token_Enum_Name : in     String_Type;
       FA_Name         : in     String_Type);

   function Token_Count(Self : Generator_Class) return Natural;

   procedure Write_Spec
      (Self   : in     Generator_Class;
       Writer : in out kv.apg.writer.Writer_Class'CLASS);

   procedure Write_Body
      (Self   : in     Generator_Class;
       Writer : in out kv.apg.writer.Writer_Class'CLASS);

private

   type Generator_Class is tagged
      record
         Parser : kv.apg.parse.Parser_Pointer_Type;
         Package_Name : String_Type;
         Token_Enum_Name : String_Type;
         FA_Name : String_Type;
         Tokens : Natural;
      end record;

end kv.apg.lexgen;
