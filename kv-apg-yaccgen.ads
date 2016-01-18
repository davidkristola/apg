
with kv.core.wwstr; use kv.core.wwstr;
with kv.apg.parse;
with kv.apg.writer;
with kv.apg.rewriter;
with kv.apg.writer.buffer;
with kv.apg.enum;

package kv.apg.yaccgen is

   type Generator_Class is new kv.apg.rewriter.Text_Converter_Class with private;

   procedure Initialize
      (Self         : in out Generator_Class;
       Parser       : in     kv.apg.parse.Parser_Pointer_Type;
       Package_Name : in     String_Type);

   overriding function Convert
      (Self      : in out Generator_Class;
       Prefix    : in     String_Type;
       Postfix   : in     String_Type;
       Template  : in     String_Type) return kv.apg.writer.buffer.Buffer_Class'CLASS;

   -- Convert will call these routines based on the template:


private

   type Generator_Class is new kv.apg.rewriter.Text_Converter_Class with
      record
         Parser       : kv.apg.parse.Parser_Pointer_Type;
         Package_Name : String_Type;
         Tokens       : Natural;
         Token_Enum   : kv.apg.enum.Enumeration_Class;
      end record;

end kv.apg.yaccgen;
