
with kv.core.wwstr; use kv.core.wwstr;
with kv.apg.parse;
with kv.apg.writer;
with kv.apg.rewriter;
with kv.apg.writer.buffer;
with kv.apg.enum;
with kv.apg.fast;
with kv.apg.fa.nfa;

package kv.apg.lexgen is

   -- The goal of this generator is to create a collection of Ada source files
   -- that will input a UTF-8 stream of text and output a stream of tokens.
   -- Tokens are represented as they are in this program.
   --
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
   not overriding procedure Insert_Package_Name -- "package_name"
      (Self   : in out Generator_Class;
       Buffer : in out kv.apg.writer.buffer.Buffer_Writer_Class);

   not overriding procedure Insert_Token_Type -- "token_type"
      (Self   : in out Generator_Class;
       Buffer : in out kv.apg.writer.buffer.Buffer_Writer_Class);

   not overriding function Get_Cnfa(Self : Generator_Class) return kv.apg.fa.nfa.Nfa_Class;
   not overriding function Get_States(Self : Generator_Class) return kv.apg.fast.State_List_Type;
   not overriding function Get_Tokens(Self : Generator_Class) return kv.apg.enum.Enumeration_Class;

   procedure Source_Code_States
      (States : in     kv.apg.fast.State_List_Type;
       Tokens : in     kv.apg.enum.Enumeration_Class;
       Buffer : in out kv.apg.writer.buffer.Buffer_Writer_Class);



   function Token_Count(Self : Generator_Class) return Natural;

   procedure Write_Spec
      (Self   : in     Generator_Class;
       Writer : in out kv.apg.writer.Writer_Class'CLASS);

   procedure Write_Body
      (Self   : in     Generator_Class;
       Writer : in out kv.apg.writer.Writer_Class'CLASS);

private

   type Generator_Class is new kv.apg.rewriter.Text_Converter_Class with
      record
         Parser       : kv.apg.parse.Parser_Pointer_Type;
         Package_Name : String_Type;
         Tokens       : Natural;
         Token_Enum   : kv.apg.enum.Enumeration_Class;
         Cnfa         : kv.apg.fa.nfa.Nfa_Class;
      end record;

end kv.apg.lexgen;
