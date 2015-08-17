
with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.writer.buffer;

package kv.apg.rewriter is

   type Text_Converter_Class is abstract tagged null record;
   procedure Convert
      (Self      : in out Text_Converter_Class;
       Original  : in     String_Type;
       Converted :    out String_Type) is abstract;

   type Rewriter_Class is tagged null record;
   procedure Apply
      (Self        : in out Rewriter_Class;
       Source      : in     kv.apg.writer.buffer.Buffer_Writer_Class;
       Converter   : in out Text_Converter_Class'CLASS;
       Destination : in out kv.apg.writer.buffer.Buffer_Writer_Class);

   -- Helper class
   type Template_Line_Class is tagged limited private;
   procedure Initialize
      (Self : in out Template_Line_Class;
       Line : in     String_Type);
   function Has_Template(Self : Template_Line_Class) return Boolean;
   function Get_Before(Self : Template_Line_Class) return String_Type;
   function Get_Template(Self : Template_Line_Class) return String_Type;
   function Get_After(Self : Template_Line_Class) return String_Type;
   function Get_All(Self : Template_Line_Class) return String_Type;

private

   type Template_Line_Class is tagged limited
      record
         Line           : String_Type;
         Template_Start : Natural;
         Template_End   : Natural;
      end record;

end kv.apg.rewriter;
