
with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.writer.buffer;

package kv.apg.rewriter is

   Wrong_Convert_Error : exception;

   type Text_Converter_Class is interface;
   function Is_Multi_Line(Self : Text_Converter_Class; Original : String_Type) return Boolean is abstract;
   procedure Convert
      (Self      : in out Text_Converter_Class;
       Template  : in     String_Type;
       Converted :    out String_Type) is abstract;
   function Convert
      (Self      : in out Text_Converter_Class;
       Prefix    : in     String_Type;
       Postfix   : in     String_Type;
       Template  : in     String_Type) return kv.apg.writer.buffer.Buffer_Class'CLASS is abstract;

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
