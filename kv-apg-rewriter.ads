
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

end kv.apg.rewriter;
