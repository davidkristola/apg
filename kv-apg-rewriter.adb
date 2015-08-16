
with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Wide_Wide_Unbounded;
use Ada.Strings.Wide_Wide_Unbounded;

with Ada.Text_IO; use Ada.Text_IO;

package body kv.apg.rewriter is

   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Ada.Characters.Conversions;

   ----------------------------------------------------------------------------
   function To_S(S : Wide_Wide_String) return String_Type is
   begin
      return ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String(S);
   end To_S;


  procedure Apply
      (Self        : in out Rewriter_Class;
       Source      : in     kv.apg.writer.buffer.Buffer_Writer_Class;
       Converter   : in out Text_Converter_Class'CLASS;
       Destination : in out kv.apg.writer.buffer.Buffer_Writer_Class) is

      Working : String_Type;
      Replace_Start : Natural;
      Replace_End : Natural;
      Extract : String_Type;
      Replacement : String_Type;

   begin
      for I in 1..Source.Line_Count loop
         Working := Source.Get_Line(I);
         Replace_Start := Index(Working, "«");
         while Replace_Start /= 0 loop
            Replace_End := Index(Working, "»");
            Extract := To_S(Slice(Working, Replace_Start+2, Replace_End-1));
            --Put_Line("Extract="&To_UTF(+Extract));
            --Put_Line("Pre="&To_UTF(Slice(Working, 1, Replace_Start-1)));
            --Put_Line("Post="&To_UTF(Slice(Working, Replace_End+2, Length(Working))));
            Converter.Convert(Extract, Replacement);
            --Put_Line("Replacement="&To_UTF(+Replacement));
            Working := To_S(Slice(Working, 1, Replace_Start-1)) & Replacement & To_S(Slice(Working, Replace_End+2, Length(Working)));
            Replace_Start := Index(Working, "«");
         end loop;
         Destination.Write_Line(Working);
      end loop;
   end Apply;

end kv.apg.rewriter;
