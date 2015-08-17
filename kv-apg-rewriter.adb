
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
  procedure Apply
      (Self        : in out Rewriter_Class;
       Source      : in     kv.apg.writer.buffer.Buffer_Writer_Class;
       Converter   : in out Text_Converter_Class'CLASS;
       Destination : in out kv.apg.writer.buffer.Buffer_Writer_Class) is

      Working     : Template_Line_Class;
      Replacement : String_Type;

   begin
      for I in 1..Source.Line_Count loop
         Working.Initialize(Source.Get_Line(I));
         while Working.Has_Template loop
            Converter.Convert(Working.Get_Template, Replacement);
            Working.Initialize(Working.Get_Before & Replacement & Working.Get_After);
         end loop;
         Destination.Write_Line(Working.Get_All);
      end loop;
   end Apply;


   ----------------------------------------------------------------------------
   function To_S(S : Wide_Wide_String) return String_Type is
   begin
      return ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String(S);
   end To_S;

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Template_Line_Class;
       Line : in     String_Type) is
   begin
      Self.Line := Line;
      Self.Template_Start := Index(Self.Line, "«");
      Self.Template_End := Index(Self.Line, "»");
   end Initialize;

   ----------------------------------------------------------------------------
   function Has_Template(Self : Template_Line_Class) return Boolean is
   begin
      return (Self.Template_Start /= 0) and (Self.Template_End /= 0);
   end Has_Template;

   ----------------------------------------------------------------------------
   function Get_Before(Self : Template_Line_Class) return String_Type is
   begin
      return To_S(Slice(Self.Line, 1, Self.Template_Start-1));
   end Get_Before;

   ----------------------------------------------------------------------------
   function Get_Template(Self : Template_Line_Class) return String_Type is
   begin
      return To_S(Slice(Self.Line, Self.Template_Start+2, Self.Template_End-1));
   end Get_Template;

   ----------------------------------------------------------------------------
   function Get_After(Self : Template_Line_Class) return String_Type is
   begin
      return To_S(Slice(Self.Line, Self.Template_End+2, Length(Self.Line)));
   end Get_After;

   ----------------------------------------------------------------------------
   function Get_All(Self : Template_Line_Class) return String_Type is
   begin
      return Self.Line;
   end Get_All;

end kv.apg.rewriter;
