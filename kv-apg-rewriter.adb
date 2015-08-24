
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

      use kv.apg.writer.buffer;

      Working     : Template_Line_Class;
      Replacement : String_Type;
      Empty       : kv.apg.writer.buffer.Buffer_Writer_Class;
      Temp        : kv.apg.writer.buffer.Buffer_Writer_Class;

      function Transfer(Multi_Lines : kv.apg.writer.buffer.Buffer_Class'CLASS) return Buffer_Writer_Class is
         T1 : Buffer_Writer_Class;
         T2 : Buffer_Writer_Class;
         W  : Template_Line_Class;
         R  : Boolean := False;
      begin
         Temp := Empty;
         for I in 1 .. Multi_Lines.Line_Count loop
            W.Initialize(Multi_Lines.Get_Line(I));
            if W.Has_Template then
               R := True;
            end if;
            T1.Write_Line(Multi_Lines.Get_Line(I));
         end loop;
         if R then
            Self.Apply(T1, Converter, T2);
            return T2;
         else
            return T1;
         end if;
      end Transfer;

   begin
      for I in 1..Source.Line_Count loop
         Working.Initialize(Source.Get_Line(I));
         if Working.Has_Template then
            Temp := Transfer(Converter.Convert
               (Prefix => Working.Get_Before,
                Postfix => Working.Get_After,
                Template => Working.Get_Template));
            for I in 1 .. Temp.Line_Count loop
               Destination.Write_Line(Temp.Get_Line(I));
            end loop;
         else
            Destination.Write_Line(Working.Get_All);
         end if;
--         while Working.Has_Template loop
--            Transfer(Converter.Convert
--               (Prefix => Working.Get_Before,
--                Postfix => Working.Get_After,
--                Template => Working.Get_Template));
--            for I in 1 .. Temp.Line_Count - 1 loop
--               Destination.Write_Line(Temp.Get_Line(I));
--            end loop;
--            Working.Initialize(Temp.Get_Line(Temp.Line_Count));
--         end loop;
--         Destination.Write_Line(Working.Get_All);
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
