
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

      -------------------------------------------------------------------------
      -- Postcondition: the returned buffer is clean
      function Recursive_Transform
         (Lines : Buffer_Class'CLASS) return Buffer_Writer_Class is

         Initial_Copy : Buffer_Writer_Class;
         Clean_Copy : Buffer_Writer_Class;
         Possible_Template : Template_Line_Class;
         Needs_More_Processing : Boolean := False;

      begin
         for I in 1 .. Lines.Line_Count loop
            Possible_Template.Initialize(Lines.Get_Line(I));
            if Possible_Template.Has_Template then
               Needs_More_Processing := True;
            end if;
            Initial_Copy.Write_Line(Lines.Get_Line(I));
         end loop;
         if Needs_More_Processing then
            Self.Apply(Initial_Copy, Converter, Clean_Copy);
            return Clean_Copy;
         else
            return Initial_Copy;
         end if;
      end Recursive_Transform;

      -------------------------------------------------------------------------
      function Expand_Template(Working : Template_Line_Class) return Buffer_Writer_Class is
      begin
         return Recursive_Transform
            (Converter.Convert
               (Prefix => Working.Get_Before,
                Postfix => Working.Get_After,
                Template => Working.Get_Template));
      end Expand_Template;

      -------------------------------------------------------------------------
      -- Precondition: line is clean (has no unresolved templates)
      procedure Copy_Line_To_Destination(Line : in     String_Type) is
      begin
         Destination.Write_Line(Line);
      end Copy_Line_To_Destination;

      -------------------------------------------------------------------------
      -- Precondition: all lines are clean (has no unresolved templates)
      procedure Copy_All_Lines_To_Destination(Lines : in     Buffer_Writer_Class) is
      begin
         for I in 1 .. Lines.Line_Count loop
            Copy_Line_To_Destination(Lines.Get_Line(I));
         end loop;
      end Copy_All_Lines_To_Destination;

      -------------------------------------------------------------------------
      procedure Apply_Line(Line : in     String_Type) is
         Possible_Template : Template_Line_Class;
      begin
         Possible_Template.Initialize(Line);
         if Possible_Template.Has_Template then
            Copy_All_Lines_To_Destination(Expand_Template(Possible_Template));
         else
            Copy_Line_To_Destination(Line);
         end if;
      end Apply_Line;

   begin
      for I in 1..Source.Line_Count loop
         Apply_Line(Source.Get_Line(I));
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
