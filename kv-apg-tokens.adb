with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Characters.Conversions;

with kv.core.wwstr;

package body kv.apg.tokens is
   procedure yo is
   begin
      Put_Line("yo");
   end yo;

   use Ada.Characters.Conversions;
   use Ada.Strings.Wide_Wide_Unbounded;
   use kv.core.wwstr;

   Eos : constant String_Type := +";";


   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Token_Class;
       Kind : in     Token_Type;
       Line : in     Positive;
       Data : in     Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String) is
   begin
      --Put_Line("Token " & Token_Type'IMAGE(Kind) & " @ " & Positive'IMAGE(Line) & ", value = '"&To_String(Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String(Data))&"'.");
      Self.Kind := Kind;
      Self.Line := Line;
      Self.Data := Data;
   end Initialize;

   ----------------------------------------------------------------------------
   function Get_Kind(Self : Token_Class) return Token_Type is
   begin
      return Self.Kind;
   end Get_Kind;

   ----------------------------------------------------------------------------
   function Get_Line(Self : Token_Class) return Positive is
   begin
      return Self.Line;
   end Get_Line;

   ----------------------------------------------------------------------------
   function Get_Data(Self : Token_Class) return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String is
   begin
      return Self.Data;
   end Get_Data;

   ----------------------------------------------------------------------------
   function Get_Data_As_String(Self : Token_Class) return String is
   begin
      return To_String(+Self.Data); -- This could lose information
   end Get_Data_As_String;

   ----------------------------------------------------------------------------
   function Is_Eos(Self : Token_Class) return Boolean is
   begin
      return (Self.Kind = A_Symbol) and then (Self.data = +Eos);
   end Is_Eos;

   ----------------------------------------------------------------------------
   function "="(L, R : Token_Class) return Boolean is
      use Ada.Strings.Wide_Wide_Unbounded; -- "="
   begin
      return (L.Kind = R.Kind) and then (L.Line = R.Line) and then (L.data = R.Data);
   end "=";

end kv.apg.tokens;
