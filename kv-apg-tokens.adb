with Ada.Text_IO;
use Ada.Text_IO;

package body kv.apg.tokens is
   procedure yo is
   begin
      Put_Line("yo");
   end yo;



   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Token_Class;
       Kind : in     Token_Type;
       Line : in     Positive;
       Data : in     Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String) is
   begin
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

end kv.apg.tokens;
