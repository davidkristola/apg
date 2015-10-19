with Ada.Strings.Wide_Wide_Unbounded;

package body kv.apg.locations is

   -------------------------------------------------------------------------
   procedure Initialize
      (Self   : in out Location_Type;
       Line   : in     Natural;
       Column : in     Natural) is
   begin
      Self.Line := Line;
      Self.Column := Column;
   end Initialize;

   -------------------------------------------------------------------------
   function Image(Self : Location_Type) return String is
   begin
      return "line" & Natural'IMAGE(Self.Line) & ", column" & Natural'IMAGE(Self.Column);
   end Image;

   -------------------------------------------------------------------------
   procedure Next_Line(Self : in out Location_Type) is
   begin
      Self.Line := Self.Line + 1;
      Self.Column := 1;
   end Next_Line;

   -------------------------------------------------------------------------
   procedure Next_Column(Self : in out Location_Type) is
   begin
      Self.Column := Self.Column + 1;
   end Next_Column;

   -------------------------------------------------------------------------
   function Get_Line(Self : Location_Type) return Natural is
   begin
      return Self.Line;
   end Get_Line;

   -------------------------------------------------------------------------
   function Get_Column(Self : Location_Type) return Natural is
   begin
      return Self.Column;
   end Get_Column;

   -------------------------------------------------------------------------
   function "="(L, R : Location_Type) return Boolean is
   begin
      return (L.Line = R.Line) and (L.Column = R.Column);
   end "=";

   -------------------------------------------------------------------------
   not overriding procedure Initialize
      (Self   : in out File_Location_Type;
       File   : in     String_Type;
       Line   : in     Natural;
       Column : in     Natural) is
   begin
      Self.File := File;
      Self.Line := Line;
      Self.Column := Column;
   end Initialize;

   -------------------------------------------------------------------------
   overriding function Image(Self : File_Location_Type) return String is
   begin
      return "File: " & To_String(Self.File) & ", line" & Natural'IMAGE(Self.Line) & ", column" & Natural'IMAGE(Self.Column);
   end Image;

   -------------------------------------------------------------------------
   overriding function "="(L, R : File_Location_Type) return Boolean is
      use Ada.Strings.Wide_Wide_Unbounded; -- "="
   begin
      return (L.File = R.File) and ((L.Line = R.Line) and (L.Column = R.Column));
   end "=";

end kv.apg.locations;
