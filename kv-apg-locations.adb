package body kv.apg.locations is

   procedure Initialize
      (Self   : in out Location_Type;
       File   : in     String_Type;
       Line   : in     Natural;
       Column : in     Natural) is
   begin
      Self.File := File;
      Self.Line := Line;
      Self.Column := Column;
   end Initialize;

   function Image(Self : Location_Type) return String is
   begin
      return "File: " & To_String(Self.File) & ", line" & Natural'IMAGE(Self.Line) & ", column" & Natural'IMAGE(Self.Column);
   end Image;

   not overriding procedure Initialize
      (Self : in out File_Location_Factory_Class;
       File : in     String) is
   begin
      Self.File := To_String_type(File);
   end Initialize;

   overriding function New_Location
      (Self   : in     File_Location_Factory_Class;
       Line   : in     Natural;
       Column : in     Natural) return Location_Type is
      Location : Location_Type;
   begin
      Initialize(Location, Self.File, Line => Line, Column => Column);
      return Location;
   end New_Location;

end kv.apg.locations;
