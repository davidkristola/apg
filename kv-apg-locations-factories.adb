
package body kv.apg.locations.factories is

   not overriding procedure Initialize
      (Self : in out File_Location_Factory_Class;
       File : in     String) is
   begin
      Self.File := To_String_type(File);
   end Initialize;

   overriding function New_Location
      (Self   : in     File_Location_Factory_Class;
       Line   : in     Natural;
       Column : in     Natural) return File_Location_Type is
      Location : File_Location_Type;
   begin
      Initialize(Location, Self.File, Line => Line, Column => Column);
      return Location;
   end New_Location;

end kv.apg.locations.factories;
