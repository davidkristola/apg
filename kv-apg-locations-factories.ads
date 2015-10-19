
package kv.apg.locations.factories is

   type Location_Factory_Class is interface;
   function New_Location
      (Self   : in     Location_Factory_Class;
       Line   : in     Natural;
       Column : in     Natural) return File_Location_Type is abstract;


   -- Concrete factory encapsulating a pre-determined file name
   type File_Location_Factory_Class is new Location_Factory_Class with private;
   not overriding procedure Initialize
      (Self : in out File_Location_Factory_Class;
       File : in     String);
   overriding function New_Location
      (Self   : in     File_Location_Factory_Class;
       Line   : in     Natural;
       Column : in     Natural) return File_Location_Type;


private

   type File_Location_Factory_Class is new Location_Factory_Class with
      record
         File : String_Type;
      end record;

end kv.apg.locations.factories;
