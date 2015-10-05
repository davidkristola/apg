with kv.core.wwstr; use kv.core.wwstr;

package kv.apg.locations is

   type Location_Type is private;

   procedure Initialize
      (Self   : in out Location_Type;
       File   : in     String_Type;
       Line   : in     Natural;
       Column : in     Natural);

   function Image(Self : Location_Type) return String;

   --TODO: add getters if needed





   type Location_Factory_Class is interface;
   function New_Location
      (Self   : in     Location_Factory_Class;
       Line   : in     Natural;
       Column : in     Natural) return Location_Type is abstract;


   -- Concrete factory encapsulating a pre-determined file name
   type File_Location_Factory_Class is new Location_Factory_Class with private;
   not overriding procedure Initialize
      (Self : in out File_Location_Factory_Class;
       File : in     String);
   overriding function New_Location
      (Self   : in     File_Location_Factory_Class;
       Line   : in     Natural;
       Column : in     Natural) return Location_Type;


private

   type Location_Type is
      record
         File   : String_Type;
         Line   : Natural := 0;
         Column : Natural := 0;
      end record;


   type File_Location_Factory_Class is new Location_Factory_Class with
      record
         File : String_Type;
      end record;

end kv.apg.locations;
