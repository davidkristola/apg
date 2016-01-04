with kv.core.wwstr; use kv.core.wwstr;

package kv.apg.locations is

   type Location_Type is tagged private;

   procedure Initialize
      (Self   : in out Location_Type;
       Line   : in     Natural;
       Column : in     Natural);

   function Image(Self : Location_Type) return String;
   function Cite(Self : Location_Type; Additional : String := "") return String;

   procedure Next_Line(Self : in out Location_Type);
   procedure Next_Column(Self : in out Location_Type);
   function Get_Line(Self : Location_Type) return Natural;
   function Get_Column(Self : Location_Type) return Natural;
   function "="(L, R : Location_Type) return Boolean;


   type File_Location_Type is new Location_Type with private;

   not overriding procedure Initialize
      (Self   : in out File_Location_Type;
       File   : in     String_Type;
       Line   : in     Natural;
       Column : in     Natural);

   overriding function Image(Self : File_Location_Type) return String;
   overriding function "="(L, R : File_Location_Type) return Boolean;

private

   type Location_Type is tagged
      record
         Line   : Natural := 1;
         Column : Natural := 1;
      end record;

   type File_Location_Type is new Location_Type with
      record
         File : String_Type;
      end record;

end kv.apg.locations;
