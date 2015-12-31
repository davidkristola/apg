with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Characters.Conversions;

with kv.core.wwstr;

package body kv.apg.tokens is

   use Ada.Characters.Conversions;
   use Ada.Strings.Wide_Wide_Unbounded;
   use kv.core.wwstr;

   Eos : constant String_Type := +";";


   ----------------------------------------------------------------------------
   procedure Initialize
      (Self  : in out Token_Class;
       Kind  : in     Token_Type;
       Where : in     kv.apg.locations.File_Location_Type;
       Data  : in     String_Type) is
   begin
      Self.Kind := Kind;
      Self.Data := Data;
      Self.Location := Where;
   end Initialize;

   ----------------------------------------------------------------------------
   function Get_Kind(Self : Token_Class) return Token_Type is
   begin
      return Self.Kind;
   end Get_Kind;

   ----------------------------------------------------------------------------
   function Get_Line(Self : Token_Class) return Positive is
   begin
      return Self.Location.Get_Line;
   end Get_Line;

   ----------------------------------------------------------------------------
   function Get_Location(Self : Token_Class) return kv.apg.locations.File_Location_Type is
   begin
      return Self.Location;
   end Get_Location;

   ----------------------------------------------------------------------------
   function Get_Data(Self : Token_Class) return String_Type is
   begin
      return Self.Data;
   end Get_Data;

   ----------------------------------------------------------------------------
   function Get_Data_As_String(Self : Token_Class) return String is
   begin
      return To_UTF(+Self.Data);
   end Get_Data_As_String;

   ----------------------------------------------------------------------------
   function Is_Eos(Self : Token_Class) return Boolean is
   begin
      return (Self.Kind = A_Symbol) and then (Self.data = +Eos);
   end Is_Eos;

   ----------------------------------------------------------------------------
   function "="(L, R : Token_Class) return Boolean is
      use kv.apg.locations; -- "="
   begin
      return (L.Kind = R.Kind) and then (L.Location = R.Location) and then (L.data = R.Data);
   end "=";

   ----------------------------------------------------------------------------
   function Invalid_Token return Token_Class is
      Here : kv.apg.locations.File_Location_Type;
      Invalid : Token_Class;
   begin
      Here.Initialize(To_String_Type("n/a"), 0, 0);
      Invalid.Initialize(A_Special, Here, To_String_Type("Invalid"));
      return Invalid;
   end Invalid_Token;

   ----------------------------------------------------------------------------
   function End_Of_File_Token return Token_Class is
      Here : kv.apg.locations.File_Location_Type;
      T : Token_Class;
   begin
      Here.Initialize(To_String_Type("n/a"), 0, 0);
      T.Initialize(A_Special, Here, To_String_Type("End_Of_File"));
      return T;
   end End_Of_File_Token;

   ----------------------------------------------------------------------------
   function Epsilon_Token return Token_Class is
      Here : kv.apg.locations.File_Location_Type;
      T : Token_Class;
   begin
      Here.Initialize(To_String_Type("n/a"), 0, 0);
      T.Initialize(A_Special, Here, To_String_Type("Epsilon"));
      return T;
   end Epsilon_Token;

   ----------------------------------------------------------------------------
   function Meta_Start_Rule_Token return Token_Class is
      Here : kv.apg.locations.File_Location_Type;
      T : Token_Class;
   begin
      Here.Initialize(To_String_Type("n/a"), 0, 0);
      T.Initialize(A_Special, Here, To_String_Type("Meta_Start_Rule"));
      return T;
   end Meta_Start_Rule_Token;

end kv.apg.tokens;
