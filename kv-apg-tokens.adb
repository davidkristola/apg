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
      Invalid.Initialize(A_Word, Here, To_String_Type("Invalid"));
      return Invalid;
   end Invalid_Token;

end kv.apg.tokens;
