
with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.locations;

package kv.apg.tokens is

   type Token_Type is (A_Word, A_Symbol, A_Char, A_String, A_Block, A_Comment, A_Special);

   type Token_Class is tagged private;

   procedure Initialize
      (Self  : in out Token_Class;
       Kind  : in     Token_Type;
       Where : in     kv.apg.locations.File_Location_Type;
       Data  : in     String_Type);

   function Get_Kind(Self : Token_Class) return Token_Type;
   function Get_Line(Self : Token_Class) return Positive;
   function Get_Location(Self : Token_Class) return kv.apg.locations.File_Location_Type;
   function Get_Data(Self : Token_Class) return String_Type;
   function Get_Data_As_String(Self : Token_Class) return String;
   function Is_Eos(Self : Token_Class) return Boolean;

   function "="(L, R : Token_Class) return Boolean;

   -- A few special tokens
   function Invalid_Token return Token_Class;
   function End_Of_File_Token return Token_Class;
   function Epsilon_Token return Token_Class;
   function Meta_Start_Rule_Token return Token_Class;

private

   type Token_Class is tagged
      record
         Kind     : Token_Type;
         Location : kv.apg.locations.File_Location_Type;
         Data     : String_Type;
      end record;

end kv.apg.tokens;
