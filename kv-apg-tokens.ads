
with Ada.Strings.Wide_Wide_Unbounded;

package kv.apg.tokens is
   procedure yo;

   type Token_Type is (A_Word, A_Symbol, A_Char, A_String, A_Block, A_Comment);

   type Token_Class is tagged private;

   procedure Initialize
      (Self : in out Token_Class;
       Kind : in     Token_Type;
       Line : in     Positive;
       Data : in     Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String);

   function Get_Kind(Self : Token_Class) return Token_Type;
   function Get_Line(Self : Token_Class) return Positive;
   function Get_Data(Self : Token_Class) return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
   function Get_Data_As_String(Self : Token_Class) return String;
   function Is_Eos(Self : Token_Class) return Boolean;

   function "="(L, R : Token_Class) return Boolean;

private

   type Token_Class is tagged
      record
         Kind : Token_Type;
         Line : Positive;
         Data : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      end record;

end kv.apg.tokens;
