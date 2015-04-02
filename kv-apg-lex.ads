
with Ada.Strings.Wide_Wide_Unbounded;

with kv.apg.tokens;

package kv.apg.lex is

   subtype String_Type is Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

   function "+"(S : String) return String_Type;
   function "+"(S : Wide_Wide_String) return String_Type;
   function "+"(U : String_Type) return Wide_Wide_String;

   type Lexer_Class is tagged private;

   procedure Ingest_Character
      (Self : in out Lexer_Class;
       Next : in     Wide_Wide_Character);

   function Inbetween_Tokens
      (Self : in     Lexer_Class) return Boolean;

   function Tokens_Available
      (Self : in     Lexer_Class) return Natural;

private

   type Lex_State_Type is (Between, In_Word, In_Symbol, In_Char, In_String, In_Block, In_Comment);

   type Lexer_Class is tagged
      record
         State : Lex_State_Type := Between;
         Count : Natural := 0;
         Accum : String_Type;
      end record;

   procedure Begin_Token
      (Self : in out Lexer_Class;
       Next : in     Wide_Wide_Character);

   procedure Complete_Token
      (Self : in out Lexer_Class);

end kv.apg.lex;
