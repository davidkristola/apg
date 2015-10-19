
private with Ada.Containers.Doubly_Linked_Lists;

with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.tokens;
with kv.apg.locations;

package kv.apg.lex is

   type Lexer_Class is tagged private;

   procedure Ingest_Character
      (Self : in out Lexer_Class;
       Next : in     Wide_Wide_Character);

   function Inbetween_Tokens
      (Self : in     Lexer_Class) return Boolean;

   function Tokens_Available
      (Self : in     Lexer_Class) return Natural;

   function Get_Next_Token
      (Self : in out Lexer_Class) return kv.apg.tokens.Token_Class;


   -- Control debug output of package
   procedure Set_Debug(Value : in Boolean);

private

   use kv.apg.tokens;

   type Lex_State_Type is (Between, In_Word, In_Symbol, In_Char, In_String, In_Block, In_Comment);

   package Token_List is new Ada.Containers.Doubly_Linked_Lists(kv.apg.tokens.Token_Class);

   type Lexer_Class is tagged
      record
         State : Lex_State_Type := Between;
         Where : kv.apg.locations.Location_Type;
         Line  : kv.apg.locations.Location_Type;
         Accum : String_Type;
         List  : Token_List.List;
      end record;

   procedure Begin_Token
      (Self : in out Lexer_Class;
       Next : in     Wide_Wide_Character);

   procedure Complete_Token
      (Self : in out Lexer_Class;
       Kind : in     kv.apg.tokens.Token_Type);

   procedure Accumulate_Character
      (Self : in out Lexer_Class;
       Next : in     Wide_Wide_Character);

   procedure Reset_Accumulator
      (Self : in out Lexer_Class);

end kv.apg.lex;
