with Ada.Text_IO;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;

package body kv.apg.lex is

   use Ada.Text_IO;
   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Ada.Characters.Conversions;

   Quotation : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Quotation);
   Comment : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Number_Sign);
   Apostrophe : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Apostrophe);
   Open_Block : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Left_Angle_Quotation);
   Close_Block : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Right_Angle_Quotation);

   function "+"(S : String) return String_Type is
      WS : constant Wide_Wide_String := To_Wide_Wide_String(S);
   begin
      return +WS;
   end "+";

   function "+"(S : Wide_Wide_String) return String_Type renames Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String;
   function "+"(U : String_Type) return Wide_Wide_String renames Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String;

   ----------------------------------------------------------------------------
   function Character_State(Next : Wide_Wide_Character) return Lex_State_Type is
   begin
      if Is_Alphanumeric(Next) then
         return In_Word;
      elsif Next = Quotation then
         return In_String;
      elsif Next = Comment then
         return In_Comment;
      elsif Next = Apostrophe then
         return In_Char;
      elsif Is_Line_Terminator(Next) or Is_Space(Next) then
         return Between;
      end if;
      return In_Symbol;
   end Character_State;

   ----------------------------------------------------------------------------
   procedure Ingest_Character
      (Self : in out Lexer_Class;
       Next : in     Wide_Wide_Character) is
      Tentative_State : Lex_State_Type := Character_State(Next);
   begin
      case Self.State is
         when Between =>
            Self.State := Tentative_State;
            if Self.State /= Between then
               Self.Begin_Token(Next);
            end if;
         when In_Word =>
            if Is_Alphanumeric(Next) then
               Self.Accum := Self.Accum & Next; -- keep accumulating
            else
               Self.Complete_Token;
               Self.State := Tentative_State;
            end if;
         when In_Symbol =>
            if Tentative_State = In_Symbol then -- Multi-character symbol
               Self.Accum := Self.Accum & Next; -- keep accumulating
            else
               Self.Complete_Token;
               Self.State := Tentative_State;
            end if;
            if Self.State /= Between then
               Self.Begin_Token(Next);
            end if;
         when In_Char =>
            if Next = Apostrophe then
               Self.State := Between;
               Self.Complete_Token;
            else
               Self.Accum := Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String & Next;
            end if;
         when In_String =>
            if Next = Quotation then
               Self.State := Between;
               Self.Complete_Token;
            else
               Self.Accum := Self.Accum & Next; -- keep accumulating
            end if;
         when In_Block =>
            Self.State := Between;
         when In_Comment =>
            if Is_Line_Terminator(Next) then
               Self.State := Between;
               Self.Complete_Token;
            end if;
      end case;
      --Put_Line("State = " & Lex_State_Type'IMAGE(Self.State));
   end Ingest_Character;

   ----------------------------------------------------------------------------
   function Inbetween_Tokens
      (Self : in     Lexer_Class) return Boolean is
   begin
      return (Self.State = Between);
   end Inbetween_Tokens;

   ----------------------------------------------------------------------------
   function Tokens_Available
      (Self : in     Lexer_Class) return Natural is
   begin
      return Self.Count; --TODO
   end Tokens_Available;

   ----------------------------------------------------------------------------
   procedure Begin_Token
      (Self : in out Lexer_Class;
       Next : in     Wide_Wide_Character) is
   begin
      Self.Accum := Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String & Next;
      --TODO
   end Begin_Token;

   ----------------------------------------------------------------------------
   procedure Complete_Token
      (Self : in out Lexer_Class) is
   begin
      Self.Count := Self.Count + 1;
      --TODO
   end Complete_Token;

end kv.apg.lex;
