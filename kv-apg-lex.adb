with Ada.Text_IO; use Ada.Text_IO;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;

package body kv.apg.lex is

   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Ada.Characters.Conversions;

   Quotation : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Quotation);
   Comment : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Number_Sign);
   Apostrophe : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Apostrophe);
   Open_Block : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Left_Angle_Quotation);
   Close_Block : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Right_Angle_Quotation);
   Underscore : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Low_Line);
   Line_Feed : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.LF);

   ----------------------------------------------------------------------------
--   function "+"(S : String) return String_Type is
--      WS : constant Wide_Wide_String := To_Wide_Wide_String(S);
--   begin
--      return +WS;
--   end "+";

   ----------------------------------------------------------------------------
--   function "+"(S : Wide_Wide_String) return String_Type renames Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String;

   ----------------------------------------------------------------------------
--   function "+"(U : String_Type) return Wide_Wide_String renames Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String;

   ----------------------------------------------------------------------------
   function Character_State(Next : Wide_Wide_Character) return Lex_State_Type is
   begin
      if Is_Alphanumeric(Next) or Next = Underscore then
         return In_Word;
      elsif Next = Quotation then
         return In_String;
      elsif Next = Comment then
         return In_Comment;
      elsif Next = Apostrophe then
         return In_Char;
      elsif Next = Open_Block then
         return In_Block;
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

      procedure Start_New_Token_If is
      begin
         if Self.State /= Between then
            Self.Begin_Token(Next);
         end if;
      end Start_New_Token_If;

   begin
      case Self.State is
         when Between =>
            Self.State := Tentative_State;
            Start_New_Token_If;
         when In_Word =>
            if Tentative_State = In_Word then
               Self.Accumulate_Character(Next);
            else
               Self.Complete_Token(kv.apg.tokens.A_Word);
               Self.State := Tentative_State;
            end if;
         when In_Symbol =>
            if Tentative_State = In_Symbol then -- Multi-character symbol
               Self.Accumulate_Character(Next);
            else
               Self.Complete_Token(kv.apg.tokens.A_Symbol);
               Self.State := Tentative_State;
               Start_New_Token_If;
            end if;
         when In_Char =>
            if Next = Apostrophe then
               Self.State := Between;
               Self.Complete_Token(kv.apg.tokens.A_Char);
            else
               Self.Reset_Accumulator;
               Self.Accumulate_Character(Next);
            end if;
         when In_String =>
            if Next = Quotation then
               Self.State := Between;
               Self.Complete_Token(kv.apg.tokens.A_String);
            else
               Self.Accumulate_Character(Next);
            end if;
         when In_Block =>
            if Next = Close_Block then
               Self.State := Between;
               Self.Complete_Token(kv.apg.tokens.A_Block);
            else
               Self.Accumulate_Character(Next);
            end if;
         when In_Comment =>
            if Is_Line_Terminator(Next) then
               Self.State := Between;
               Self.Complete_Token(kv.apg.tokens.A_Comment);
            else
               Self.Accumulate_Character(Next);
            end if;
      end case;
      --Put_Line("State = " & Lex_State_Type'IMAGE(Self.State));
      if Next = Line_Feed then
         Self.Line := Self.Line + 1;
      end if;
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
      return Natural(Self.List.Length);
   end Tokens_Available;

   ----------------------------------------------------------------------------
   function Get_Next_Token
      (Self : in out Lexer_Class) return kv.apg.tokens.Token_Class is
      Token : kv.apg.tokens.Token_Class;
   begin
      Token := Self.List.First_Element;
      Self.List.Delete_First;
      return Token;
   end Get_Next_Token;

   Keep_First_Character : constant array (Lex_State_Type) of Boolean := (In_Word => True, In_Symbol => True, others => False);

   ----------------------------------------------------------------------------
   procedure Begin_Token
      (Self : in out Lexer_Class;
       Next : in     Wide_Wide_Character) is
   begin
      Self.Reset_Accumulator;
      if Keep_First_Character(Self.State) then
         Self.Accumulate_Character(Next);
      end if;
      Self.Where := Self.Line;
   end Begin_Token;

   ----------------------------------------------------------------------------
   procedure Complete_Token
      (Self : in out Lexer_Class;
       Kind : in     kv.apg.tokens.Token_Type) is
      Token : kv.apg.tokens.Token_Class;
   begin
      Token.Initialize(Kind, Self.Where, Self.Accum);
      Self.List.Append(Token);
   end Complete_Token;

   ----------------------------------------------------------------------------
   procedure Accumulate_Character
      (Self : in out Lexer_Class;
       Next : in     Wide_Wide_Character) is
   begin
      --Put_Line("Adding '"&To_Character(Next)&"' to '"&To_String(+Self.Accum)&"'.");
      Self.Accum := Self.Accum & Next;
      --Put_Line("Self.Accum is now '"&To_String(+Self.Accum)&"'.");
   end Accumulate_Character;

   ----------------------------------------------------------------------------
   procedure Reset_Accumulator
      (Self : in out Lexer_Class) is
   begin
      --Put_Line("Resetting Self.Accum.");
      Self.Accum := Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String;
   end Reset_Accumulator;

end kv.apg.lex;
