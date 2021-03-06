with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;

package body kv.apg.lex is

   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Ada.Characters.Conversions;

   Debug : Boolean := False;

   -------------------------------------------------------------------------
   procedure Set_Debug(Value : in Boolean) is
   begin
      Debug := Value;
   end Set_Debug;


   Quotation   : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Quotation);
   Comment     : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Number_Sign);
   Apostrophe  : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Apostrophe);
   Open_Block  : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Left_Angle_Quotation);
   Close_Block : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Right_Angle_Quotation);
   Underscore  : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Low_Line);
   Line_Feed   : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.LF);
   Eos         : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Semicolon);
   Open_Paren  : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Left_Parenthesis);
   Close_Paren : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Right_Parenthesis);

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
   function Is_Mono_Symbol(Next : Wide_Wide_Character) return Boolean is
   begin
      --TODO: use a map or something
      if Next = Eos or else Next = Open_Paren or else Next = Close_Paren then
         return True;
      end if;
      return False;
   end Is_Mono_Symbol;


   ----------------------------------------------------------------------------
   procedure Start_File
      (Self : in out Lexer_Class;
       File : in     String_Type) is
   begin
      Self.File := File;
      Self.Current_Position.Initialize(File, 1, 1);
   end Start_File;


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
               Self.Complete_Word;
               Self.State := Tentative_State;
               Start_New_Token_If;
            end if;
         when In_Symbol =>
            if Tentative_State = In_Symbol and then not Is_Mono_Symbol(Next) then -- Multi-character symbol
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
      if Debug then Put_Line("State = " & Lex_State_Type'IMAGE(Self.State)); end if;
      if Is_Line_Terminator(Next) then
         Self.Current_Position.Next_Line;
      else
         Self.Current_Position.Next_Column;
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
      return Natural(Self.Tokens.Length);
   end Tokens_Available;

   ----------------------------------------------------------------------------
   function Get_Next_Token
      (Self : in out Lexer_Class) return kv.apg.tokens.Token_Class is
      Token : kv.apg.tokens.Token_Class;
   begin
      Token := Self.Tokens.First_Element;
      Self.Tokens.Delete_First;
      return Token;
   end Get_Next_Token;

   ----------------------------------------------------------------------------
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
      Self.Start_Of_Token := Self.Current_Position;
      if Is_Mono_Symbol(Next) then
         if Debug then Put_Line("Auto-completing mono-symbol " & To_Character(Next)); end if;
         Self.Complete_Token(kv.apg.tokens.A_Symbol);
      end if;
   end Begin_Token;

   ----------------------------------------------------------------------------
   function Is_Number(Sample : String_Type) return Boolean is
      C : Wide_Wide_Character;
   begin
      for I in 1 .. Length(Sample) loop
         C := Element(Sample, I);
         if not Is_Decimal_Digit(C) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Number;

   ----------------------------------------------------------------------------
   procedure Complete_Word
      (Self : in out Lexer_Class) is
   begin
      if Is_Number(Self.Accumulator) then
         Self.Complete_Token(kv.apg.tokens.A_Number);
      else
         Self.Complete_Token(kv.apg.tokens.A_Word);
      end if;
   end Complete_Word;

   ----------------------------------------------------------------------------
   procedure Complete_Token
      (Self : in out Lexer_Class;
       Kind : in     kv.apg.tokens.Token_Type) is
      Token : kv.apg.tokens.Token_Class;
   begin
      if Debug then Put_Line("Completing token " & kv.apg.tokens.Token_Type'IMAGE(Kind)); end if;
      Token.Initialize(Kind, Self.Start_Of_Token, Self.Accumulator);
      Self.Tokens.Append(Token);
      Self.State := Between;
   end Complete_Token;

   ----------------------------------------------------------------------------
   procedure Accumulate_Character
      (Self : in out Lexer_Class;
       Next : in     Wide_Wide_Character) is
   begin
      if Debug then Put_Line("Adding '"&To_Character(Next)&"' to '"&To_String(+Self.Accumulator)&"'."); end if;
      Self.Accumulator := Self.Accumulator & Next;
      if Debug then Put_Line("Self.Accumulator is now '"&To_String(+Self.Accumulator)&"'."); end if;
   end Accumulate_Character;

   ----------------------------------------------------------------------------
   procedure Reset_Accumulator
      (Self : in out Lexer_Class) is
   begin
      Self.Accumulator := Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String;
   end Reset_Accumulator;

end kv.apg.lex;
