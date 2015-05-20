with Ada.Text_IO;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Deallocation;

with kv.apg.tokens;
with kv.apg.lex;
with kv.core.wwstr;

package body kv.apg.parse.token is

   use Ada.Text_IO;
   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Ada.Characters.Conversions;
   use kv.apg.tokens;
   use kv.apg.regex;
   use kv.apg.lex;
   use kv.core.wwstr;

   -------------------------------------------------------------------------
   procedure Expect_Name
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Word then
         Self.Name_Token := Token;
         Self.Expect := Equal;
      else
         Self.Status := Done_Error;
      end if;
   end Expect_Name;

   -------------------------------------------------------------------------
   procedure Expect_Equal
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Symbol and then To_String(+Token.Get_Data) = "=" then
         Self.Expect := Value;
      else
         Self.Status := Done_Error;
      end if;
   end Expect_Equal;

   -------------------------------------------------------------------------
   function Is_Valid_Token(Token : kv.apg.tokens.Token_Class) return Boolean is
   begin
      if Token.Get_Kind = A_Char or else Token.Get_Kind = A_String or else Token.Get_Kind = A_Block then
         return True; -- This is a constant
      end if;
      if Token.Get_Data_As_String = "|" or else -- TODO: this list is an oblique duplicate of regex
         Token.Get_Data_As_String = "." or else
         Token.Get_Data_As_String = "*" or else
         Token.Get_Data_As_String = "+" or else
         Token.Get_Data_As_String = "?" or else
         Token.Get_Data_As_String = "(" or else
         Token.Get_Data_As_String = ")"
      then
         return True; -- Valid symbol
      end if;
      return False;
   end Is_Valid_Token;

   -------------------------------------------------------------------------
   procedure Expect_Value
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Is_Valid_Token(Token) then
         Self.Tree.Graft_To_Tree(Token);
      else
         Self.Status := Done_Error; --TODO
      end if;
      Self.Expect := Value_Or_Eos;
   end Expect_Value;

   -------------------------------------------------------------------------
   procedure Ingest_Token
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      case Self.Expect is
         when Name =>
            Expect_Name(Self, Token);
         when Equal =>
            Expect_Equal(Self, Token);
         when Value =>
            Expect_Value(Self, Token);
         when Value_Or_Eos =>
            if Token.Is_Eos then
               Self.Status := Done_Good;
            else
               Expect_Value(Self, Token);
            end if;
      end case;
   end Ingest_Token;

   -------------------------------------------------------------------------
   function Get_Directive(Self : Token_State_Class) return kv.apg.directives.Directive_Pointer_Type is
      Directive : access kv.apg.directives.Token_Class;
   begin
      Directive := new kv.apg.directives.Token_Class;
      Directive.Initialize(Name => Self.Name_Token.Get_Data, Tree => Self.Tree);
      return kv.apg.directives.Directive_Pointer_Type(Directive);
   end Get_Directive;

end kv.apg.parse.token;
