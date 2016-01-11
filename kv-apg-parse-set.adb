with Ada.Text_IO;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Deallocation;

with kv.core.wwstr;

with kv.apg.tokens;
with kv.apg.lex;
with kv.apg.logger;

package body kv.apg.parse.set is

   use Ada.Text_IO;
   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Ada.Characters.Conversions;
   use kv.apg.tokens;
   use kv.apg.lex;
   use kv.core.wwstr;
   use kv.apg.logger;

   -------------------------------------------------------------------------
   procedure Ingest_Token
      (Self  : in out Set_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
      use kv.apg.tokens;
   begin
      case Self.Expect is
         when Set_Name  => Self.Expect_Name(Token);
         when Set_Equal => Self.Expect_Equal(Token);
         when Set_Value => Self.Expect_Value(Token);
         when Set_Eos   => Self.Expect_Eos(Token);
      end case;
   end Ingest_Token;

   -------------------------------------------------------------------------
   not overriding procedure Expect_Name
      (Self  : in out Set_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Word then
         Self.Name_Token := Token;
         Self.Expect := Set_Equal;
      else
         Self.Handle_Error(Token, "Expected an unquoted name");
      end if;
   end Expect_Name;

   -------------------------------------------------------------------------
   not overriding procedure Expect_Equal
      (Self  : in out Set_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Symbol and then To_String(+Token.Get_Data) = "=" then
         Self.Expect := Set_Value;
      else
         Self.Handle_Error(Token, "Expected '='");
      end if;
   end Expect_Equal;

   -------------------------------------------------------------------------
   not overriding procedure Expect_Value
      (Self  : in out Set_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_String or else Token.Get_Kind = A_Block then
         Self.Value_Token := Token;
         Self.Expect := Set_Eos;
      else
         Self.Handle_Error(Token, "Expected a quoted string or block");
      end if;
   end Expect_Value;

   -------------------------------------------------------------------------
   not overriding procedure Expect_Eos
      (Self  : in out Set_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Is_Eos then
         Self.Status := Done_Good;
      else
         Self.Handle_Error(Token, "Expected ';'");
      end if;
   end Expect_Eos;

   -------------------------------------------------------------------------
   function Get_Directive(Self : in out Set_State_Class) return kv.apg.directives.Directive_Pointer_Type is
      Set_Directive : access kv.apg.directives.Set_Class;
   begin
      if Self.Status /= Done_Good then
         return null;
      end if;
      Self.Status := Done_Done;
      Set_Directive := new kv.apg.directives.Set_Class;
      Set_Directive.Initialize(Name => Self.Name_Token, Value => Self.Value_Token.Get_Data);
      return kv.apg.directives.Directive_Pointer_Type(Set_Directive);
   end Get_Directive;

end kv.apg.parse.set;
