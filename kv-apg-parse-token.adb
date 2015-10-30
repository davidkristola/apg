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
   function Is_Token_Variant(S : String) return Boolean is
   begin
      return (S = "token") or (S = "pattern") or (S = "skipover");
   end Is_Token_Variant;

   -------------------------------------------------------------------------
   not overriding procedure Initialize(Self : in out Token_State_Class; Token_Variant : in     String) is
   begin
      if (Token_Variant = "token") then
         Self.Kind := kv.apg.directives.Accepting;
      elsif (Token_Variant = "pattern") then
         Self.Kind := kv.apg.directives.Pattern;
      elsif (Token_Variant = "skipover") then
         Self.Kind := kv.apg.directives.Skipover;
      else
         -- There should be no way to get here.
         Self.Status := Done_Error;
      end if;
   end Initialize;

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
         Token.Get_Data_As_String = ")" or else
         Token.Get_Data_As_String = "-"
      then
         return True; -- Valid symbol
      end if;
      if Token.Get_Kind = A_Word then
      -- TODO: consider pushing this into the tokenizer and having an A_Code_Point token.
         declare
            Word : constant String := Token.Get_Data_As_String;
         begin
            if (Word(1) = 'U' and then ((Word'LENGTH = 5) or (Word'LENGTH = 9))) then
               return True; -- Valid (looking) Code Point
            end if;
            -- TODO: valid named patterns or named sets?
         end;
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
   procedure Process_Eos
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
      use kv.apg.logger;
   begin
      if Self.Tree.Is_Complete then
         Self.Status := Done_Good;
      else
         Self.Status := Done_Error; --TODO
         if Self.Logger /= null then
            Self.Logger.Note_Error
               (Location => Token.Get_Location,
                Citation => Token.Get_Data,
                Reason   => "Incomplete regular expression");
         end if;
      end if;
   end Process_Eos;

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
               Process_Eos(Self, Token);
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
      Directive.Initialize(Name => Self.Name_Token.Get_Data, Tree => Self.Tree, Kind => Self.Kind);
      return kv.apg.directives.Directive_Pointer_Type(Directive);
   end Get_Directive;

end kv.apg.parse.token;
