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
   use kv.apg.logger;
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
         Self.Expect := Colon_Or_Equal;
      else
         Self.Status := Done_Error;
      end if;
   end Expect_Name;

   -------------------------------------------------------------------------
   procedure Expect_Colon_Or_Equal
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Symbol then
         if Token.Get_Data_As_String = "=" then
            Self.Expect := Value;
         elsif Token.Get_Data_As_String = ":" then
            Self.Expect := Flag;
         else
            Self.Status := Done_Error;
         end if;
      else
         Self.Status := Done_Error;
      end if;
   end Expect_Colon_Or_Equal;

   -------------------------------------------------------------------------
   procedure Process_Flag
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Number then
         Self.Precedence := kv.apg.enum.Token_Precedence_Type'VALUE(Token.Get_Data_As_String);
      elsif Token.Get_Data_As_String = "left" then
         Self.Associativity := kv.apg.enum.Left;
      elsif Token.Get_Data_As_String = "right" then
         Self.Associativity := kv.apg.enum.Right;
      else
         Self.Status := Done_Error;
      end if;
   end Process_Flag;

   -------------------------------------------------------------------------
   procedure Expect_Flag
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Word or Token.Get_Kind = A_Number then
         Self.Expect := Flag_Or_Equal;
         Process_Flag(Self, Token); -- Could change status to Done_Error
      else
         Self.Status := Done_Error;
      end if;
   end Expect_Flag;

   -------------------------------------------------------------------------
   procedure Expect_Flag_Or_Equal
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Word or Token.Get_Kind = A_Number then
         Self.Expect := Equal;
         Process_Flag(Self, Token); -- Could change status to Done_Error
      elsif Token.Get_Kind = A_Symbol and then Token.Get_Data_As_String = "=" then
         Self.Expect := Value;
      else
         Self.Status := Done_Error;
      end if;
   end Expect_Flag_Or_Equal;

   -------------------------------------------------------------------------
   procedure Expect_Equal
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Symbol and then Token.Get_Data_As_String = "=" then
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
         if Self.Logger /= null then
            Self.Logger.Note_Error
               (Location => Token.Get_Location,
                Citation => Token.Get_Data,
                Reason   => "Empty regular expression");
         end if;
      end if;
      Self.Expect := Value_Or_Eos;
   end Expect_Value;

   -------------------------------------------------------------------------
   procedure Process_Eos
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Self.Tree.Is_Complete then
         Self.Status := Done_Good;
      else
         Self.Status := Done_Error;
         if Self.Logger /= null then
            Self.Logger.Note_Error
               (Location => Token.Get_Location,
                Citation => Token.Get_Data,
                Reason   => "Incomplete regular expression");
            Self.Tree.Diagnose_To_Log(kv.apg.logger.Safe_Logger_Pointer(Self.Logger));
         end if;
      end if;
   end Process_Eos;

   -------------------------------------------------------------------------
   procedure Expect_Value_Or_Eos
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Is_Eos then
         Process_Eos(Self, Token);
      else
         Expect_Value(Self, Token);
      end if;
   end Expect_Value_Or_Eos;

   -------------------------------------------------------------------------
   procedure Ingest_Token
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      case Self.Expect is
         when Name =>            Expect_Name(Self, Token);
         when Colon_Or_Equal =>  Expect_Colon_Or_Equal(Self, Token);
         when Flag =>            Expect_Flag(Self, Token);
         when Flag_Or_Equal =>   Expect_Flag_Or_Equal(Self, Token);
         when Equal =>           Expect_Equal(Self, Token);
         when Value =>           Expect_Value(Self, Token);
         when Value_Or_Eos =>    Expect_Value_Or_Eos(Self, Token);
      end case;
   end Ingest_Token;

   -------------------------------------------------------------------------
   function Get_Directive(Self : Token_State_Class) return kv.apg.directives.Directive_Pointer_Type is
      Directive : access kv.apg.directives.Token_Class;
   begin
      Directive := new kv.apg.directives.Token_Class;
      Directive.Initialize(Name => Self.Name_Token, Tree => Self.Tree, Kind => Self.Kind);
      Directive.Set_Associativity(Self.Associativity);
      Directive.Set_Precedence(Self.Precedence);
      return kv.apg.directives.Directive_Pointer_Type(Directive);
   end Get_Directive;

end kv.apg.parse.token;
