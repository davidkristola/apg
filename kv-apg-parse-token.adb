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
   use kv.apg.lex;
   use kv.core.wwstr;

   -------------------------------------------------------------------------
   procedure Ingest_Token
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
      use kv.apg.tokens;
      Match_Node : access kv.apg.regex.Match_Node_Class;
      Wild_Node : access kv.apg.regex.Match_Any_Node_Class;
      Or_Node : access kv.apg.regex.Or_Node_Class;
   begin
      case Self.Expect is
         when Name =>
            if Token.Get_Kind = A_Word then
               Self.Name_Token := Token;
               Self.Expect := Equal;
            else
               Self.Status := Done_Error;
            end if;
         when Equal =>
            if Token.Get_Kind = A_Symbol and then To_String(+Token.Get_Data) = "=" then
               Self.Expect := Value;
            else
               Self.Status := Done_Error;
            end if;
         when Value | Value_Or_Eos => --TODO: finish this logic
            if Self.Expect = Value_Or_Eos and then Token.Is_Eos then
               Self.Status := Done_Good;
            elsif Token.Get_Kind = A_String or else Token.Get_Kind = A_Block then
               Match_Node := new kv.apg.regex.Match_Node_Class;
               Match_Node.Initialize(Token.Get_Data);
               Match_Node.Set_Left(Self.Tree);
               Self.Tree := kv.apg.regex.Node_Pointer_Type(Match_Node);
               Self.Expect := Value_Or_Eos;
            elsif Token.Get_Kind = A_Symbol then
               Put_Line("Got symbol " & Token.Get_Data_As_String);
               if Token.Get_Data_As_String = "." then
                  Wild_Node := new kv.apg.regex.Match_Any_Node_Class;
                  Wild_Node.Initialize;
                  Wild_Node.Set_Left(Self.Tree);
                  Self.Tree := kv.apg.regex.Node_Pointer_Type(Wild_Node);
                  Self.Expect := Value_Or_Eos;
               elsif Token.Get_Data_As_String = "|" then
                  Or_Node := new kv.apg.regex.Or_Node_Class;
                  Or_Node.Initialize;
                  Or_Node.Set_Left(Self.Tree);
                  Self.Tree := kv.apg.regex.Node_Pointer_Type(Or_Node);
                  Self.Expect := Value_Or_Eos;
               else
                  Self.Status := Done_Error; --TODO
               end if;
            else
               Self.Status := Done_Error;
            end if;
         when Eos =>
            if Token.Is_Eos then
               Self.Status := Done_Good;
            else
               Self.Status := Done_Error;
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
