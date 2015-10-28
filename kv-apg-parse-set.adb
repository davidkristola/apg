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
         when Set_Name =>
            if Token.Get_Kind = A_Word then
               Self.Name_Token := Token;
               Self.Expect := Set_Equal;
            else
               Self.Status := Done_Error;
            end if;
         when Set_Equal =>
            if Token.Get_Kind = A_Symbol and then To_String(+Token.Get_Data) = "=" then
               Self.Expect := Set_Value;
            else
               Self.Status := Done_Error;
               if Self.Logger /= null then
                  Self.Logger.Note_Error
                     (Location => Token.Get_Location,
                      Citation => Token.Get_Data,
                      Reason   => "Expected '='");
               end if;
            end if;
         when Set_Value =>
            if Token.Get_Kind = A_String or else Token.Get_Kind = A_Block then
               Self.Value_Token := Token;
               Self.Expect := Set_Eos;
            else
               Self.Status := Done_Error;
            end if;
         when Set_Eos =>
            if Token.Is_Eos then
               Self.Status := Done_Good;
            else
               Self.Status := Done_Error;
            end if;
      end case;
   end Ingest_Token;

   -------------------------------------------------------------------------
   function Get_Directive(Self : Set_State_Class) return kv.apg.directives.Directive_Pointer_Type is
      Set_Directive : access kv.apg.directives.Set_Class;
   begin
      Set_Directive := new kv.apg.directives.Set_Class;
      Set_Directive.Initialize(Name => Self.Name_Token.Get_Data, Value => Self.Value_Token.Get_Data);
      return kv.apg.directives.Directive_Pointer_Type(Set_Directive);
   end Get_Directive;

end kv.apg.parse.set;
