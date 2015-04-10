with Ada.Text_IO;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded;

with kv.apg.tokens;
with kv.apg.lex;

package body kv.apg.parse is

   use Ada.Text_IO;
   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Ada.Characters.Conversions;
   use kv.apg.tokens;
   use kv.apg.lex;

   ----------------------------------------------------------------------------
   procedure Initialise
      (Self : in out Parser_Class) is
   begin
      Self.Expect := Directive;
   end Initialise;

   ----------------------------------------------------------------------------
   procedure Ingest_Token
      (Self  : in out Parser_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      Put_Line("Ingest_Token " & Token_Type'IMAGE(Token.Get_Kind) & " '" & To_String(+Token.Get_Data) & "'");
      case Self.Expect is
         when Initialize =>
            raise Constraint_Error;
         when Directive =>
            if To_String(+Token.Get_Data) = "set" then
               Self.Expect := Set_Name;
            else
               Self.Expect := Error_Eos;
            end if;
         when Set_Name =>
            Self.Expect := Set_Equal;
         when Set_Equal =>
            Self.Expect := Set_Value;
         when Set_Value =>
            Self.Expect := Set_Eos;
         when Set_Eos =>
            Self.Expect := Directive;
         when others =>
            null;
      end case;
   end Ingest_Token;

   ----------------------------------------------------------------------------
   function Inbetween_Directives
      (Self : in     Parser_Class) return Boolean is
   begin
      return Self.Expect = Directive;
   end Inbetween_Directives;

   ----------------------------------------------------------------------------
   function Error_Count
      (Self : in     Parser_Class) return Natural is
   begin
      return 0;
   end Error_Count;

   ----------------------------------------------------------------------------
   function Directive_Count
      (Self : in     Parser_Class) return Natural is
   begin
      return 0;
   end Directive_Count;

   ----------------------------------------------------------------------------
   function Next_Directive
      (Self : in out Parser_Class) return kv.apg.directives.Directive_Pointer_Type is
   begin
      return null;
   end Next_Directive;

end kv.apg.parse;
