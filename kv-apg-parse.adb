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

   procedure Initialise
      (Self : in out Parser_Class) is
   begin
      null;
   end Initialise;

   procedure Ingest_Token
      (Self  : in out Parser_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      Put_Line("Ingest_Token " & Token_Type'IMAGE(Token.Get_Kind) & " '" & To_String(+Token.Get_Data) & "'");
   end Ingest_Token;

end kv.apg.parse;
