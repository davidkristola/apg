with Ada.Text_IO;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Deallocation;

with kv.apg.tokens;
with kv.apg.lex;
with kv.core.wwstr;

package body kv.apg.parse is

   use Ada.Text_IO;
   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Ada.Characters.Conversions;
   use kv.apg.tokens;
   use kv.apg.lex;
   use kv.core.wwstr;

   procedure Free is new Ada.Unchecked_Deallocation(Substates.State_Class'CLASS, Substates.State_Pointer_Type);


   ----------------------------------------------------------------------------
   package body Substates is

      -------------------------------------------------------------------------
      function Status(Self : State_Class) return Status_Type is
      begin
         return Self.Status;
      end Status;

      -------------------------------------------------------------------------
      procedure Ingest_Token
         (Self  : in out Set_State_Class;
          Token : in     kv.apg.tokens.Token_Class) is
      begin
         case Self.Expect is
            when Set_Name =>
               Self.Name_Token := Token;
               Self.Expect := Set_Equal;
            when Set_Equal =>
               Self.Expect := Set_Value;
            when Set_Value =>
               Self.Value_Token := Token;
               Self.Expect := Set_Eos;
            when Set_Eos =>
               Self.Status := Done_Good;
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
   end Substates;

   ----------------------------------------------------------------------------
   procedure Initialise
      (Self : in out Parser_Class) is
   begin
      Self.Action := Scan;
   end Initialise;

   ----------------------------------------------------------------------------
   procedure Ingest_Token
      (Self  : in out Parser_Class;
       Token : in     kv.apg.tokens.Token_Class) is
      use Substates;
   begin
      --Put_Line("Ingest_Token " & Token_Type'IMAGE(Token.Get_Kind) & " '" & To_String(+Token.Get_Data) & "'");
      case Self.Action is
         when Scan =>
            if To_String(+Token.Get_Data) = "set" then
               Self.Action := Process;
               Self.Substate := new Substates.Set_State_Class;
            else
               Self.Action := Recover;
            end if;
         when Process =>
            Self.Substate.Ingest_Token(Token);
            if Self.Substate.Status in Substates.Done_Status_Type then
               if Self.Substate.Status = Substates.Done_Good then
                  Self.Directives.Append(Self.Substate.Get_Directive);
                  Self.Action := Scan;
               else
                  Self.Action := Recover;
               end if;
               Free(Self.Substate);
            end if;
         when Recover =>
            if Token.Is_Eos then
               Self.Action := Scan;
            end if;
      end case;
   end Ingest_Token;

   ----------------------------------------------------------------------------
   function Inbetween_Directives
      (Self : in     Parser_Class) return Boolean is
   begin
      return Self.Action = Scan;
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
      return Natural(Self.Directives.Length);
   end Directive_Count;

   ----------------------------------------------------------------------------
   function Next_Directive
      (Self : in out Parser_Class) return kv.apg.directives.Directive_Pointer_Type is
      Directive : kv.apg.directives.Directive_Pointer_Type;
   begin
      Directive := Self.Directives.First_Element;
      Self.Directives.Delete_First;
      return Directive;
   end Next_Directive;

end kv.apg.parse;
