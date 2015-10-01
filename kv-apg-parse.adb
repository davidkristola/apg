with Ada.Text_IO;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Deallocation;

with kv.apg.tokens;
with kv.apg.lex;
with kv.core.wwstr;

with kv.apg.parse.set;
with kv.apg.parse.token;

package body kv.apg.parse is

   use Ada.Text_IO;
   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Ada.Characters.Conversions;
   use kv.apg.tokens;
   use kv.apg.lex;
   use kv.core.wwstr;

   procedure Free is new Ada.Unchecked_Deallocation(State_Class'CLASS, State_Pointer_Type);


   -------------------------------------------------------------------------
   function Status(Self : State_Class) return Status_Type is
   begin
      return Self.Status;
   end Status;

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
      Token_State : kv.apg.parse.token.Token_State_Pointer_Type;
      use kv.apg.tokens;
   begin
      if Token.Get_Kind = A_Comment then
         return; -- Skip comments
      end if;
      --Put_Line("Ingest_Token " & Token_Type'IMAGE(Token.Get_Kind) & " '" & To_String(+Token.Get_Data) & "'");
      case Self.Action is
         when Scan =>
            if Token.Get_Data_As_String = "set" then
               Self.Action := Process;
               Self.Substate := new kv.apg.parse.set.Set_State_Class;
            elsif kv.apg.parse.token.Is_Token_Variant(Token.Get_Data_As_String) then
               Self.Action := Process;
               Token_State := new kv.apg.parse.token.Token_State_Class;
               Token_State.Initialize(Token.Get_Data_As_String);
               Self.Substate := State_Pointer_Type(Token_State);
            else
               Self.Errors := Self.Errors + 1;
               Self.Action := Recover;
            end if;
         when Process =>
            Self.Substate.Ingest_Token(Token);
            if Self.Substate.Status in Done_Status_Type then
               if Self.Substate.Status = Done_Good then
                  Self.Directives.Append(Self.Substate.Get_Directive);
                  Self.Action := Scan;
               else
                  Self.Errors := Self.Errors + 1;
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
      return Self.Action /= Process;
   end Inbetween_Directives;

   ----------------------------------------------------------------------------
   function Error_Count
      (Self : in     Parser_Class) return Natural is
   begin
      return Self.Errors;
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

   ----------------------------------------------------------------------------
   procedure Process_Directives
      (self    : in out Parser_Class;
       Visitor : in out kv.apg.directives.Directive_Visitor_Class'CLASS) is
   begin
      for Directive of Self.Directives loop
         Directive.Process(Visitor);
      end loop;
   end Process_Directives;

   ----------------------------------------------------------------------------
   procedure Delete_Directives
      (Self : in out Parser_Class) is
      Directive : kv.apg.directives.Directive_Pointer_Type;
   begin
      while not Self.Directives.Is_Empty loop
         Directive := Self.Next_Directive;
         Free(Directive);
      end loop;
   end Delete_Directives;

end kv.apg.parse;
