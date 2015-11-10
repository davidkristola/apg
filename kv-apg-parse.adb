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
with kv.apg.parse.rule;

package body kv.apg.parse is

   use Ada.Text_IO;
   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Ada.Characters.Conversions;
   use kv.apg.tokens;
   use kv.apg.lex;
   use kv.apg.logger;
   use kv.core.wwstr;

   procedure Free is new Ada.Unchecked_Deallocation(State_Class'CLASS, State_Pointer_Type);


   -------------------------------------------------------------------------
   function Status(Self : State_Class) return Status_Type is
   begin
      return Self.Status;
   end Status;

   ----------------------------------------------------------------------------
   procedure Set_Logger
      (Self   : in out State_Class;
       Logger : in     kv.apg.logger.Logger_Pointer) is
   begin
      Self.Logger := Logger;
   end Set_Logger;

   -------------------------------------------------------------------------
   procedure Handle_Error
      (Self   : in out State_Class;
       Token  : in     kv.apg.tokens.Token_Class;
       Reason : in     String) is
   begin
      Self.Status := Done_Error;
      if Self.Logger /= null then
         Self.Logger.Note_Error
            (Location => Token.Get_Location,
             Citation => Token.Get_Data,
             Reason   => Reason);
      end if;
   end Handle_Error;

   ----------------------------------------------------------------------------
   procedure Initialise
      (Self : in out Parser_Class) is
   begin
      Self.Action := Scan;
   end Initialise;

   ----------------------------------------------------------------------------
   procedure Set_Logger
      (Self   : in out Parser_Class;
       Logger : in     kv.apg.logger.Logger_Pointer) is
   begin
      Self.Logger := Logger;
   end Set_Logger;

   ----------------------------------------------------------------------------
   procedure Ingest_Token
      (Self  : in out Parser_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Comment then
         return; -- Skip comments
      end if;
      --Put_Line("Ingest_Token " & Token_Type'IMAGE(Token.Get_Kind) & " '" & To_String(+Token.Get_Data) & "'");
      case Self.Action is
         when Scan    => Self.Do_Action_Scan(Token);
         when Process => Self.Do_Action_Process(Token);
         when Recover => Self.Do_Action_Recover(Token);
      end case;
   end Ingest_Token;

   ----------------------------------------------------------------------------
   procedure Start_Processing_Set_Directive
      (Self : in out Parser_Class) is
   begin
      Self.Action := Process;
      Self.Substate := new kv.apg.parse.set.Set_State_Class;
      Self.Substate.Set_Logger(Self.Logger);
   end Start_Processing_Set_Directive;

   ----------------------------------------------------------------------------
   procedure Start_Processing_Token_Directive
      (Self : in out Parser_Class;
       Kind : in     String) is
      Token_State : kv.apg.parse.token.Token_State_Pointer_Type;
   begin
      Self.Action := Process;
      Token_State := new kv.apg.parse.token.Token_State_Class;
      Token_State.Initialize(Kind);
      Self.Substate := State_Pointer_Type(Token_State);
      Self.Substate.Set_Logger(Self.Logger);
   end Start_Processing_Token_Directive;

   ----------------------------------------------------------------------------
   procedure Start_Processing_Rule_Directive
      (Self : in out Parser_Class;
       Kind : in     String) is
   begin
      Self.Action := Process;
      Self.Substate := new kv.apg.parse.rule.Rule_State_Class;
      Self.Substate.Set_Logger(Self.Logger);
   end Start_Processing_Rule_Directive;

   ----------------------------------------------------------------------------
   procedure Handle_Scan_Error
      (Self  : in out Parser_Class;
       Token : in     kv.apg.tokens.Token_Class) is
      use kv.apg.logger;
   begin
      Self.Errors := Self.Errors + 1;
      Self.Action := Recover;
      if Self.Logger /= null then
         Self.Logger.Note_Error
            (Location => Token.Get_Location,
             Citation => Token.Get_Data,
             Reason   => "Expected directive");
      end if;
   end Handle_Scan_Error;

   ----------------------------------------------------------------------------
   procedure Handle_Process_Error
      (Self  : in out Parser_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      Self.Errors := Self.Errors + 1;
      Self.Action := Recover;
      -- Processing error reporting is handled down in the appropriate code.
   end Handle_Process_Error;

   ----------------------------------------------------------------------------
   procedure Do_Action_Scan
      (Self  : in out Parser_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Data_As_String = "set" then
         Start_Processing_Set_Directive(Self);
      elsif kv.apg.parse.token.Is_Token_Variant(Token.Get_Data_As_String) then
         Start_Processing_Token_Directive(Self, Token.Get_Data_As_String);
      elsif Token.Get_Data_As_String = "rule" then
         Start_Processing_Rule_Directive(Self, Token.Get_Data_As_String);
      else
         Handle_Scan_Error(Self, Token);
      end if;
   end Do_Action_Scan;

   ----------------------------------------------------------------------------
   procedure Save_Completed_Directive
      (Self : in out Parser_Class) is
   begin
      Self.Directives.Append(Self.Substate.Get_Directive);
   end Save_Completed_Directive;

   ----------------------------------------------------------------------------
   procedure Complete_Action_Process
      (Self  : in out Parser_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Self.Substate.Status = Done_Good then
         Save_Completed_Directive(Self);
         Self.Action := Scan;
      else
         Handle_Process_Error(Self, Token);
      end if;
      Free(Self.Substate);
   end Complete_Action_Process;

   ----------------------------------------------------------------------------
   procedure Do_Action_Process
      (Self  : in out Parser_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      Self.Substate.Ingest_Token(Token);
      if Self.Substate.Status in Done_Status_Type then
         Complete_Action_Process(Self, Token);
      end if;
   end Do_Action_Process;

   ----------------------------------------------------------------------------
   procedure Do_Action_Recover
      (Self  : in out Parser_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Is_Eos then
         Self.Action := Scan;
      end if;
   end Do_Action_Recover;

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
