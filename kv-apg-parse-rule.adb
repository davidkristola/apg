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

package body kv.apg.parse.rule is

   use Ada.Text_IO;
   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Ada.Characters.Conversions;
   use kv.apg.tokens;
   use kv.apg.lex;
   use kv.core.wwstr;
   use kv.apg.logger;

   -------------------------------------------------------------------------
   procedure Process_Name
      (Self  : in out Rule_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      --Put_Line("Process_Name looking at <"&Token.Get_Data_As_String&">.");
      if Token.Get_Kind = A_Word then
         Self.Name_Token := Token;
         Self.Expect := Equal;
      else
         Put_Line("Expected an unquoted name");
         Self.Handle_Error(Token, "Expected an unquoted name");
      end if;
   end Process_Name;

   -------------------------------------------------------------------------
   procedure Process_Equal
      (Self  : in out Rule_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      --Put_Line("Process_Equal looking at <"&Token.Get_Data_As_String&">.");
      if Token.Get_Kind = A_Symbol and then Token.Get_Data_As_String = "=" then
         Self.Expect := Flag_Or_Production;
      else
         Put_Line("Expected '='");
         Self.Handle_Error(Token, "Expected '='");
      end if;
   end Process_Equal;

   -------------------------------------------------------------------------
   procedure Process_Production
      (Self  : in out Rule_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      Self.Expect := Element_Or_Causes; -- TODO
      Self.Working := kv.apg.rules.New_Production_Class;
   end Process_Production;

   -------------------------------------------------------------------------
   procedure Process_Flag
      (Self  : in out Rule_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      Self.Expect := Flag_Or_Production;
      -- TODO
      if Token.Get_Data_As_String = "start" then
         Self.Start_Flag := True;
      end if;
   end Process_Flag;

   -------------------------------------------------------------------------
   procedure Process_Flag_Or_Production
      (Self  : in out Rule_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      --Put_Line("Process_Flag_Or_Production looking at <"&Token.Get_Data_As_String&">.");
      if Token.Get_Kind = A_Word then
         Process_Flag(Self, Token);
      elsif Token.Get_Kind = A_Symbol and then Token.Get_Data_As_String = "|" then
         Process_Production(Self, Token);
      else
         Put_Line("Expected a flag or '|'");
         Self.Handle_Error(Token, "Expected a flag or '|'");
      end if;
   end Process_Flag_Or_Production;

   -------------------------------------------------------------------------
   procedure Process_Eos
      (Self  : in out Rule_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      --Put_Line("Process_Eos looking at <"&Token.Get_Data_As_String&">.");
      Self.Status := Done_Good;
   end Process_Eos;

   -------------------------------------------------------------------------
   procedure Process_Production_Or_Eos
      (Self  : in out Rule_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      --Put_Line("Process_Production_Or_Eos looking at <"&Token.Get_Data_As_String&">.");
      if Token.Is_Eos then
         Process_Eos(Self, Token);
      elsif Token.Get_Kind = A_Symbol and then Token.Get_Data_As_String = "|" then
         Process_Production(Self, Token);
      else
         Put_Line("Expected ';' or '|'. Got <"&Token.Get_Data_As_String&">.");
         Self.Handle_Error(Token, "Expected ';' or '|'.");
      end if;
   end Process_Production_Or_Eos;

   -------------------------------------------------------------------------
   procedure Process_Element
      (Self  : in out Rule_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      Self.Working.Append(kv.apg.rules.New_Pre_Element(Token));
   end Process_Element;

   -------------------------------------------------------------------------
   procedure Process_Causes
      (Self  : in out Rule_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      Self.Expect := Code;
   end Process_Causes;

   -------------------------------------------------------------------------
   procedure Process_Element_Or_Causes
      (Self  : in out Rule_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      --Put_Line("Process_Element_Or_Causes looking at <"&Token.Get_Data_As_String&">.");
      if Token.Get_Kind = A_Word then
         Process_Element(Self, Token);
      elsif Token.Get_Kind = A_Symbol and then Token.Get_Data_As_String = "=>" then
         Process_Causes(Self, Token);
      else
         Put_Line("Expected a rule element or '=>'.");
         Self.Handle_Error(Token, "Expected a rule element or '=>'.");
      end if;
   end Process_Element_Or_Causes;

   -------------------------------------------------------------------------
   procedure Process_Code
      (Self  : in out Rule_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      --Put_Line("Process_Code looking at <"&Token.Get_Data_As_String&">.");
      Self.Expect := Production_Or_Eos;
      Self.Working.Set_Code(Token);
      Self.Productions.Append(Self.Working);
   end Process_Code;

   -------------------------------------------------------------------------
   procedure Ingest_Token
      (Self  : in out Rule_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      case Self.Expect is
         when Name               => Process_Name(Self, Token);
         when Equal              => Process_Equal(Self, Token);
         when Flag_Or_Production => Process_Flag_Or_Production(Self, Token);
         when Production_Or_Eos  => Process_Production_Or_Eos(Self, Token);
         when Element_Or_Causes  => Process_Element_Or_Causes(Self, Token);
         when Code               => Process_Code(Self, Token);
      end case;
   end Ingest_Token;

   -------------------------------------------------------------------------
   function Get_Directive(Self : Rule_State_Class) return kv.apg.directives.Directive_Pointer_Type is
      Rule_Directive : access kv.apg.directives.Rule_Class;
      Rule : kv.apg.rules.Rule_Pointer;
   begin
      Rule_Directive := new kv.apg.directives.Rule_Class;
      Rule := new kv.apg.rules.Rule_Class;
      Rule.Initialize(Self.Name_Token, Self.Productions);
      Rule.Set_Is_Start(Self.Start_Flag);
      Rule_Directive.Initialize(Name => Self.Name_Token, Rule => Rule);
      return kv.apg.directives.Directive_Pointer_Type(Rule_Directive);
   end Get_Directive;

end kv.apg.parse.rule;
