with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;

with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Tags;

with Ada.Text_IO; use Ada.Text_IO;

with kv.apg.locations;
with kv.apg.incidents;
with kv.apg.lalr.stacks;
with kv.apg.lalr.tables;

package body kv.apg.lalr.engines is

   use Ada.Strings.UTF_Encoding;
   use Ada.Strings.UTF_Encoding.Strings;

   use kv.apg.incidents; -- Severity_Type
   use kv.apg.lalr.stacks;
   use kv.apg.lalr.tables;


   function Img(Arg : Production_Index_Type) return String renames Production_Index_Type'IMAGE;


   ----------------------------------------------------------------------------
   procedure Process_Hint_Action
      (Self   : in out Parser_Engine_Class;
       Hint   : in     Action_Hint_Type;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is

      Action : Action_Entry_Type;

   begin
      if Hint.Symbol.Get_Number = End_Of_File then
         -- Special case: Add an accept action
         Action := (What => Accept_Input, Precedence => 0, Associativity => kv.apg.enum.Neither);
         Logger.Note_By_Severity(Debug, Img(Hint.From_State) & ": Add ACCEPT " & To_String(Hint.Symbol.Name));
      else
         -- Add a shift action
         Action := (What => Shift, Where => Hint.To_State, Precedence => Self.Grammar.Get_Tokens.Get_Precedence(Natural(Hint.Symbol.Get_Number)), Associativity => kv.apg.enum.Neither);
         Logger.Note_By_Severity(Debug, Img(Hint.From_State) & ": Add SHIFT " & To_String(Hint.Symbol.Name) & " and goto " & Img(Hint.To_State));
      end if;
      Self.Actions.Set_Action(Action, Hint.From_State, Hint.Symbol.Get_Number, Logger);
   end Process_Hint_Action;

   ----------------------------------------------------------------------------
   procedure Process_Hint_Goto
      (Self   : in out Parser_Engine_Class;
       Hint   : in     Action_Hint_Type;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is
   begin
      -- Add a non-terminal goto
      Logger.Note_By_Severity(Debug, Img(Hint.From_State) & ": Add GOTO " & To_String(Hint.Symbol.Name) & " and goto " & Img(Hint.To_State));
      Self.Gotos.Set_Goto(Hint.To_State, Hint.From_State, Self.Grammar.Rule_Of(Hint.Symbol).Get_Number);
   end Process_Hint_Goto;

   ----------------------------------------------------------------------------
   procedure Process_Hint
      (Self   : in out Parser_Engine_Class;
       Hint   : in     Action_Hint_Type;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is
   begin
      if Hint.Symbol.Is_Terminal then
         Process_Hint_Action(Self, Hint, Logger);
      else
         Process_Hint_Goto(Self, Hint, Logger);
      end if;
   end Process_Hint;

   ----------------------------------------------------------------------------
   procedure Process_Hints
      (Self   : in out Parser_Engine_Class;
       Hints  : in     Action_Space.Vector;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is

   begin
      for Hint of Hints loop
         if Hint.Symbol /= null then
            Process_Hint(Self, Hint, Logger);
         end if;
      end loop;
   end Process_Hints;


   ----------------------------------------------------------------------------
   procedure Process_State_Item
      (Self   : in out Parser_Engine_Class;
       Index  : in     State_Index_Type;
       Item   : in     Constant_Item_Pointer;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is

      Rule   : Rule_Pointer;
      Symbol : Constant_Symbol_Pointer;
      Action : Action_Entry_Type;

      use kv.apg.enum;

   begin
      Rule := Item.Get_Big_A; -- Get this Item's rule
      for T of Rule.Follow loop -- Consider everything that can follow this rule...
         Symbol := Self.Grammar.Translate(T);
         -- Add a reduce action
         Action := (What => Reduce, Production => Item.Get_Production_Number, Precedence => Item.Production.Get_Precedence, Associativity => kv.apg.enum.Neither);
         Logger.Note_By_Severity(Debug, Img(Index) & ": add RUDUCE by production" & Img(Item.Get_Production_Number) & ", terminal " & To_String(Symbol.Name));
         Self.Actions.Set_Action(Action, Index, T, Logger);
      end loop;
   end Process_State_Item;

   ----------------------------------------------------------------------------
   procedure Process_States
      (Self   : in out Parser_Engine_Class;
       States : in     State_Space.Vector;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is
   begin
      for State of States loop
         for Item of State.Kernels loop
            if not Item.Has_Next then -- If the dot is at the end of the item...
               Process_State_Item(Self, State.Index, Item, Logger);
            end if;
         end loop;
      end loop;
   end Process_States;

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self    : in out Parser_Engine_Class;
       Grammar : in     Grammar_Pointer;
       Logger  : in     kv.apg.logger.Safe_Logger_Pointer) is

      Start_State : State_Entry_Type := (Symbol => null, State => 0);
      Info        : State_Information_Type;
      State_Count : State_Index_Type;

   begin
      Self.Grammar := Grammar;
      Info := Grammar.Generate_Parser_States(Logger);
      State_Count := State_Index_Type(Info.States.Length)+1;
      Self.Stack.Push_State(Start_State);
      Self.Actions.Initialize(State_Count, Grammar.Terminal_Lo, Grammar.Terminal_Hi);
      Self.Gotos.Initialize(State_Count, Grammar.Rule_Number_Lo, Grammar.Rule_Number_Hi);
      Process_Hints(Self, Info.Hints, Logger);
      Process_States(Self, Info.States, Logger);
   end Initialize;

   ----------------------------------------------------------------------------
   function Img(State : State_Entry_Type) return String is
   begin
      if State.Symbol = null then
         return "[null, " & Img(State.State) & "]";
      end if;
      return "[" & To_String(State.Symbol.Name) & ", " & Img(State.State) & "]";
   end Img;

   ----------------------------------------------------------------------------
   procedure Parse_Token
      (Self   : in out Parser_Engine_Class;
       Token  : in     Terminal_Index_Type;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is

      Unshifted     : Boolean := True;
      Current_State : State_Entry_Type;
      State_Index   : State_Index_Type;
      Action        : Action_Entry_Type;
      Rule          : Rule_Pointer;
      Symbol        : Constant_Symbol_Pointer;
      Production    : Production_Pointer;

   begin
      Symbol := Self.Grammar.Translate(Token);
      while Unshifted loop
         State_Index := Self.Stack.Top_State;
         Logger.Note_By_Severity(Debug, "Processing state "&Img(State_Index)&" with terminal " & To_String(Symbol.Name));
         Action := Self.Actions.Get_Action(State_Index, Token);
         case Action.What is
            when Shift =>
               Current_State := (Symbol => Symbol, State => Action.Where);
               Logger.Note_By_Severity(Debug, "SHIFT " & Img(Current_State));
               Self.Stack.Push_State(Current_State);
               Unshifted := False;
            when Reduce =>
               Production := Self.Grammar.Get_Production(Action.Production);
               Logger.Note_By_Severity(Debug, "REDUCE by " & To_String(Production.Image) & ".");
               for I in 1..Production.Symbol_Count loop
                  Current_State := Self.Stack.Pop_State;
                  Logger.Note_By_Severity(Debug, "---- pop "  & Img(Current_State));
               end loop;
               State_Index := Self.Stack.Top_State;
               Rule := Production.Get_Rule;
               Current_State := (Symbol => Rule.Get_Symbol, State => Self.Gotos.Get_Goto(State_Index, Rule.Get_Number));
               Logger.Note_By_Severity(Debug, "PUSH " & Img(Current_State));
               Self.Stack.Push_State(Current_State);
            when Accept_Input =>
               Logger.Note_By_Severity(Information, "ACCEPT!!!!");
               Unshifted := False;
               Self.Accepted := True;
            when Error =>
               Logger.Note_By_Severity(Error, "ERROR!!!!");
               Unshifted := False;
               Self.Errors := Self.Errors + 1;
         end case;
      end loop;
   end Parse_Token;

   ----------------------------------------------------------------------------
   function Error_Count(Self : Parser_Engine_Class) return Natural is
   begin
      return Self.Errors + Self.Actions.Error_Count;
   end Error_Count;

   ----------------------------------------------------------------------------
   function Has_Accepted(Self : Parser_Engine_Class) return Boolean is
   begin
      return Self.Accepted and (Self.Errors = 0);
   end Has_Accepted;


end kv.apg.lalr.engines;
