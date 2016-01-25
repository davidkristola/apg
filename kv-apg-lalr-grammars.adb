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

package body kv.apg.lalr.grammars is

   use Ada.Strings.UTF_Encoding;
   use Ada.Strings.UTF_Encoding.Strings;

   use kv.apg.incidents; -- Severity_Type

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self   : in out Grammar_Class;
       Tokens : in     kv.apg.enum.Enumeration_Class) is
   begin
      Self.Tokens := Tokens;
   end Initialize;

   ----------------------------------------------------------------------------
   function Get_Error_Count(Self : Grammar_Class) return Natural is
   begin
      return Self.Errors;
   end Get_Error_Count;

   ----------------------------------------------------------------------------
   function Get_Tokens(Self : Grammar_Class) return kv.apg.enum.Enumeration_Class is
   begin
      return Self.Tokens;
   end Get_Tokens;

   ----------------------------------------------------------------------------
   procedure Add_Rule
      (Self : in out Grammar_Class;
       Rule : in     Rule_Pointer) is
   begin
      Self.Rules.Include(Decode(To_String(Rule.Get_Name), UTF_8), Rule);
      Self.Count := Self.Count + 1;
      Rule.Number := Non_Terminal_Index_Type(Self.Count);
      Rule.Me := Rule;
   end Add_Rule;

   ----------------------------------------------------------------------------
   function Find_Start(Self : Grammar_Class) return Rule_Pointer is
   begin
      for Rule of Self.Rules loop
         if Rule.Start_Rule then
            return Rule;
         end if;
      end loop;
      return null;
   end Find_Start;

   ----------------------------------------------------------------------------
   procedure Add_Meta_Rule
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is

      P : Production_Pointer;
      P_V : Production_Vectors.Vector;
      S : Rule_Pointer;
      R : Rule_Pointer := new Rule_Class;

   begin
      Logger.Note_By_Severity(Debug, "Add_Meta_Rule");
      S := Find_Start(Self);
      if S = null then
         Logger.Note_By_Severity(Error, "Add_Meta_Rule: No start rule found!");
      end if;
      Logger.Note_By_Severity(Debug, S.Name_Token.Cite("Is a start rule"));
      P := New_Production_Class;
      P.Append(New_Non_Terminal_Symbol(S.Get_Token, S.Get_Number));
      P.Append(New_End_Of_File_Terminal);
      P_V.Append(P);
      R.Initialize(kv.apg.tokens.Meta_Start_Rule_Token, P_V);
      P.Set_Rule(R);
      R.Start_Rule := True;
      S.Start_Rule := False;
      Self.Add_Rule(R);
   end Add_Meta_Rule;

   ----------------------------------------------------------------------------
   procedure Collect_Grammar_Symbol
      (Self   : in out Grammar_Class;
       Symbol : in     Constant_Symbol_Pointer) is
   begin
      for Current of Self.All_Symbols loop
         if Current.Is_Same_As(Symbol.all) then
            return; -- Already collected
         end if;
      end loop;
      Self.All_Symbols.Append(Symbol);
   end Collect_Grammar_Symbol;

   ----------------------------------------------------------------------------
   procedure Resolve_Rules
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is

      Symbol         : Constant_Symbol_Pointer;
      Current        : Symbol_Vectors.Cursor;
      To_Be_Deleted  : Constant_Symbol_Pointer;
      Updated_Symbol : Constant_Symbol_Pointer;
      Index          : Natural;
      Rule_Hash      : Ada.Containers.Hash_Type := 1;
      Symbol_Rule    : Rule_Pointer;

      use Symbol_Vectors;
      use kv.apg.enum;
      use Ada.Strings.UTF_Encoding;
      use Ada.Strings.UTF_Encoding.Strings;
      use Ada.Containers;
      use Ada.Tags;

   begin
      for Rule of Self.Rules loop
         Rule.My_Hash := Rule_Hash;
         Rule_Hash := Rule_Hash + 1;
         Logger.Note_Info(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "Rule <" & Rule.Name_Token.Get_Data_As_String & ">");
         for Production of Rule.Productions loop
            Logger.Note_Info(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "  Production <" & Decode(To_String(Production.Image), UTF_8) & ">");
            Production.Set_Rule(Rule);
            Current := First(Production.Symbols);
            while Current /= No_Element loop
               Symbol := Symbol_Vectors.Element(Current);
               -- update the element from a Pre_Symbol_Class to either a Terminal_Class (token) or a Non_Terminal_Class (rule)
               -- log an error if the element does not resolve
               if Self.Find_Non_Terminal(Symbol.Token.Get_Data) /= null then
                  Logger.Note_Debug(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "    Symbol <" & Symbol.Token.Get_Data_As_String & "> is a nonterminal (rule)");
                  To_Be_Deleted := Symbol;
                  Symbol_Rule := Self.Find_Non_Terminal(Symbol.Token.Get_Data);
                  Updated_Symbol := New_Non_Terminal_Symbol(Symbol_Rule.Get_Token, Symbol_Rule.Get_Number);
                  Collect_Grammar_Symbol(Self, Updated_Symbol);
                  Production.Symbols.Replace_Element(Current, Updated_Symbol);
                  Free(To_Be_Deleted);
               elsif Self.Find_Terminal(Symbol.Token.Get_Data) /= kv.apg.enum.Not_Found_Error then
                  Logger.Note_Debug(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "    Symbol <" & Symbol.Token.Get_Data_As_String & "> is a terminal (token)");
                  Index := Self.Find_Terminal(Symbol.Token.Get_Data);
                  if Self.Tokens.Get_Precedence(Index) > Production.Get_Precedence then
                     Logger.Note_Debug(Symbol.Token.Get_Location, Symbol.Token.Get_Data, "    Increase production precedence to " & Token_Precedence_Type'IMAGE(Self.Tokens.Get_Precedence(Index)));
                     Production.Precedence := Self.Tokens.Get_Precedence(Index);
                  end if;
                  To_Be_Deleted := Symbol;
                  Updated_Symbol := new Terminal_Class'(Token => Self.Tokens.Get(Index), Key => kv.apg.fast.Key_Type(Index));
                  Collect_Grammar_Symbol(Self, Updated_Symbol);
                  Production.Symbols.Replace_Element(Current, Updated_Symbol);
                  Free(To_Be_Deleted);
               else
                  if Symbol.all'TAG = Pre_Symbol_Class'TAG then
                     Logger.Note_Debug(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "    Symbol <" & Symbol.Token.Get_Data_As_String & "> is undefined (an error)");
                     Logger.Note_Error(Symbol.Token.Get_Location, Symbol.Token.Get_Data, "Symbol of production rule """ & Rule.Name_Token.Get_Data_As_String & """ not found.");
                     Self.Errors := Self.Errors + 1;
                  else
                     Logger.Note_Debug(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "    Symbol <" & Symbol.Token.Get_Data_As_String & "> was previously defined in place.");
                     -- Including the following line will cause the End_Of_File symbol to be in the collection
                     Collect_Grammar_Symbol(Self, Symbol);
                  end if;
               end if;
               Next(Current);
            end loop;
         end loop;
      end loop;
   end Resolve_Rules;


   ----------------------------------------------------------------------------
   function Img(Arg : Terminal_Index_Type) return String is --renames Terminal_Index_Type'IMAGE;
   begin
      return Terminal_Index_Type'IMAGE(Arg);
   end Img;

   ----------------------------------------------------------------------------
   procedure Resolve_Productions
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is

      type Can_Vanish_Answer_Type is (Yes, No, Maybe);
      type Candidate_List_Type is array (Can_Vanish_Answer_Type) of Production_Vectors.Vector;

      List_Of : Candidate_List_Type;

      use Production_Vectors;
      use Ada.Containers;

      -------------------------------------------------------------------------
      function Rule_Has_A_Yes(Rule : Rule_Pointer) return Boolean is
      begin
         for Production of Rule.Productions loop
            if List_Of(Yes).Contains(Production) then
               return True;
            end if;
         end loop;
         return False;
      end Rule_Has_A_Yes;

      -------------------------------------------------------------------------
      function Rule_Is_All_No(Rule : Rule_Pointer) return Boolean is
      begin
         for Production of Rule.Productions loop
            if not List_Of(No).Contains(Production) then
               return False;
            end if;
         end loop;
         return True;
      end Rule_Is_All_No;

      -------------------------------------------------------------------------
      function Cursory_Check(Production : Production_Pointer) return Can_Vanish_Answer_Type is
      begin
         if Production.Matches_An_Empty_Sequence then
            return Yes;
         elsif Production.Has_A_Terminal then
            return No;
         end if;
         return Maybe;
      end Cursory_Check;

      -------------------------------------------------------------------------
      function Symbol_By_Symbol_Check(Production : Production_Pointer) return Can_Vanish_Answer_Type is
      begin
         for Symbol of Production.Symbols loop
            if Self.Rule_Of(Symbol).Has_An_Empty_Sequence or else Rule_Has_A_Yes(Self.Rule_Of(Symbol)) then
               null; -- Need to check the rest of the elements
            else
               if Rule_Is_All_No(Self.Rule_Of(Symbol)) then
                  return No;
               else
                  return Maybe; -- We need to resolve more non terminals before we can be sure
               end if;
            end if;
         end loop;
         return Yes;
      end Symbol_By_Symbol_Check;

      -------------------------------------------------------------------------
      procedure Disposition
         (Production : in     Production_Pointer;
          Answer     : in     Can_Vanish_Answer_Type) is
      begin
         Production.Vanishable := (Answer = Yes);
         List_Of(Answer).Append(Production);
      end Disposition;

      -------------------------------------------------------------------------
      function Pop_Unresolved_Production return Production_Pointer is
         Top : Production_Pointer;
      begin
         Top := First_Element(List_Of(Maybe));
         Delete_First(List_Of(Maybe));
         return Top;
      end Pop_Unresolved_Production;

      -------------------------------------------------------------------------
      procedure Number
         (Production : in     Production_Pointer) is
      begin
         Self.Max_P := Self.Max_P + 1;
         Production.Number := Production_Index_Type(Self.Max_P);
      end Number;

      Current    : Production_Pointer;
      Can_Vanish : Can_Vanish_Answer_Type;

   begin
      Self.Max_P := 0;
      for Rule of Self.Rules loop
         for Production of Rule.Productions loop
            Number(Production);
            Can_Vanish := Cursory_Check(Production);
            Disposition(Production, Can_Vanish);
         end loop;
      end loop;
      for I in 1 .. Production_Vectors.Length(List_Of(Maybe)) ** 2 loop
         Current := Pop_Unresolved_Production;
         Can_Vanish := Symbol_By_Symbol_Check(Current);
         Disposition(Current, Can_Vanish);
      exit when Production_Vectors.Is_Empty(List_Of(Maybe));
      end loop;
      for Production of List_Of(Maybe) loop
         Production.Vanishable := False; -- Productions that cycle around without clearly resolving to true or false must be false (for lack of true).
         Self.Errors := Self.Errors + 1;
         Logger.Note_Error(Production.Get_Rule.Name_Token.Get_Location, Production.Image, "Production could not be resolved!");
      end loop;
   end Resolve_Productions;


   ----------------------------------------------------------------------------
   function Image(Self : Terminal_Sets.Set) return String_Type is
      Answer : String_Type := To_String_Type("{");
      use Terminal_Sets;
   begin
      for Current of Self loop
         Answer := Answer & To_String_Type(",") & To_String_Type(Img(Current));
      end loop;
      return Answer & To_String_Type("}");
   end Image;


   ----------------------------------------------------------------------------
   function To_S(St : String_Type) return String is
   begin
      return Decode(To_String(St), UTF_8);
   end To_S;


   ----------------------------------------------------------------------------
   package Rule_Deps is new Ada.Containers.Hashed_Sets
      (Element_Type => Rule_Pointer,
       Hash => Hash,
       Equivalent_Elements => Equivalent_Elements);

   ----------------------------------------------------------------------------
   package Dep_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type => Rule_Pointer,
       Element_Type => Rule_Deps.Set,
       Hash => Hash,
       Equivalent_Keys => Equivalent_Elements,
       "=" => Rule_Deps."=");

   ----------------------------------------------------------------------------
   procedure Add_Dependency
      (Dependencies : in out Dep_Maps.Map;
       Source       : in     Rule_Pointer;
       Destination  : in     Rule_Pointer) is
      Working : Rule_Deps.Set;
   begin
      if Source = Destination then
         return;
      end if;
      --Put_Line("Rule "&Decode(To_String(Source.Get_Name), UTF_8)&" passes its First terminals to rule " & Decode(To_String(Destination.Get_Name), UTF_8) & ".");
      if Dependencies.Contains(Source) then
         Working := Dependencies.Element(Source);
      end if;
      Working.Include(Destination);
      Dependencies.Include(Source, Working);
   end Add_Dependency;

   ----------------------------------------------------------------------------
   generic
      with procedure Transfer
         (Source      : in     Rule_Pointer;
          Destination : in     Rule_Pointer);
   procedure Propogate_Dependencies
      (Dependencies : in out Dep_Maps.Map);

   ----------------------------------------------------------------------------
   procedure Propogate_Dependencies
      (Dependencies : in out Dep_Maps.Map) is

      use Dep_Maps;

      Current : Dep_Maps.Cursor;
      Source  : Rule_Pointer;

   begin
      for I in 1..2 loop -- Two passes will propagate all terminals -- TODO: is this correct?
         Current := Dependencies.First;
         while Current /= No_Element loop
            Source := Key(Current);
            for Destination of Dependencies.Element(Source) loop
               Transfer(Source, Destination);
            end loop;
            Current := Next(Current);
         end loop;
      end loop;
   end Propogate_Dependencies;

   ----------------------------------------------------------------------------
   procedure Transfer_First
      (Source      : in     Rule_Pointer;
       Destination : in     Rule_Pointer) is
   begin
      Destination.Firsts.Union(Source.Firsts);
   end Transfer_First;

   ----------------------------------------------------------------------------
   procedure Transfer_Follow
      (Source      : in     Rule_Pointer;
       Destination : in     Rule_Pointer) is
   begin
      Destination.Follows.Union(Source.Follows);
   end Transfer_Follow;

   ----------------------------------------------------------------------------
   procedure Propogate_First_Dependencies is new Propogate_Dependencies(Transfer => Transfer_First);

   ----------------------------------------------------------------------------
   procedure Propogate_Follow_Dependencies is new Propogate_Dependencies(Transfer => Transfer_Follow);


   ----------------------------------------------------------------------------
   function First_Of(Self : Grammar_Class; Symbol : Constant_Symbol_Pointer) return Terminal_Sets.Set is
      Answer : Terminal_Sets.Set;
   begin
      if Symbol.Is_Terminal then
         Answer.Insert(Terminal_Index_Type(Symbol.Get_Index));
         return Answer;
      else
         return Self.Rule_Of(Symbol).First;
      end if;
   end First_Of;

   ----------------------------------------------------------------------------
   procedure Resolve_Firsts
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is

      -------------------------------------------------------------------------
      procedure Collect_Terminals_And_Non_Terminal_Dependencies
         (Dependencies : in out Dep_Maps.Map) is
      begin
         Rule_Loop: for Rule of Self.Rules loop
            Production_Loop: for Production of Rule.Productions loop
               Symbol_Loop: for Symbol of Production.Symbols loop
                  if Symbol.Is_Terminal then
                     Rule.Firsts.Union(Self.First_Of(Symbol));
                     exit Symbol_Loop;
                  else -- is nonterminal
                     -- Rule depends on Symbol, but that can't be resolved just now.
                     Add_Dependency(Dependencies, Self.Rule_Of(Symbol), Rule);
                     if not Self.Rule_Of(Symbol).Can_Disappear then
                        exit Symbol_Loop;
                     end if;
                  end if;
               end loop Symbol_Loop;
            end loop Production_Loop;
         end loop Rule_Loop;
      end Collect_Terminals_And_Non_Terminal_Dependencies;

      -------------------------------------------------------------------------
      procedure Add_Non_Propogatable_Terminals is
      begin
         for Rule of Self.Rules loop
            if Rule.Can_Disappear then
               Rule.Firsts.Include(Epsilon);
            end if;
         end loop;
      end Add_Non_Propogatable_Terminals;

      Dependencies : Dep_Maps.Map;

   begin
      Collect_Terminals_And_Non_Terminal_Dependencies(Dependencies);
      Propogate_First_Dependencies(Dependencies);
      Add_Non_Propogatable_Terminals;
   end Resolve_Firsts;


   ----------------------------------------------------------------------------
   procedure Resolve_Follows
      (Self    : in out Grammar_Class;
       Add_EOF : in     Boolean;
       Logger  : in     kv.apg.logger.Safe_Logger_Pointer) is

      -------------------------------------------------------------------------
      procedure Add_Endmarkers is
      begin
         for Rule of Self.Rules loop
            if Rule.Start_Rule then
               Rule.Follows.Include(End_Of_File);
            end if;
         end loop;
      end Add_Endmarkers;

      -------------------------------------------------------------------------
      procedure Add_Sans_Epsilon
         (Receiver : in     Rule_Pointer;
          Source   : in     Constant_Symbol_Pointer) is
         Working : Terminal_Sets.Set;
      begin
         Working := Self.First_Of(Source);
         Working.Exclude(Epsilon); -- Remove Epsilon
         --Put_Line("Adding First of " & To_S(Source.Name) & To_S(Image(Working)) & " to " & To_S(Receiver.Get_Name) & To_S(Image(Receiver.Follows)));
         Receiver.Follows.Union(Working);
      end Add_Sans_Epsilon;

      -------------------------------------------------------------------------
      procedure Collect_Following_Firsts is
         Symbol_Count : Natural;
         Symbol       : Constant_Symbol_Pointer;
      begin
         for Rule of Self.Rules loop
            Production_Loop: for Production of Rule.Productions loop
               Symbol_Count := Production.Symbol_Count;
               if Symbol_Count = 0 then
                  exit Production_Loop;
               end if;
               for Index in 1 .. Symbol_Count-1 loop
                  Symbol := Production.Symbols(Index);
                  if not Symbol.Is_Terminal then
                     Add_Sans_Epsilon(Self.Rule_Of(Symbol), Production.Symbols(Index + 1));
                  end if;
               end loop;
            end loop Production_Loop;
         end loop;
      end Collect_Following_Firsts;

      -------------------------------------------------------------------------
      procedure Collect_Dependencies
         (Dependencies : in out Dep_Maps.Map) is

         Symbol_Count : Natural;
         Symbol       : Constant_Symbol_Pointer;

      begin
         for Rule of Self.Rules loop
            Production_Loop: for Production of Rule.Productions loop
               Symbol_Count := Production.Symbol_Count;
               if Symbol_Count = 0 then
                  exit Production_Loop;
               end if;
               for Index in reverse 1 .. Symbol_Count loop
                  Symbol := Production.Symbols(Index);
                  if Symbol.Is_Terminal then
                     exit Production_Loop;
                  end if;
                  if Self.Rule_Of(Symbol) = Rule then
                     exit Production_Loop; -- Recursive production
                  end if;
                  Add_Dependency(Dependencies, Rule, Self.Rule_Of(Symbol));
                  --Put_Line("Rule "&To_S(Rule.Get_Name)&" passes its Follow terminals to rule " & To_S(Symbol.Name) & ".");
                  if not Self.Rule_Of(Symbol).Can_Disappear then
                     exit Production_Loop;
                  end if;
               end loop;
            end loop Production_Loop;
         end loop;
      end Collect_Dependencies;

      Dependencies : Dep_Maps.Map;

   begin
      if Add_EOF then
         Add_Endmarkers;
      end if;
      Collect_Following_Firsts;
      Collect_Dependencies(Dependencies);
      Propogate_Follow_Dependencies(Dependencies);
   end Resolve_Follows;


   ----------------------------------------------------------------------------
   procedure Validate
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is

      Start_Count : Natural := 0;

   begin
      for Rule of Self.Rules loop
         if Rule.Is_Start then
            Start_Count := Start_Count + 1;
         end if;
      end loop;
      if Start_Count /= 1 then
         Self.Errors := Self.Errors + 1;
         Logger.Note_By_Severity(Error, "Expected one rule flagged as the start rule, found" & Natural'IMAGE(Start_Count) & ".");
      end if;
   end Validate;


   ----------------------------------------------------------------------------
   function Rule_Of(Self : Grammar_Class; Symbol : Constant_Symbol_Pointer) return Rule_Pointer is
   begin
      for R of Self.Rules loop
         if not Symbol.Is_Terminal and then Integer(R.Get_Number) = Non_Terminal_Class(Symbol.all).Get_Index then
            return R;
         end if;
      end loop;
      return null;
   end Rule_Of;

   ----------------------------------------------------------------------------
   function Find_Non_Terminal(Self : Grammar_Class; Name : String_Type) return Rule_Pointer is
      use Ada.Strings.UTF_Encoding;
      use Ada.Strings.UTF_Encoding.Strings;
      Key : constant String := Decode(To_String(Name), UTF_8);
      use Rule_Maps;
   begin
      if Self.Rules.Find(Key) /= No_Element then
         return Self.Rules.Element(Key);
      end if;
      return null;
   end Find_Non_Terminal;

   ----------------------------------------------------------------------------
   function Find_Terminal(Self : Grammar_Class; Name : String_Type) return Integer is
   begin
      return Self.Tokens.Get(Name);
   end Find_Terminal;

   ----------------------------------------------------------------------------
   function Production_Count(Self : Grammar_Class; Name : String_Type) return Natural is
      Rule : Rule_Pointer;
   begin
      Rule := Self.Find_Non_Terminal(Name);
      if Rule = null then
         raise Rule_Not_Found_Error;
      end if;
      return Rule.Production_Count;
   end Production_Count;

   ----------------------------------------------------------------------------
   function Symbol_Count(Self : Grammar_Class; Name : String_Type; Production : Positive) return Natural is
      Rule : Rule_Pointer;
   begin
      Rule := Self.Find_Non_Terminal(Name);
      if Rule = null then
         raise Rule_Not_Found_Error;
      end if;
      if Production > Rule.Production_Count then
         raise Production_Not_Found_Error;
      end if;
      return Rule.Get_Production(Production).Symbol_Count;
   end Symbol_Count;


   ----------------------------------------------------------------------------
   function Get_Symbol(Self : Grammar_Class; Name : String_Type; Production : Positive; Symbol : Positive) return Constant_Symbol_Pointer is
      Rule : Rule_Pointer;
   begin
      Rule := Self.Find_Non_Terminal(Name);
      if Rule = null then
         raise Rule_Not_Found_Error;
      end if;
      if Production > Rule.Production_Count then
         raise Production_Not_Found_Error;
      end if;
      return Constant_Symbol_Pointer(Rule.Get_Production(Production).Get_Symbol(Symbol));
   end Get_Symbol;


   ----------------------------------------------------------------------------
   function Get_Production(Self : Grammar_Class; Number : Production_Index_Type) return Production_Pointer is
   begin
      for Rule of Self.Rules loop
         for Production of Rule.Productions loop
            if Production.Get_Number = Number then
               return Production;
            end if;
         end loop;
      end loop;
      return null;
   end Get_Production;

   ----------------------------------------------------------------------------
   function Get_Rule(Self : Grammar_Class; Number : Non_Terminal_Index_Type) return Rule_Pointer is
   begin
      for Rule of Self.Rules loop
         if Rule.Get_Number = Number then
            return Rule;
         end if;
      end loop;
      return null;
   end Get_Rule;

   ----------------------------------------------------------------------------
   function Rule_Number_Lo(Self : Grammar_Class) return Non_Terminal_Index_Type is
   begin
      return 1;
   end Rule_Number_Lo;

   ----------------------------------------------------------------------------
   function Rule_Number_Hi(Self : Grammar_Class) return Non_Terminal_Index_Type is
   begin
      return Non_Terminal_Index_Type(Self.Count);
   end Rule_Number_Hi;

   ----------------------------------------------------------------------------
   function Production_Number_Lo(Self : Grammar_Class) return Production_Index_Type is
   begin
      return 1;
   end Production_Number_Lo;

   ----------------------------------------------------------------------------
   function Production_Number_Hi(Self : Grammar_Class) return Production_Index_Type is
   begin
      return Production_Index_Type(Self.Max_P);
   end Production_Number_Hi;

   ----------------------------------------------------------------------------
   function Terminal_Lo(Self : Grammar_Class) return Terminal_Index_Type is
   begin
      return End_Of_File;
   end Terminal_Lo;

   ----------------------------------------------------------------------------
   function Terminal_Hi(Self : Grammar_Class) return Terminal_Index_Type is
   begin
      return Terminal_Index_Type(Self.Tokens.Get_Count);
   end Terminal_Hi;


   ----------------------------------------------------------------------------
   function Grammar_Symbols(Self : Grammar_Class) return Symbol_Vectors.Vector is
   begin
      return Self.All_Symbols;
   end Grammar_Symbols;

   ----------------------------------------------------------------------------
   function Translate(Self : Grammar_Class; Terminal : Terminal_Index_Type) return Constant_Symbol_Pointer is
   begin
      for S of Self.All_Symbols loop
         if S.Is_Terminal then
            if S.Get_Number = Terminal then
               return S;
            end if;
         end if;
      end loop;
      return null;
   end Translate;

   ----------------------------------------------------------------------------
   function Translate(Self : Grammar_Class; Terminals : Terminal_Sets.Set) return Symbol_Vectors.Vector is
      Answer : Symbol_Vectors.Vector;
   begin
      for T of Terminals loop
         Answer.Append(Self.Translate(T));
      end loop;
      return Answer;
   end Translate;


   ----------------------------------------------------------------------------
   function First_Kernel_Set(Self : Grammar_Class) return Item_Sets.Set is
      Answer : Item_Sets.Set;
      S : Rule_Pointer := Self.Find_Start;
      K : Constant_Item_Pointer;
   begin
      K := New_Kernel_Class(Constant_Production_Pointer(S.Get_Production(1)), 0);
      Answer.Insert(K);
      return Answer;
   end First_Kernel_Set;

   ----------------------------------------------------------------------------
   function Closure -- We are using kernels and closures expand kernels into items
      (Self   : Grammar_Class;
       Kernel : Item_Sets.Set;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return Item_Sets.Set is
   begin
      return Kernel;
   end Closure;

   ----------------------------------------------------------------------------
   function Goto_Step_Over_One -- goto(I, X)
      (Self   : Grammar_Class;
       Kernel : Constant_Item_Pointer;
       Index  : State_Index_Type;
       Symbol : Constant_Symbol_Pointer;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return Item_Sets.Set is

      Answer : Item_Sets.Set;

   begin
      if Kernel.Has_Next and then Symbol.Is_Same_As(Kernel.Get_Big_B.all) then
         Logger.Note_By_Severity(Debug, "Add next kernel for: " & To_String(Kernel.Image));
         Answer.Insert(Kernel.New_Next_Kernel_Class);
      end if;
      return Answer;
   end Goto_Step_Over_One;

   ----------------------------------------------------------------------------
   function Goto_Step_Over -- goto(I, X)
      (Self   : Grammar_Class;
       Kernel : Item_Sets.Set;
       Index  : State_Index_Type;
       Symbol : Constant_Symbol_Pointer;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return Item_Sets.Set is

      Answer : Item_Sets.Set;

   begin
      Logger.Note_By_Severity(Debug, "Goto("&Img(Index)&", " & To_String(Symbol.Name) & ") -- over.");
      for Item of Kernel loop
         Logger.Note_By_Severity(Debug, "Processing kernel item: " & To_String(Item.Image));
         Answer.Union(Self.Goto_Step_Over_One(Item, Index, Symbol, Logger));
      end loop;
      return Answer;
   end Goto_Step_Over;

   type Rule_Flags_Type is array (Non_Terminal_Index_Type range <>) of Boolean;

   ----------------------------------------------------------------------------
   package Rule_Queues is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Rule_Pointer);

   ----------------------------------------------------------------------------
   function Goto_Step_Into_One -- goto(I, X)
      (Self   : Grammar_Class;
       Kernel : Constant_Item_Pointer;
       Index  : State_Index_Type;
       Symbol : Constant_Symbol_Pointer;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return Item_Sets.Set is

      Answer : Item_Sets.Set;
      Next : Constant_Symbol_Pointer;
      Rule : Rule_Pointer;
      First : Rule_Pointer;
      Follow_Up : Rule_Queues.Vector;

      Examined : Rule_Flags_Type(Self.Rule_Number_Lo .. Self.Rule_Number_Hi) := (others => False);

   begin
      if Kernel.Has_Next then
         Next := Kernel.Get_Big_B;
         if not Next.Is_Terminal then
            Logger.Note_By_Severity(Debug, "Next is non-terminal: " & To_String(Next.Name));
            Rule := Self.Find_Non_Terminal(Next.Name);
            while Rule /= null loop
               Examined(Rule.Get_Number) := True;
               Logger.Note_By_Severity(Debug, "Processing rule: " & To_String(Rule.Get_Name));
               for Production of Rule.Productions loop
                  Logger.Note_By_Severity(Debug, "Checking production: " & To_String(Production.Image));
                  if Production.Symbol_Count > 0 then -- if production isn't empty
                     if Symbol.Is_Same_As(Production.Get_Symbol(1).all) then
                        Logger.Note_By_Severity(Debug, "Add new kernel for: " & To_String(Production.Image));
                        Answer.Insert(New_Kernel_Class(Constant_Production_Pointer(Production), 1));
                     end if;
                     -- * right most => ; but this is really a left most dive
                     if not Production.Get_Symbol(1).Is_Terminal then
                        First := Self.Rule_Of(Production.Get_Symbol(1));
                        if not Examined(First.Get_Number) then
                           Logger.Note_By_Severity(Debug, "Follow this up: " & To_String(First.Get_Name));
                           Follow_Up.Append(First);
                        end if;
                        -- TODO: ??? while can disappear keep moving right and going into
                     end if;
                  end if;
               end loop;
               Rule := null;
               if not Follow_Up.Is_Empty then
                  Rule := Follow_Up.First_Element;
                  Follow_Up.Delete_First;
               end if;
            end loop;
         end if;
      end if;
      return Answer;
   end Goto_Step_Into_One;

   ----------------------------------------------------------------------------
   function Goto_Step_Into -- goto(I, X)
      (Self   : Grammar_Class;
       Kernel : Item_Sets.Set;
       Index  : State_Index_Type;
       Symbol : Constant_Symbol_Pointer;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return Item_Sets.Set is

      Answer : Item_Sets.Set;

   begin
      Logger.Note_By_Severity(Debug, "Goto("&Img(Index)&" " & To_String(Symbol.Name) & ") -- into.");
      for Item of Kernel loop
         Logger.Note_By_Severity(Debug, "Processing kernel item: " & To_String(Item.Image));
         Answer.Union(Self.Goto_Step_Into_One(Item, Index, Symbol, Logger));
      end loop;
      return Answer;
   end Goto_Step_Into;


   ----------------------------------------------------------------------------
   function Img(I : State_Index_Type) return String is
      Answer : String := State_Index_Type'IMAGE(I);
   begin
      Answer(1) := 'I';
      return Answer;
   end Img;

   ----------------------------------------------------------------------------
   function Generate_Parser_States(Self : Grammar_Class; Logger : kv.apg.logger.Safe_Logger_Pointer) return State_Information_Type is

      Answer : State_Information_Type;
      Count : State_Index_Type := 0;
      Current : State_Index_Type := 0;
      Found_More : Boolean := False;
      Hint : Action_Hint_Type;

      -------------------------------------------------------------------------
      procedure Add_State(Kernels : in Item_Sets.Set) is
         use Item_Sets;

         State_Def : State_Definition_Type;

      begin
         for S in State_Index_Type(0) .. Count-1 loop
            if Answer.States(S).Kernels = Kernels then
               Logger.Note_By_Severity(Debug, "Not adding state definition because it matches: " & Img(S));
               Hint.To_State := S;
               State_Def := Answer.States(S);
               Answer.Hints.Append(Hint);
               Answer.States.Replace_Element(S, State_Def);
               return;
            end if;
         end loop;
         Logger.Note_By_Severity(Debug, "Adding state definition for: " & Img(Count));
         Hint.To_State := Count;
         State_Def.Index := Count;
         State_Def.Kernels := Kernels;
         Answer.Hints.Append(Hint);
         Answer.States.Append(State_Def);
         Count := Count + 1;
      end Add_State;

      -------------------------------------------------------------------------
      procedure Process_Source_Kernels
         (Symbol : in Constant_Symbol_Pointer;
          Kernels : in Item_Sets.Set) is

         Working : Item_Sets.Set;

      begin
         for K of Kernels loop -- Constant_Item_Pointer
            Working.Union(Self.Goto_Step_Over_One(K, Current, Symbol, Logger));
            Working.Union(Self.Goto_Step_Into_One(K, Current, Symbol, Logger));
         end loop;
         if not Working.Is_Empty then
            Add_State(Working);
            Found_More := True;
         end if;
      end Process_Source_Kernels;

      K : Item_Sets.Set;

   begin
      Hint.From_State := 0;
      Add_State(Self.First_Kernel_Set);
      loop
         Logger.Note_By_Severity(Debug, "Processing " & Img(Current));
         Hint.From_State := Current;
         for Symbol of Self.All_Symbols loop
            Hint.Symbol := Symbol;
            K := Answer.States.Constant_Reference(Current).Kernels;
            Process_Source_Kernels(Symbol, K);
         end loop;
         Current := Current + 1;
      exit when Current > Count; -- Should not be able to get here, but protect against pathological case
      exit when (not Found_More) and (Current = Count);
         Found_More := False;
      end loop;
      return Answer;
   end Generate_Parser_States;

end kv.apg.lalr.grammars;
