with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;

with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with kv.apg.locations;

package body kv.apg.rules is

   use Ada.Strings.UTF_Encoding;
   use Ada.Strings.UTF_Encoding.Strings;

   procedure Free_Instance is new Ada.Unchecked_Deallocation(Symbol_Class'CLASS, Symbol_Pointer);

   ----------------------------------------------------------------------------
   function New_Pre_Symbol
      (Token : in     kv.apg.tokens.Token_Class) return Symbol_Pointer is
   begin
      return new Pre_Symbol_Class'(Token => Token);
   end New_Pre_Symbol;

   ----------------------------------------------------------------------------
   procedure Free
      (Symbol : in out Symbol_Pointer) is
   begin
      Free_Instance(Symbol);
   end Free;


   ----------------------------------------------------------------------------
   function Is_Same_As(Self : Terminal_Class; Other : Symbol_Class'CLASS) return Boolean is
      use kv.apg.fast; -- "="
   begin
      return (Other.Is_Terminal) and then Terminal_Class(Other).Key = Self.Key;
   end Is_Same_As;

   ----------------------------------------------------------------------------
   function First(Self : Terminal_Class) return Terminal_Sets.Set is
      Answer : Terminal_Sets.Set;
   begin
      Answer.Insert(Terminal_Type(Self.Key));
      return Answer;
   end First;

   ----------------------------------------------------------------------------
   function Name(Self : Symbol_Class) return String_Type is
   begin
      return Self.Token.Get_Data;
   end Name;

   ----------------------------------------------------------------------------
   function Can_Disappear(Self : Non_Terminal_Class) return Boolean is
   begin
      return Self.Rule.Can_Disappear;
   end Can_Disappear;

   ----------------------------------------------------------------------------
   function Is_Same_As(Self : Non_Terminal_Class; Other : Symbol_Class'CLASS) return Boolean is
   begin
      return (not Other.Is_Terminal) and then Non_Terminal_Class(Other).Rule = Self.Rule;
   end Is_Same_As;

   ----------------------------------------------------------------------------
   function First(Self : Non_Terminal_Class) return Terminal_Sets.Set is
   begin
      return Self.Rule.First; --TODO: could be recursive loop
   end First;




   ----------------------------------------------------------------------------
   function Can_Disappear(Self : Pre_Symbol_Class) return Boolean is
   begin
      raise Unresolved_Error;
      return False;
   end Can_Disappear;

   ----------------------------------------------------------------------------
   function Is_Terminal(Self : Pre_Symbol_Class) return Boolean is
   begin
      raise Unresolved_Error;
      return False;
   end Is_Terminal;

   ----------------------------------------------------------------------------
   function Is_Same_As(Self : Pre_Symbol_Class; Other : Symbol_Class'CLASS) return Boolean is
   begin
      raise Unresolved_Error;
      return False;
   end Is_Same_As;

   ----------------------------------------------------------------------------
   function First(Self : Pre_Symbol_Class) return Terminal_Sets.Set is
      Answer : Terminal_Sets.Set;
   begin
      raise Unresolved_Error;
      return Answer;
   end First;




   ----------------------------------------------------------------------------
   function New_Production_Class return Production_Pointer is
      Self : Production_Pointer;
   begin
      Self := new Production_Class;
      Self.Clear;
      return Self;
   end New_Production_Class;

   ----------------------------------------------------------------------------
   procedure Append
      (Self   : in out Production_Class;
       Symbol : in     Symbol_Pointer) is
   begin
      Self.Symbols.Append(Symbol);
   end Append;

   ----------------------------------------------------------------------------
   function Symbol_Count(Self : Production_Class) return Natural is
   begin
      return Natural(Symbol_Vector.Length(Self.Symbols));
   end Symbol_Count;

   ----------------------------------------------------------------------------
   function Get_Symbol(Self : Production_Class; Symbol : Positive) return Constant_Symbol_Pointer is
      Sp : Symbol_Pointer;
   begin
      Sp := Self.Symbols(Symbol);
      return Constant_Symbol_Pointer(Sp);
   end Get_Symbol;

   ----------------------------------------------------------------------------
   function Image(Self : Production_Class) return String_Type is
      Answer : String_Type := To_String_Type("(");
   begin
      for Symbol of Self.Symbols loop
         Answer := Answer & To_String_Type(" ") & Symbol.Token.Get_Data;
      end loop;
      if Self.Rule /= null then
         return Self.Rule.Get_Name & To_String_Type(" -> ") & Answer & To_String_Type(" ) => ") & Self.Code;
      else
         return Answer & To_String_Type(" ) => ") & Self.Code;
      end if;
   end Image;

   ----------------------------------------------------------------------------
   procedure Clear
      (Self : in out Production_Class) is
   begin
      Self.Symbols := Symbol_Vector.Empty_Vector;
      Self.Code := Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String;
   end Clear;

   ----------------------------------------------------------------------------
   procedure Set_Code
      (Self : in out Production_Class;
       Code : in     kv.apg.tokens.Token_Class) is
   begin
      Self.Code := Code.Get_Data; --TODO: save whole token
   end Set_Code;

   ----------------------------------------------------------------------------
   function Get_Code(Self : Production_Class) return String_Type is
   begin
      return Self.Code;
   end Get_Code;

   ----------------------------------------------------------------------------
   procedure Set_Rule
      (Self : in out Production_Class;
       Rule : in     Rule_Pointer) is
   begin
      Self.Rule := Rule;
   end Set_Rule;

   ----------------------------------------------------------------------------
   function Get_Rule(Self : Production_Class) return Rule_Pointer is
   begin
      return Self.Rule;
   end Get_Rule;

   ----------------------------------------------------------------------------
   function Matches_An_Empty_Sequence(Self : Production_Class) return Boolean is
   begin
      return Symbol_Vector.Is_Empty(Self.Symbols);
   end Matches_An_Empty_Sequence;

   ----------------------------------------------------------------------------
   function Can_Disappear(Self : Production_Class) return Boolean is
   begin
      return Self.Vanishable;
   end Can_Disappear;

   ----------------------------------------------------------------------------
   function Has_A_Terminal(Self : Production_Class) return Boolean is
   begin
      for Symbol of Self.Symbols loop
         if Symbol.Is_Terminal then
            return True;
         end if;
      end loop;
      return False;
   end Has_A_Terminal;

   ----------------------------------------------------------------------------
   function First(Self : Production_Class) return Terminal_Sets.Set is
      Answer  : Terminal_Sets.Set;
      Current : Terminal_Sets.Set;
   begin
      Put_Line("First of " & Decode(To_String(Self.Image), UTF_8));
      if Self.Vanishable then
         Put_Line("   Add Epsilon");
         Answer.Include(Epsilon);
      end if;
      for Symbol of Self.Symbols loop
         Current := Symbol.First;
         Put_Line("   Add First(" & Decode(To_String(Symbol.Name), UTF_8) & ")");
         Answer.Union(Current);
         if not Current.Contains(Epsilon) then
            exit;
         end if;
      end loop;
      return Answer;
   end First;





   ----------------------------------------------------------------------------
   procedure Initialize
      (Self        : in out Rule_Class;
       Name        : in     kv.apg.tokens.Token_Class;
       Productions : in     Production_Vector.Vector) is
   begin
      Self.Name_Token := Name;
      Self.Productions := Productions;
      Self.Start_Rule := False;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Set_Is_Start
      (Self     : in out Rule_Class;
       Is_Start : in     Boolean) is
   begin
      Self.Start_Rule := Is_Start;
   end Set_Is_Start;

   ----------------------------------------------------------------------------
   function Is_Start(Self : Rule_Class) return Boolean is
   begin
      return Self.Start_Rule;
   end Is_Start;

   ----------------------------------------------------------------------------
   function Get_Name(Self : Rule_Class) return String_Type is
   begin
      return Self.Name_Token.Get_Data;
   end Get_Name;

   ----------------------------------------------------------------------------
   function Production_Count(Self : Rule_Class) return Natural is
   begin
      return Natural(Production_Vector.Length(Self.Productions));
   end Production_Count;

   ----------------------------------------------------------------------------
   function Get_Production(Self : Rule_Class; Production : Positive) return Production_Pointer is
      Pp : Production_Pointer;
   begin
      if Production > Self.Production_Count then
         return null;
      end if;
      Pp := Self.Productions(Production);
      return Pp;
   end Get_Production;


   ----------------------------------------------------------------------------
   function Can_Disappear(Self : Rule_Class) return Boolean is
   begin
      for Production of Self.Productions loop
         if Production.Matches_An_Empty_Sequence or Production.Can_Disappear then
            return True;
         end if;
      end loop;
      return False;
   end Can_Disappear;

   ----------------------------------------------------------------------------
   function Has_An_Empty_Sequence(Self : Rule_Class) return Boolean is
   begin
      for Production of Self.Productions loop
         if Production.Matches_An_Empty_Sequence then
            return True;
         end if;
      end loop;
      return False;
   end Has_An_Empty_Sequence;

   ----------------------------------------------------------------------------
   function First(Self : Rule_Class) return Terminal_Sets.Set is
   begin
      return Self.Firsts;
   end First;

   ----------------------------------------------------------------------------
   function Follow(Self : Rule_Class) return Terminal_Sets.Set is
   begin
      return Self.Follows;
   end Follow;

   ----------------------------------------------------------------------------
   function Hash(Self : Rule_Pointer) return Ada.Containers.Hash_Type is
   begin
      return Self.My_Hash;
   end Hash;

   ----------------------------------------------------------------------------
   function Equivalent_Elements(Left, Right : Rule_Pointer) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Elements;





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
   procedure Add_Rule
      (Self : in out Grammar_Class;
       Rule : in     Rule_Pointer) is
   begin
      Self.Rules.Include(Decode(To_String(Rule.Get_Name), UTF_8), Rule);
   end Add_Rule;

   ----------------------------------------------------------------------------
   procedure Resolve_Rules
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is

      Symbol : Symbol_Pointer;
      Current : Symbol_Vector.Cursor;
      To_Be_Deleted : Symbol_Pointer;
      Updated_Symbol : Symbol_Pointer;
      Index : Natural;
      Rule_Hash : Ada.Containers.Hash_Type := 1;

      use Symbol_Vector;
      use Ada.Strings.UTF_Encoding;
      use Ada.Strings.UTF_Encoding.Strings;
      use Ada.Containers;

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
               Symbol := Symbol_Vector.Element(Current);
               -- update the element from a Pre_Symbol_Class to either a Terminal_Class (token) or a Non_Terminal_Class (rule)
               -- log an error if the element does not resolve
               if Self.Find_Non_Terminal(Symbol.Token.Get_Data) /= null then
                  Logger.Note_Info(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "    Symbol <" & Symbol.Token.Get_Data_As_String & "> is a nonterminal (rule)");
                  To_Be_Deleted := Symbol;
                  Updated_Symbol := new Non_Terminal_Class'(Token => Symbol.Token, Rule => Self.Find_Non_Terminal(Symbol.Token.Get_Data));
                  Production.Symbols.Replace_Element(Current, Updated_Symbol);
                  Free(To_Be_Deleted);
               elsif Self.Find_Terminal(Symbol.Token.Get_Data) /= kv.apg.enum.Not_Found_Error then
                  Logger.Note_Info(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "    Symbol <" & Symbol.Token.Get_Data_As_String & "> is a terminal (token)");
                  Index := Self.Find_Terminal(Symbol.Token.Get_Data);
                  To_Be_Deleted := Symbol;
                  Updated_Symbol := new Terminal_Class'(Token => Self.Tokens.Get(Index), Key => kv.apg.fast.Key_Type(Index));
                  Production.Symbols.Replace_Element(Current, Updated_Symbol);
                  Free(To_Be_Deleted);
               else
                  Logger.Note_Info(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "    Symbol <" & Symbol.Token.Get_Data_As_String & "> is undefined (an error)");
                  Logger.Note_Error(Symbol.Token.Get_Location, Symbol.Token.Get_Data, "Symbol of production rule """ & Rule.Name_Token.Get_Data_As_String & """ not found.");
                  Self.Errors := Self.Errors + 1;
               end if;
               Next(Current);
            end loop;
         end loop;
      end loop;
   end Resolve_Rules;

   ----------------------------------------------------------------------------
   function Rule_Of(Symbol : Symbol_Pointer) return Rule_Pointer is
   begin
      return Non_Terminal_Class(Symbol.all).Rule;
   end Rule_Of;


   ----------------------------------------------------------------------------
   procedure Resolve_Productions
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is

      type Can_Vanish_Answer_Type is (Yes, No, Maybe);
      type Candidate_List_Type is array (Can_Vanish_Answer_Type) of Production_Vector.Vector;

      List_Of : Candidate_List_Type;

      use Production_Vector;
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
            if Rule_Of(Symbol).Has_An_Empty_Sequence or else Rule_Has_A_Yes(Rule_Of(Symbol)) then
               null; -- Need to check the rest of the elements
            else
               if Rule_Is_All_No(Rule_Of(Symbol)) then
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

      Current    : Production_Pointer;
      Can_Vanish : Can_Vanish_Answer_Type;

   begin
      for Rule of Self.Rules loop
         for Production of Rule.Productions loop
            Can_Vanish := Cursory_Check(Production);
            Disposition(Production, Can_Vanish);
         end loop;
      end loop;
      for I in 1 .. Production_Vector.Length(List_Of(Maybe)) ** 2 loop
         Current := Pop_Unresolved_Production;
         Can_Vanish := Symbol_By_Symbol_Check(Current);
         Disposition(Current, Can_Vanish);
      exit when Production_Vector.Is_Empty(List_Of(Maybe));
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
         Answer := Answer & To_String_Type(",") & To_String_Type(Terminal_Type'IMAGE(Current));
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
   procedure Resolve_Firsts
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is

      use Dep_Maps;

      Dependencies : Dep_Maps.Map;

      -------------------------------------------------------------------------
      procedure Collect_Terminals_And_Non_Terminal_Dependencies is
      begin
         Rule_Loop: for Rule of Self.Rules loop
            Production_Loop: for Production of Rule.Productions loop
               Symbol_Loop: for Symbol of Production.Symbols loop
                  if Symbol.Is_Terminal then
                     Rule.Firsts.Union(Symbol.First);
                     exit Symbol_Loop;
                  else -- is nonterminal
                     -- Rule depends on Symbol, but that can't be resolved just now.
                     Add_Dependency(Dependencies, Rule_Of(Symbol), Rule);
                     if not Rule_Of(Symbol).Can_Disappear then
                        exit Symbol_Loop;
                     end if;
                  end if;
               end loop Symbol_Loop;
            end loop Production_Loop;
         end loop Rule_Loop;
      end Collect_Terminals_And_Non_Terminal_Dependencies;

      -------------------------------------------------------------------------
      procedure Propogate_Dependencies is
         Current : Dep_Maps.Cursor;
         Source : Rule_Pointer;
      begin
         for I in 1..2 loop -- Two passes will propagate all terminals -- TODO: is this correct?
            Current := Dependencies.First;
            while Current /= No_Element loop
               Source := Key(Current);
               for Destination of Dependencies.Element(Source) loop
                  --Put_Line("Union "&To_S(Source.Get_Name) & To_S(Image(Source.Firsts)) &" into " & To_S(Destination.Get_Name) & To_S(Image(Destination.Firsts)) & ".");
                  Destination.Firsts.Union(Source.Firsts);
               end loop;
               Current := Next(Current);
            end loop;
         end loop;
      end Propogate_Dependencies;

      -------------------------------------------------------------------------
      procedure Add_Non_Propogatable_Terminals is
      begin
         for Rule of Self.Rules loop
            if Rule.Can_Disappear then
               Rule.Firsts.Include(Epsilon);
            end if;
         end loop;
      end Add_Non_Propogatable_Terminals;

   begin
      Collect_Terminals_And_Non_Terminal_Dependencies;
      Propogate_Dependencies;
      Add_Non_Propogatable_Terminals;
   end Resolve_Firsts;


   ----------------------------------------------------------------------------
   procedure Resolve_Follows
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is

      use Dep_Maps;

      Dependencies : Dep_Maps.Map;

      procedure Add_Endmarkers is
      begin
         for Rule of Self.Rules loop
            if Rule.Start_Rule then
               Rule.Follows.Include(End_Of_File);
            end if;
         end loop;
      end Add_Endmarkers;

      procedure Add_Sans_Epsilon
         (Receiver : in     Rule_Pointer;
          Source   : in     Symbol_Pointer) is
         Working : Terminal_Sets.Set;
      begin
         Working := Source.First;
         -- Remove Epsilon
         Receiver.Follows.Union(Working);
      end Add_Sans_Epsilon;

      procedure Collect_Following_Firsts is
         Symbol_Count : Natural;
         Symbol       : Symbol_Pointer;
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
                     Add_Sans_Epsilon(Rule_Of(Symbol), Production.Symbols(Index + 1));
                  end if;
               end loop;
            end loop Production_Loop;
         end loop;
      end Collect_Following_Firsts;

      procedure Collect_Dependencies is begin null; end;
         --Add_Dependency(Dependencies, Rule_Of(Symbol), Rule);

      procedure Propogate_Dependencies is begin null; end;

   begin
      Add_Endmarkers;
      Collect_Following_Firsts;
      Collect_Dependencies;
      Propogate_Dependencies;
   end Resolve_Follows;


   ----------------------------------------------------------------------------
   procedure Validate
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is

      Start_Count : Natural := 0;
      Location : kv.apg.locations.File_Location_Type;

   begin
      for Rule of Self.Rules loop
         if Rule.Is_Start then
            Start_Count := Start_Count + 1;
         end if;
      end loop;
      if Start_Count /= 1 then
         Self.Errors := Self.Errors + 1;
         Logger.Note_Error(Location, To_String_Type("start"), "Expected one rule flagged as the start rule, found" & Natural'IMAGE(Start_Count) & ".");
      end if;
   end Validate;

   ----------------------------------------------------------------------------

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
   end ;

end kv.apg.rules;
