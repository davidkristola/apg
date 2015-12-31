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

package body kv.apg.rules is

   use Ada.Strings.UTF_Encoding;
   use Ada.Strings.UTF_Encoding.Strings;

   procedure Free_Symbol_Instance is new Ada.Unchecked_Deallocation(Symbol_Class'CLASS, Symbol_Pointer);
   procedure Free_Item_Instance is new Ada.Unchecked_Deallocation(Kernel_Class'CLASS, Item_Pointer);
   function Remove_Constant is new Ada.Unchecked_Conversion(Source => Constant_Item_Pointer, Target => Item_Pointer);
   function Remove_Constant is new Ada.Unchecked_Conversion(Source => Constant_Symbol_Pointer, Target => Symbol_Pointer);

   ----------------------------------------------------------------------------
   function New_Pre_Symbol
      (Token : in     kv.apg.tokens.Token_Class) return Constant_Symbol_Pointer is
   begin
      return new Pre_Symbol_Class'(Token => Token);
   end New_Pre_Symbol;

   ----------------------------------------------------------------------------
   procedure Free
      (Symbol : in out Constant_Symbol_Pointer) is
      Temp : Symbol_Pointer := Remove_Constant(Symbol);
   begin
      Free_Symbol_Instance(Temp);
      Symbol := null;
   end Free;


   ----------------------------------------------------------------------------
   function End_Of_File_Terminal return Terminal_Class is
      T : Terminal_Class;
   begin
      T.Token := kv.apg.tokens.End_Of_File_Token;
      T.Key := End_Of_File;
      return T;
   end End_Of_File_Terminal;

   ----------------------------------------------------------------------------
   function New_End_Of_File_Terminal return Constant_Symbol_Pointer is
      T : access Terminal_Class := new Terminal_Class;
   begin
      T.Token := kv.apg.tokens.End_Of_File_Token;
      T.Key := End_Of_File;
      return Constant_Symbol_Pointer(T);
   end New_End_Of_File_Terminal;

   ----------------------------------------------------------------------------
   function Epsilon_Terminal return Terminal_Class is
      T : Terminal_Class;
   begin
      T.Token := kv.apg.tokens.Epsilon_Token;
      T.Key := Epsilon;
      return T;
   end Epsilon_Terminal;

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
      Answer.Insert(Terminal_Index_Type(Self.Key));
      return Answer;
   end First;

   ----------------------------------------------------------------------------
   function Get_Number(Self : Terminal_Class) return Terminal_Index_Type is
   begin
      return Terminal_Index_Type(Self.Key);
   end Get_Number;

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
   function Get_Number(Self : Non_Terminal_Class) return Terminal_Index_Type is
   begin
      raise Terminal_Expected_Error;
      return Terminal_Index_Type(Epsilon);
   end Get_Number;

   ----------------------------------------------------------------------------
   function New_Non_Terminal_From_Rule(Rule : Rule_Pointer) return Constant_Symbol_Pointer is
      N : access Non_Terminal_Class := new Non_Terminal_Class;
   begin
      N.Token := Rule.Name_Token;
      N.Rule := Rule;
      return Constant_Symbol_Pointer(N);
   end New_Non_Terminal_From_Rule;




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
   function Get_Number(Self : Pre_Symbol_Class) return Terminal_Index_Type is
   begin
      raise Unresolved_Error;
      return Terminal_Index_Type(Epsilon);
   end Get_Number;

   ----------------------------------------------------------------------------
   function "="(L, R : Constant_Symbol_Pointer) return Boolean is
      use Ada.Tags;
   begin
      if L.all'TAG = R.all'TAG then
         return L.all.Is_Same_As(R.all);
      end if;
      return False; -- not the same tag, can't be equal
   end "=";



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
       Symbol : in     Constant_Symbol_Pointer) is
   begin
      Self.Symbols.Append(Symbol);
   end Append;

   ----------------------------------------------------------------------------
   function Symbol_Count(Self : Production_Class) return Natural is
   begin
      return Natural(Symbol_Vectors.Length(Self.Symbols));
   end Symbol_Count;

   ----------------------------------------------------------------------------
   function Get_Symbol(Self : Production_Class; Symbol : Positive) return Constant_Symbol_Pointer is
      Sp : Constant_Symbol_Pointer;
   begin
      if Symbol > Self.Symbol_Count then
         return null;
      end if;
      Sp := Self.Symbols(Symbol);
      return Sp;
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
      Self.Symbols := Symbol_Vectors.Empty_Vector;
      Self.Code := Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String;
      Self.Number := Predicate_Index_Type'LAST;
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
      return Symbol_Vectors.Is_Empty(Self.Symbols);
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
   function Get_Number(Self : Production_Class) return Predicate_Index_Type is
   begin
      if Self.Number = Predicate_Index_Type'LAST then
         raise Unresolved_Error;
      end if;
      return Self.Number;
   end Get_Number;






   ----------------------------------------------------------------------------
   function New_Kernel_Class
      (Production   : Constant_Production_Pointer;
       Dot_Position : Natural) return Constant_Item_Pointer is
      IP : access Kernel_Class;
   begin
      if Dot_Position > Production.Symbol_Count then
         raise Dot_Position_Error;
      end if;
      IP := new Kernel_Class;
      IP.Production := Production;
      IP.Dot_Position := Dot_Position;
      return Constant_Item_Pointer(IP);
   end New_Kernel_Class;

   ----------------------------------------------------------------------------
   function New_Next_Kernel_Class(Self : Kernel_Class) return Constant_Item_Pointer is
   begin
      return New_Kernel_Class(Self.Production, Self.Dot_Position+1);
   end New_Next_Kernel_Class;

   ----------------------------------------------------------------------------
   function New_Item_Class
      (Production   : Constant_Production_Pointer;
       Dot_Position : Natural;
       Terminal     : Constant_Symbol_Pointer) return Constant_Item_Pointer is
      IP : access Item_Class;
   begin
      if not Terminal.Is_Terminal then
         raise Terminal_Expected_Error;
      end if;
      if Dot_Position > Production.Symbol_Count then
         raise Dot_Position_Error;
      end if;
      -- if Terminal is not in Production.Get_Rule.Follow then
      -- raise Non_Following_Terminal_Error;
      -- end if;
      if not Production.Get_Rule.Follow.Contains(Terminal.Get_Number) then
         raise Non_Following_Terminal_Error;
      end if;
      IP := new Item_Class;
      IP.Production := Production;
      IP.Dot_Position := Dot_Position;
      IP.Terminal := Terminal;
      return Constant_Item_Pointer(IP);
   end New_Item_Class;

   ----------------------------------------------------------------------------
   -- The only way to get an Item is to call New_Item_Class which allocates one.
   -- We use the Constant_Item_Pointer to reinforce the idea that items are
   -- immutable.
   --
   procedure Free
      (Item : in out Constant_Item_Pointer) is
      Free_Me : Item_Pointer := Remove_Constant(Item);
   begin
      Free_Item_Instance(Free_Me);
      Item := null;
   end Free;

   ----------------------------------------------------------------------------
   function Core_Image(Self : Kernel_Class'CLASS) return String_Type is
      Answer  : String_Type := To_String_Type("[");
      Symbols : Positive := Self.Production.Symbol_Count;
      Dot_I   : Positive := Self.Dot_Position + 1;
   begin
      Answer := Answer & Self.Production.Get_Rule.Get_Name & To_String_Type(" ->");
      for I in 1 .. Symbols loop
         if I = Dot_I then
            Answer := Answer & To_String_Type(" .");
         end if;
         Answer := Answer & To_String_Type(" ") & Self.Production.Get_Symbol(I).Name;
      end loop;
      if Dot_I > Symbols then
         Answer := Answer & To_String_Type(" .");
      end if;
      return Answer;
   end Core_Image;

   ----------------------------------------------------------------------------
   function Image(Self : Kernel_Class) return String_Type is
   begin
      return Core_Image(Self) & To_String_Type("]");
   end Image;

   ----------------------------------------------------------------------------
   function Image(Self : Item_Class) return String_Type is
   begin
      return Core_Image(Self) & To_String_Type(", ") & Self.Terminal.Name & To_String_Type("]");
   end Image;

   ----------------------------------------------------------------------------
   function Has_Next(Self : Kernel_Class) return Boolean is
   begin
      return (Self.Dot_Position < Self.Production.Symbol_Count);
   end Has_Next;

   ----------------------------------------------------------------------------
   function Get_Big_A(Self : Kernel_Class) return Rule_Pointer is
   begin
      return Self.Production.Get_Rule;
   end Get_Big_A;

   ----------------------------------------------------------------------------
   function Get_Little_Alpha(Self : Kernel_Class) return Constant_Symbol_Pointer is
   begin
      if Self.Dot_Position = 0 then
         return null; -- ɛ
      end if;
      return Self.Production.Get_Symbol(Self.Dot_Position); -- Little alpha is before dot
   end Get_Little_Alpha;

   ----------------------------------------------------------------------------
   function Get_Big_B(Self : Kernel_Class) return Constant_Symbol_Pointer is
      Symbol_After_Dot : Positive := Self.Dot_Position + 1;
   begin
      if not Self.Has_Next then
         return null;
      end if;
      return Self.Production.Get_Symbol(Symbol_After_Dot);
   end Get_Big_B;

   ----------------------------------------------------------------------------
   function Get_Little_Beta(Self : Kernel_Class) return Constant_Symbol_Pointer is
      Little_Beta_Position : Positive := Self.Dot_Position + 2; -- Big B is after dot
   begin
      return Self.Production.Get_Symbol(Little_Beta_Position);
   end Get_Little_Beta;

   ----------------------------------------------------------------------------
   function Get_Little_A(Self : Kernel_Class) return Constant_Symbol_Pointer is
   begin
      return null;
   end Get_Little_A;
   ----------------------------------------------------------------------------
   function Get_Little_A(Self : Item_Class) return Constant_Symbol_Pointer is
   begin
      return Self.Terminal;
   end Get_Little_A;

   ----------------------------------------------------------------------------
   function "<"(L, R : Kernel_Class) return Boolean is
   begin
      if L.Production.Get_Number < R.Production.Get_Number then
         return True;
      end if;
      if L.Production.Get_Number > R.Production.Get_Number then
         return False;
      end if;
      -- Same production
      return L.Dot_Position < R.Dot_Position;
   end "<";
   ----------------------------------------------------------------------------
   function "<"(L, R : Item_Class) return Boolean is
   begin
      if L.Production.Get_Number < R.Production.Get_Number then
         return True;
      end if;
      if L.Production.Get_Number > R.Production.Get_Number then
         return False;
      end if;
      -- Same production
      if L.Terminal.Get_Number < R.Terminal.Get_Number then
         return True;
      end if;
      if L.Terminal.Get_Number > R.Terminal.Get_Number then
         return False;
      end if;
      -- Same terminal
      return L.Dot_Position < R.Dot_Position;
   end "<";

   ----------------------------------------------------------------------------
   function "="(L, R : Kernel_Class) return Boolean is
   begin
      return (L.Production.Get_Number = R.Production.Get_Number) and then (L.Dot_Position = R.Dot_Position);
   end "=";
   ----------------------------------------------------------------------------
   function "="(L, R : Item_Class) return Boolean is
   begin
      return (L.Production.Get_Number = R.Production.Get_Number) and then (L.Terminal.Get_Number = R.Terminal.Get_Number) and then (L.Dot_Position = R.Dot_Position);
   end "=";


   ----------------------------------------------------------------------------
   function "<"(L, R : Constant_Item_Pointer) return Boolean is
      use Ada.Tags;
   begin
      if L.all'TAG = R.all'TAG then
         return L.all < R.all;
      end if;
      return (L.all'TAG = Kernel_Class'TAG);
   end "<";

   ----------------------------------------------------------------------------
   function "="(L, R : Constant_Item_Pointer) return Boolean is
      use Ada.Tags;
   begin
      if L.all'TAG = R.all'TAG then
         return L.all = R.all;
      end if;
      return False; -- not the same tag, can't be equal
   end "=";








   ----------------------------------------------------------------------------
   procedure Initialize
      (Self        : in out Rule_Class;
       Name        : in     kv.apg.tokens.Token_Class;
       Productions : in     Production_Vectors.Vector) is
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
      return Natural(Production_Vectors.Length(Self.Productions));
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
   function Get_Number(Self : Rule_Class) return Non_Terminal_Index_Type is
   begin
      return Self.Number;
   end Get_Number;





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
      Self.Count := Self.Count + 1;
      Rule.Number := Non_Terminal_Index_Type(Self.Count);
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

      Location : kv.apg.locations.File_Location_Type;
      Citation : constant String_Type := To_String_Type("Add_Meta_Rule");

      P : Production_Pointer;
      P_V : Production_Vectors.Vector;
      S : Rule_Pointer;
      R : Rule_Pointer := new Rule_Class;
   begin
      Logger.Note_Debug(Location, Citation, "starting");
      S := Find_Start(Self);
      if S = null then
         Logger.Note_Error(Location, Citation, "No start rule found!");
      end if;
      Logger.Note_Debug(S.Name_Token.Get_Location, S.Name_Token.Get_Data, "Is a start rule");
      P := New_Production_Class;
      P.Append(New_Non_Terminal_From_Rule(S));
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

      Symbol : Constant_Symbol_Pointer;
      Current : Symbol_Vectors.Cursor;
      To_Be_Deleted : Constant_Symbol_Pointer;
      Updated_Symbol : Constant_Symbol_Pointer;
      Index : Natural;
      Rule_Hash : Ada.Containers.Hash_Type := 1;

      use Symbol_Vectors;
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
                  Logger.Note_Info(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "    Symbol <" & Symbol.Token.Get_Data_As_String & "> is a nonterminal (rule)");
                  To_Be_Deleted := Symbol;
                  Updated_Symbol := new Non_Terminal_Class'(Token => Symbol.Token, Rule => Self.Find_Non_Terminal(Symbol.Token.Get_Data));
                  Collect_Grammar_Symbol(Self, Updated_Symbol);
                  Production.Symbols.Replace_Element(Current, Updated_Symbol);
                  Free(To_Be_Deleted);
               elsif Self.Find_Terminal(Symbol.Token.Get_Data) /= kv.apg.enum.Not_Found_Error then
                  Logger.Note_Info(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "    Symbol <" & Symbol.Token.Get_Data_As_String & "> is a terminal (token)");
                  Index := Self.Find_Terminal(Symbol.Token.Get_Data);
                  To_Be_Deleted := Symbol;
                  Updated_Symbol := new Terminal_Class'(Token => Self.Tokens.Get(Index), Key => kv.apg.fast.Key_Type(Index));
                  Collect_Grammar_Symbol(Self, Updated_Symbol);
                  Production.Symbols.Replace_Element(Current, Updated_Symbol);
                  Free(To_Be_Deleted);
               else
                  if Symbol.all'TAG = Pre_Symbol_Class'TAG then
                     Logger.Note_Info(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "    Symbol <" & Symbol.Token.Get_Data_As_String & "> is undefined (an error)");
                     Logger.Note_Error(Symbol.Token.Get_Location, Symbol.Token.Get_Data, "Symbol of production rule """ & Rule.Name_Token.Get_Data_As_String & """ not found.");
                     Self.Errors := Self.Errors + 1;
                  else
                     Logger.Note_Info(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "    Symbol <" & Symbol.Token.Get_Data_As_String & "> was previously defined in place.");
                     --Collect_Grammar_Symbol(Self, Symbol);
                  end if;
               end if;
               Next(Current);
            end loop;
         end loop;
      end loop;
   end Resolve_Rules;

   ----------------------------------------------------------------------------
   function Rule_Of(Symbol : Constant_Symbol_Pointer) return Rule_Pointer is
   begin
      return Non_Terminal_Class(Symbol.all).Rule;
   end Rule_Of;


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

      --Production_Number : Positive := 1;

      -------------------------------------------------------------------------
      procedure Number
         (Production : in     Production_Pointer) is
      begin
         --Production.Number := Production_Number;
         --Production_Number := Production_Number + 1;
         Self.Max_P := Self.Max_P + 1;
         Production.Number := Predicate_Index_Type(Self.Max_P);
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
         Answer := Answer & To_String_Type(",") & To_String_Type(Terminal_Index_Type'IMAGE(Current));
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
               --Put_Line("Follows Union "&To_S(Source.Get_Name) & To_S(Image(Source.Follows)) &" and " & To_S(Destination.Get_Name) & To_S(Image(Destination.Follows)) & ".");
               --Destination.Follows.Union(Source.Follows);
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
         Working := Source.First;
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
                     Add_Sans_Epsilon(Rule_Of(Symbol), Production.Symbols(Index + 1));
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
                  if Rule_Of(Symbol) = Rule then
                     exit Production_Loop; -- Recursive production
                  end if;
                  Add_Dependency(Dependencies, Rule, Rule_Of(Symbol));
                  --Put_Line("Rule "&To_S(Rule.Get_Name)&" passes its Follow terminals to rule " & To_S(Symbol.Name) & ".");
                  if not Symbol.Can_Disappear then
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
   function Get_Production(Self : Grammar_Class; Number : Predicate_Index_Type) return Production_Pointer is
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
   function Production_Number_Lo(Self : Grammar_Class) return Predicate_Index_Type is
   begin
      return 1;
   end Production_Number_Lo;

   ----------------------------------------------------------------------------
   function Production_Number_Hi(Self : Grammar_Class) return Predicate_Index_Type is
   begin
      return Predicate_Index_Type(Self.Max_P);
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
   function Closure
      (Self   : Grammar_Class;
       Kernel : Item_Sets.Set;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return Item_Sets.Set is
      --Answer : Item_Sets.Set;
   begin
      return Kernel;
   end Closure;

   ----------------------------------------------------------------------------
   function Goto_Step_Over -- goto(I, X)
      (Self   : Grammar_Class;
       Kernel : Item_Sets.Set;
       Index  : State_Index_Type;
       Symbol : Constant_Symbol_Pointer;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return Item_Sets.Set is

      Answer : Item_Sets.Set;

      Location : kv.apg.locations.File_Location_Type;

   begin
      Logger.Note_Debug(Location, To_String_Type("Grammar_Class.Goto_Step_Over"), "Goto("&Img(Index)&", " & To_String(Symbol.Name) & ") -- over.");
      for Item of Kernel loop
         Logger.Note_Debug(Location, To_String_Type("Grammar_Class.Goto_Step_Over"), "Processing kernel item: " & To_String(Item.Image));
         if Item.Has_Next and then Symbol.Is_Same_As(Item.Get_Big_B.all) then
            Logger.Note_Debug(Location, To_String_Type("Grammar_Class.Goto_Step_Into"), "Add next kernel for: " & To_String(Item.Image));
            Answer.Insert(Item.New_Next_Kernel_Class);
         end if;
      end loop;
      return Self.Closure(Answer, Logger);
   end Goto_Step_Over;

   type Rule_Flags_Type is array (Non_Terminal_Index_Type range <>) of Boolean;

   ----------------------------------------------------------------------------
   package Rule_Queues is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Rule_Pointer);

   ----------------------------------------------------------------------------
   function Goto_Step_Into -- goto(I, X)
      (Self   : Grammar_Class;
       Kernel : Item_Sets.Set;
       Index  : State_Index_Type;
       Symbol : Constant_Symbol_Pointer;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return Item_Sets.Set is

      Answer : Item_Sets.Set;
      Next : Constant_Symbol_Pointer;
      Rule : Rule_Pointer;
      First : Rule_Pointer;
      Follow_Up : Rule_Queues.Vector;

      Examined : Rule_Flags_Type(Self.Rule_Number_Lo .. Self.Rule_Number_Hi) := (others => False);

      Location : kv.apg.locations.File_Location_Type;

   begin
      Logger.Note_Debug(Location, To_String_Type("Grammar_Class.Goto_Step_Into"), "Goto("&Img(Index)&" " & To_String(Symbol.Name) & ") -- into.");
      for Item of Kernel loop
         Logger.Note_Debug(Location, To_String_Type("Grammar_Class.Goto_Step_Into"), "Processing kernel item: " & To_String(Item.Image));
         if Item.Has_Next then
            Next := Item.Get_Big_B;
            if not Next.Is_Terminal then
               Logger.Note_Debug(Location, Next.Name, "Next is non-terminal");
               Rule := Self.Find_Non_Terminal(Next.Name);
               while Rule /= null loop
                  Examined(Rule.Get_Number) := True;
                  Logger.Note_Debug(Location, To_String_Type("Grammar_Class.Goto_Step_Into"), "Processing rule: " & To_String(Rule.Get_Name));
                  for Production of Rule.Productions loop
                     Logger.Note_Debug(Location, To_String_Type("Grammar_Class.Goto_Step_Into"), "Checking production: " & To_String(Production.Image));
                     if Symbol.Is_Same_As(Production.Get_Symbol(1).all) then
                        Logger.Note_Debug(Location, To_String_Type("Grammar_Class.Goto_Step_Into"), "Add new kernel for: " & To_String(Production.Image));
                        Answer.Insert(New_Kernel_Class(Constant_Production_Pointer(Production), 1));
                     end if;
                     -- * right most =>
                     if not Production.Get_Symbol(1).Is_Terminal then
                        First := Self.Find_Non_Terminal(Production.Get_Symbol(1).Name);
                        if not Examined(First.Get_Number) then
                           Logger.Note_Debug(Location, To_String_Type("Grammar_Class.Goto_Step_Into"), "Follow this up: " & To_String(First.Get_Name));
                           Follow_Up.Append(First);
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
      end loop;
      return Self.Closure(Answer, Logger);
   end Goto_Step_Into;


   ----------------------------------------------------------------------------
   function Img(I : State_Index_Type) return String is
      Answer : String := State_Index_Type'IMAGE(I);
   begin
      Answer(1) := 'I';
      return Answer;
   end Img;

   ----------------------------------------------------------------------------
   function Generate_Parser_States(Self : Grammar_Class; Logger : kv.apg.logger.Safe_Logger_Pointer) return State_Space.Vector is

      Answer : State_Space.Vector;
      Count : State_Index_Type := 0;
      Current : State_Index_Type := 0;
      Found_More : Boolean := False;
      Location : kv.apg.locations.File_Location_Type;
      Working : Item_Sets.Set;

      -------------------------------------------------------------------------
      procedure Add_State(Kernels : in Item_Sets.Set) is
         use Item_Sets;
         State_Def : State_Definition_Type;
      begin
         for S in State_Index_Type(0) .. Count-1 loop
            if Answer(S).Kernels = Kernels then
               Logger.Note_Debug(Location, To_String_Type("Grammar_Class.Generate_Parser_States"), "Not adding state definition because it matches: " & Img(S));
               return;
            end if;
         end loop;
         Logger.Note_Debug(Location, To_String_Type("Grammar_Class.Generate_Parser_States"), "Adding state definition for: " & Img(Count));
         State_Def.Index := Count;
         Count := Count + 1;
         State_Def.Kernels := Kernels;
         Answer.Append(State_Def);
      end Add_State;

   begin
      Add_State(Self.First_Kernel_Set);
      loop
         Logger.Note_Debug(Location, To_String_Type("Grammar_Class.Generate_Parser_States"), "Processing " & Img(Current));
         for Symbol of Self.All_Symbols loop
            Working := Self.Goto_Step_Over(Answer(Current).Kernels, Current, Symbol, Logger);
            if not Working.Is_Empty then
               Add_State(Working);
               Found_More := True;
            end if;
            Working := Self.Goto_Step_Into(Answer(Current).Kernels, Current, Symbol, Logger);
            if not Working.Is_Empty then
               Add_State(Working);
               Found_More := True;
            end if;
         end loop;
         Current := Current + 1;
      exit when Current > Count; -- Should not be able to get here, but protect against pathological case
      exit when (not Found_More) and (Current = Count);
         Found_More := False;
      end loop;
      return Answer;
   end Generate_Parser_States;










   ----------------------------------------------------------------------------
   function Image(Action_Entry : Action_Entry_Type) return String is
      Answer : String := State_Index_Type'IMAGE(Action_Entry.Where);
      Actions : constant array (Action_Type) of Character := ('S', 'R', 'A', 'E');
   begin
      Answer(1) := Actions(Action_Entry.What);
      return Answer;
   end Image;



   Default_Action_Entry : constant Action_Entry_Type := (What => Error);


   ----------------------------------------------------------------------------
   procedure Initialize
      (Self        : in out Action_Table_Class;
       States      : in     State_Index_Type;
       Terminal_Lo : in     Terminal_Index_Type;
       Terminal_Hi : in     Terminal_Index_Type) is
   begin
      Self.Table := new Action_Table_Matrix_Type(0..States, Terminal_Lo..Terminal_Hi);
      Self.Table.all := (others => (others => Default_Action_Entry));
   end;

   ----------------------------------------------------------------------------
   procedure Set_Action
      (Self     : in out Action_Table_Class;
       Action   : in     Action_Entry_Type;
       State    : in     State_Index_Type;
       Terminal : in     Terminal_Index_Type) is
   begin
      Self.Table(State, Terminal) := Action;
   end Set_Action;

   ----------------------------------------------------------------------------
   function Get_Action
      (Self     : Action_Table_Class;
       State    : State_Index_Type;
       Terminal : Terminal_Index_Type) return Action_Entry_Type is
   begin
      return Self.Table(State, Terminal);
   end Get_Action;





   ----------------------------------------------------------------------------
   procedure Initialize
      (Self      : in out Goto_Table_Class;
       States    : in     State_Index_Type;
       Symbol_Lo : in     Non_Terminal_Index_Type;
       Symbol_Hi : in     Non_Terminal_Index_Type) is
   begin
      Self.Table := new Goto_Table_Matrix_Type(0..States, Symbol_Lo..Symbol_Hi);
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Set_Goto
      (Self        : in out Goto_Table_Class;
       Destination : in     State_Index_Type;
       State       : in     State_Index_Type;
       Symbol      : in     Non_Terminal_Index_Type) is
   begin
      Self.Table(State, Symbol) := Destination;
   end Set_Goto;

   ----------------------------------------------------------------------------
   function Get_Goto
      (Self   : Goto_Table_Class;
       State  : State_Index_Type;
       Symbol : Non_Terminal_Index_Type) return State_Index_Type is
   begin
      return Self.Table(State, Symbol);
   end Get_Goto;





   ----------------------------------------------------------------------------
   procedure Push_State
      (Self  : in out Stack_Class;
       State : in     State_Entry_Type) is
   begin
      Self.Stack.Append(State);
   end Push_State;

   ----------------------------------------------------------------------------
   function Pop_State(Self : in out Stack_Class) return State_Entry_Type is
      Answer : State_Entry_Type;
   begin
      Answer := Self.Stack.Last_Element;
      Self.Stack.Delete_Last;
      return Answer;
   end Pop_State;

   ----------------------------------------------------------------------------
   function Top_State(Self : Stack_Class) return State_Index_Type is
      Answer : State_Entry_Type;
   begin
      Answer := Self.Stack.Last_Element;
      return Answer.State;
   end Top_State;







   ----------------------------------------------------------------------------
   procedure Initialize
      (Self    : in out Parser_Engine_Class;
       Grammar : in     Grammar_Pointer;
       Logger  : in     kv.apg.logger.Safe_Logger_Pointer) is

      Start_State : State_Entry_Type := (Symbol => null, State => 0);
      Location : kv.apg.locations.File_Location_Type;

   begin
      Logger.Note_Debug(Location, To_String_Type("Parser_Engine_Class.Initialize"), "Start");
      Self.Grammar := Grammar;
      Logger.Note_Debug(Location, To_String_Type("Parser_Engine_Class.Initialize"), "Push state 0");
      Self.Stack.Push_State(Start_State);
      Logger.Note_Debug(Location, To_String_Type("Parser_Engine_Class.Initialize"), "Set up action table");
      Self.Actions.Initialize(50, Grammar.Terminal_Lo, Grammar.Terminal_Hi);
      Logger.Note_Debug(Location, To_String_Type("Parser_Engine_Class.Initialize"), "Set up goto table");
      Self.Gotos.Initialize(50, Grammar.Rule_Number_Lo, Grammar.Rule_Number_Hi);
      Logger.Note_Debug(Location, To_String_Type("Parser_Engine_Class.Initialize"), "Done");
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Parse_Token
      (Self   : in out Parser_Engine_Class;
       Token  : in     Terminal_Index_Type;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is
   begin
      null;
   end Parse_Token;

end kv.apg.rules;
