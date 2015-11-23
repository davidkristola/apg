with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with kv.apg.locations;

package body kv.apg.rules is

   procedure Free_Instance is new Ada.Unchecked_Deallocation(Element_Class'CLASS, Element_Pointer);

   ----------------------------------------------------------------------------
   function New_Pre_Element
      (Token : in     kv.apg.tokens.Token_Class) return Element_Pointer is
   begin
      return new Pre_Element_Class'(Token => Token);
   end New_Pre_Element;

   ----------------------------------------------------------------------------
   procedure Free
      (Element : in out Element_Pointer) is
   begin
      Free_Instance(Element);
   end Free;


   ----------------------------------------------------------------------------
   function Is_Same_As(Self : Terminal_Class; Other : Element_Class'CLASS) return Boolean is
      use kv.apg.fast; -- "="
   begin
      return (Other.Is_Terminal) and then Terminal_Class(Other).Key = Self.Key;
   end Is_Same_As;

   ----------------------------------------------------------------------------
   function Name(Self : Element_Class) return String_Type is
   begin
      return Self.Token.Get_Data;
   end Name;

   ----------------------------------------------------------------------------
   function Can_Disappear(Self : Non_Terminal_Class) return Boolean is
   begin
      return Self.Rule.Can_Disappear;
   end Can_Disappear;

   ----------------------------------------------------------------------------
   function Is_Same_As(Self : Non_Terminal_Class; Other : Element_Class'CLASS) return Boolean is
   begin
      return (not Other.Is_Terminal) and then Non_Terminal_Class(Other).Rule = Self.Rule;
   end Is_Same_As;



   ----------------------------------------------------------------------------
   function Can_Disappear(Self : Pre_Element_Class) return Boolean is
   begin
      raise Unresolved_Error;
      return False;
   end Can_Disappear;

   ----------------------------------------------------------------------------
   function Is_Terminal(Self : Pre_Element_Class) return Boolean is
   begin
      raise Unresolved_Error;
      return False;
   end Is_Terminal;

   ----------------------------------------------------------------------------
   function Is_Same_As(Self : Pre_Element_Class; Other : Element_Class'CLASS) return Boolean is
   begin
      raise Unresolved_Error;
      return False;
   end Is_Same_As;




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
      (Self    : in out Production_Class;
       Element : in     Element_Pointer) is
   begin
      Self.Elements.Append(Element);
   end Append;

   ----------------------------------------------------------------------------
   function Element_Count(Self : Production_Class) return Natural is
   begin
      return Natural(Element_Vector.Length(Self.Elements));
   end Element_Count;

   ----------------------------------------------------------------------------
   function Get_Element(Self : Production_Class; Element : Positive) return Constant_Element_Pointer is
      Ep : Element_Pointer;
   begin
      Ep := Self.Elements(Element);
      return Constant_Element_Pointer(Ep);
   end Get_Element;

   ----------------------------------------------------------------------------
   function Image(Self : Production_Class) return String_Type is
      Answer : String_Type := To_String_Type("(");
   begin
      for Element of Self.Elements loop
         Answer := Answer & To_String_Type(" ") & Element.Token.Get_Data;
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
      Self.Elements := Element_Vector.Empty_Vector;
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
      return Element_Vector.Is_Empty(Self.Elements);
   end Matches_An_Empty_Sequence;






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
      To_Be_Checked : Element_Vector.Vector;
      Already_Checked : Element_Vector.Vector;
      Element : Element_Pointer;
      Check : Element_Pointer;
      use Element_Vector;
   begin
      for Production of Self.Productions loop
         if Production.Matches_An_Empty_Sequence then
            return True;
         end if;
         Element := Production.Elements(1);
         if not Element.Is_Terminal then
            To_Be_Checked.Append(Element);
            Put_Line("Pushing " & To_String(Element.Name) & " onto To_Be_Checked");
         end if;
      end loop;
      --TODO: need to check the first element of each production
      -- But can't simply use recursion because that could lead
      -- to an infinite loop.
      while not Is_Empty(To_Be_Checked) loop
         Element := First_Element(To_Be_Checked);
         Delete_First(To_Be_Checked);
         Put_Line("Popping " & To_String(Element.Name) & " from To_Be_Checked");
         --
         Already_Checked.Append(Element);
         --
         Inner_Loop: for Production of Non_Terminal_Class(Element.all).Rule.Productions loop
            if Production.Matches_An_Empty_Sequence then
               return True;
            end if;
            Check := Production.Elements(1);
            if Already_Checked.Contains(Check) then
               exit Inner_Loop;
            end if;
            if not Check.Is_Terminal then
               To_Be_Checked.Append(Check);
               Put_Line("Pushing secondary " & To_String(Check.Name) & " onto To_Be_Checked");
            end if;
            --
         end loop Inner_Loop;
      end loop;
      return False;
   end Can_Disappear;



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
      use Ada.Strings.UTF_Encoding;
      use Ada.Strings.UTF_Encoding.Strings;
   begin
      Self.Rules.Include(Decode(To_String(Rule.Get_Name), UTF_8), Rule);
   end Add_Rule;

   ----------------------------------------------------------------------------
   procedure Resolve_Rules
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer) is
      Element : Element_Pointer;
      Current : Element_Vector.Cursor;
      To_Be_Deleted : Element_Pointer;
      Updated_Element : Element_Pointer;
      Index : Natural;
      use Element_Vector;
      use Ada.Strings.UTF_Encoding;
      use Ada.Strings.UTF_Encoding.Strings;
   begin
      for Rule of Self.Rules loop
         Logger.Note_Info(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "Rule <" & Rule.Name_Token.Get_Data_As_String & ">");
         for Production of Rule.Productions loop
            Logger.Note_Info(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "  Production <" & Decode(To_String(Production.Image), UTF_8) & ">");
            Production.Set_Rule(Rule);
            Current := First(Production.Elements);
            while Current /= No_Element loop
               Element := Element_Vector.Element(Current);
               -- update the element from a Pre_Element_Class to either a Terminal_Class (token) or a Non_Terminal_Class (rule)
               -- log an error if the element does not resolve
               if Self.Find_Non_Terminal(Element.Token.Get_Data) /= null then
                  Logger.Note_Info(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "    Element <" & Element.Token.Get_Data_As_String & "> is a nonterminal (rule)");
                  To_Be_Deleted := Element;
                  Updated_Element := new Non_Terminal_Class'(Token => Element.Token, Rule => Self.Find_Non_Terminal(Element.Token.Get_Data));
                  Production.Elements.Replace_Element(Current, Updated_Element);
                  Free(To_Be_Deleted);
               elsif Self.Find_Terminal(Element.Token.Get_Data) /= kv.apg.enum.Not_Found_Error then
                  Logger.Note_Info(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "    Element <" & Element.Token.Get_Data_As_String & "> is a terminal (token)");
                  Index := Self.Find_Terminal(Element.Token.Get_Data);
                  To_Be_Deleted := Element;
                  Updated_Element := new Terminal_Class'(Token => Self.Tokens.Get(Index), Key => kv.apg.fast.Key_Type(Index));
                  Production.Elements.Replace_Element(Current, Updated_Element);
                  Free(To_Be_Deleted);
               else
                  Logger.Note_Info(Rule.Name_Token.Get_Location, Rule.Name_Token.Get_Data, "    Element <" & Element.Token.Get_Data_As_String & "> is undefined (an error)");
                  Logger.Note_Error(Element.Token.Get_Location, Element.Token.Get_Data, "Element of production rule """ & Rule.Name_Token.Get_Data_As_String & """ not found.");
                  Self.Errors := Self.Errors + 1;
               end if;
               Next(Current);
            end loop;
         end loop;
      end loop;
   end Resolve_Rules;

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
   function Element_Count(Self : Grammar_Class; Name : String_Type; Production : Positive) return Natural is
      Rule : Rule_Pointer;
   begin
      Rule := Self.Find_Non_Terminal(Name);
      if Rule = null then
         raise Rule_Not_Found_Error;
      end if;
      if Production > Rule.Production_Count then
         raise Production_Not_Found_Error;
      end if;
      return Rule.Get_Production(Production).Element_Count;
   end Element_Count;


   ----------------------------------------------------------------------------
   function Get_Element(Self : Grammar_Class; Name : String_Type; Production : Positive; Element : Positive) return Constant_Element_Pointer is
      Rule : Rule_Pointer;
   begin
      Rule := Self.Find_Non_Terminal(Name);
      if Rule = null then
         raise Rule_Not_Found_Error;
      end if;
      if Production > Rule.Production_Count then
         raise Production_Not_Found_Error;
      end if;
      return Constant_Element_Pointer(Rule.Get_Production(Production).Get_Element(Element));
   end ;

end kv.apg.rules;
