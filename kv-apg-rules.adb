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
   procedure Append
      (Self    : in out Production_Class;
       Element : in     Element_Pointer) is
   begin
      Self.Elements.Append(Element);
   end Append;

   ----------------------------------------------------------------------------
   function Image(Self : in out Production_Class) return String_Type is
      Answer : String_Type := To_String_Type("(");
   begin
      for Element of Self.Elements loop
         Answer := Answer & To_String_Type(" ") & Element.Token.Get_Data;
      end loop;
      return Answer & To_String_Type(" ) => ") & Self.Code;
   end Image;

   ----------------------------------------------------------------------------
   procedure Clear
      (Self : in out Production_Class) is
   begin
      Self.Elements := Element_Vector.Empty_Vector;
      Self.Code := Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String;
   end Clear;

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
      -- look for shift/shift errors
      -- look for shift/reduce errors
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

end kv.apg.rules;
