with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Deallocation;

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
   null;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Add_Rule
      (Self : in out Grammar_Class;
       Rule : in     Rule_Pointer) is
   begin
   null;
   end Add_Rule;

   ----------------------------------------------------------------------------
   procedure Validate
      (Self   : in out Grammar_Class;
       Logger : in out kv.apg.logger.Safe_Logger_Pointer) is
   begin
   null;
   end Validate;

   ----------------------------------------------------------------------------
   function Find
      (Self : Grammar_Class; Name : String_Type) return Rule_Pointer is
   begin
      return null;
   end Find;

   ----------------------------------------------------------------------------

end kv.apg.rules;
