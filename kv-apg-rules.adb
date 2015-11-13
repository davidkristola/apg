package body kv.apg.rules is

   ----------------------------------------------------------------------------
   function New_Pre_Element
      (Token : in     kv.apg.tokens.Token_Class) return Element_Pointer is
   begin
      return new Pre_Element_Class'(Token => Token);
   end New_Pre_Element;

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
   procedure Initialize
      (Self        : in out Rule_Class;
       Name        : in     kv.apg.tokens.Token_Class;
       Productions : in     Production_Vector.Vector) is
   begin
      Self.Name_Token := Name;
      Self.Productions := Productions;
   end Initialize;

end kv.apg.rules;
