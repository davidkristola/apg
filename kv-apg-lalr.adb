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

package body kv.apg.lalr is

   use Ada.Strings.UTF_Encoding;
   use Ada.Strings.UTF_Encoding.Strings;

   use kv.apg.incidents; -- Severity_Type

   procedure Free_Symbol_Instance is new Ada.Unchecked_Deallocation(Symbol_Class'CLASS, Symbol_Pointer);
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
   function Get_Number(Self : Terminal_Class) return Terminal_Index_Type is
   begin
      return Terminal_Index_Type(Self.Key);
   end Get_Number;

   ----------------------------------------------------------------------------
   function Get_Index(Self : Terminal_Class) return Integer is
   begin
      return Integer(Self.Key);
   end Get_Index;

   ----------------------------------------------------------------------------
   function Name(Self : Symbol_Class) return String_Type is
   begin
      return Self.Token.Get_Data;
   end Name;

   ----------------------------------------------------------------------------
   function Is_Same_As(Self : Non_Terminal_Class; Other : Symbol_Class'CLASS) return Boolean is
   begin
      return (not Other.Is_Terminal) and then Non_Terminal_Class(Other).Rule_Number = Self.Rule_Number;
   end Is_Same_As;

   ----------------------------------------------------------------------------
   function Get_Number(Self : Non_Terminal_Class) return Terminal_Index_Type is
   begin
      raise Terminal_Expected_Error;
      return Terminal_Index_Type(Epsilon);
   end Get_Number;

   ----------------------------------------------------------------------------
   function Get_Index(Self : Non_Terminal_Class) return Integer is
   begin
      return Integer(Self.Rule_Number);
   end Get_Index;

   ----------------------------------------------------------------------------
   function New_Non_Terminal_Symbol(Token : kv.apg.tokens.Token_Class; Rule : Non_Terminal_Index_Type) return Constant_Symbol_Pointer is
      N : access Non_Terminal_Class := new Non_Terminal_Class;
   begin
      N.Token := Token;
      N.Rule_Number := Rule;
      return Constant_Symbol_Pointer(N);
   end New_Non_Terminal_Symbol;




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
   function Get_Number(Self : Pre_Symbol_Class) return Terminal_Index_Type is
   begin
      raise Unresolved_Error;
      return Terminal_Index_Type(Epsilon);
   end Get_Number;

   ----------------------------------------------------------------------------
   function Get_Index(Self : Pre_Symbol_Class) return Integer is
   begin
      raise Unresolved_Error;
      return Integer(Epsilon);
   end Get_Index;


   ----------------------------------------------------------------------------
   function Equal(L, R : Constant_Symbol_Pointer) return Boolean is
      use Ada.Tags;
   begin
      if (L = null) or (R = null) then
         return False;
      end if;
      if L.all'TAG = R.all'TAG then
         return L.all.Is_Same_As(R.all);
      end if;
      return False; -- not the same tag, can't be equal
   end Equal;



   ----------------------------------------------------------------------------
   function Img(Arg : Terminal_Index_Type) return String is --renames Terminal_Index_Type'IMAGE;
   begin
      return Terminal_Index_Type'IMAGE(Arg);
   end Img;

end kv.apg.lalr;
