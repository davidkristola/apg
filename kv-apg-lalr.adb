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
      Self.Number := Production_Index_Type'LAST;
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
   function Get_Number(Self : Production_Class) return Production_Index_Type is
   begin
      if Self.Number = Production_Index_Type'LAST then
         raise Unresolved_Error;
      end if;
      return Self.Number;
   end Get_Number;

   ----------------------------------------------------------------------------
   function Get_Precedence(Self : Production_Class) return kv.apg.enum.Token_Precedence_Type is
   begin
      return Self.Precedence;
   end Get_Precedence;






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
   function Get_Production_Number(Self : Kernel_Class) return Production_Index_Type is
   begin
      return Self.Production.Get_Number;
   end Get_Production_Number;

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
   function Get_Token(Self : Rule_Class) return kv.apg.tokens.Token_Class is
   begin
      return Self.Name_Token;
   end Get_Token;

   ----------------------------------------------------------------------------
   function Get_Number(Self : Rule_Class) return Non_Terminal_Index_Type is
   begin
      return Self.Number;
   end Get_Number;

   ----------------------------------------------------------------------------
   function Get_Symbol(Self : Rule_Class) return Constant_Symbol_Pointer is
   begin
      return New_Non_Terminal_Symbol(Self.Name_Token, Self.Number);
   end Get_Symbol;






   ----------------------------------------------------------------------------
   function Img(Arg : Terminal_Index_Type) return String is --renames Terminal_Index_Type'IMAGE;
   begin
      return Terminal_Index_Type'IMAGE(Arg);
   end Img;

end kv.apg.lalr;
