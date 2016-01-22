with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings;
with Ada.Strings.Hash;
with Ada.Strings.Wide_Wide_Unbounded;

with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.tokens;
with kv.apg.fast;
with kv.apg.enum;
with kv.apg.logger;

package kv.apg.rules is

   use kv.apg.tokens;
   use Ada.Strings.Wide_Wide_Unbounded;


   Unresolved_Error : exception;
   Rule_Not_Found_Error : exception;
   Production_Not_Found_Error : exception;
   Dot_Position_Error : exception;
   Terminal_Expected_Error : exception;
   Non_Following_Terminal_Error : exception;


   End_Of_File : constant := -1;
   Epsilon     : constant := 0;
   Terminal_1  : constant := 1;

   type Terminal_Index_Type is range End_Of_File .. Integer'LAST;
   type Non_Terminal_Index_Type is new Positive; -- Rule index

   function Img(Arg : Terminal_Index_Type) return String;

   package Terminal_Sets is new Ada.Containers.Ordered_Sets(Terminal_Index_Type);



   type Symbol_Class is abstract tagged private;
   type Symbol_Pointer is access all Symbol_Class'CLASS;
   type Constant_Symbol_Pointer is access constant Symbol_Class'CLASS;

   function Name(Self : Symbol_Class) return String_Type;

   function Is_Terminal(Self : Symbol_Class) return Boolean is abstract;
   function Is_Same_As(Self : Symbol_Class; Other : Symbol_Class'CLASS) return Boolean is abstract;
   function Get_Number(Self : Symbol_Class) return Terminal_Index_Type is abstract;
   function Get_Index(Self : Symbol_Class) return Integer is abstract;



   type Terminal_Class is new Symbol_Class with private;

   function End_Of_File_Terminal return Terminal_Class;
   function New_End_Of_File_Terminal return Constant_Symbol_Pointer;
   function Epsilon_Terminal return Terminal_Class;

   function Is_Terminal(Self : Terminal_Class) return Boolean is (True);
   function Is_Same_As(Self : Terminal_Class; Other : Symbol_Class'CLASS) return Boolean;
   function Get_Number(Self : Terminal_Class) return Terminal_Index_Type;
   function Get_Index(Self : Terminal_Class) return Integer;


   type Non_Terminal_Class is new Symbol_Class with private;

   function Is_Terminal(Self : Non_Terminal_Class) return Boolean is (False);
   function Is_Same_As(Self : Non_Terminal_Class; Other : Symbol_Class'CLASS) return Boolean;
   function Get_Number(Self : Non_Terminal_Class) return Terminal_Index_Type; -- will raise Terminal_Expected_Error
   function Get_Index(Self : Non_Terminal_Class) return Integer;

   function New_Non_Terminal_Symbol(Token : kv.apg.tokens.Token_Class; Rule : Non_Terminal_Index_Type) return Constant_Symbol_Pointer;


   type Pre_Symbol_Class is new Symbol_Class with private;

   function New_Pre_Symbol
      (Token : in     kv.apg.tokens.Token_Class) return Constant_Symbol_Pointer;

   procedure Free
      (Symbol : in out Constant_Symbol_Pointer);

   -- These will all raise Unresolved_Error
   function Is_Terminal(Self : Pre_Symbol_Class) return Boolean;
   function Is_Same_As(Self : Pre_Symbol_Class; Other : Symbol_Class'CLASS) return Boolean;
   function Get_Number(Self : Pre_Symbol_Class) return Terminal_Index_Type;
   function Get_Index(Self : Pre_Symbol_Class) return Integer;






   function Equal(L, R : Constant_Symbol_Pointer) return Boolean;

   package Symbol_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Constant_Symbol_Pointer,
       "=" => Equal);







   type Rule_Class;
   type Rule_Pointer is access all Rule_Class;




   type Production_Index_Type is new Positive;


   type Production_Class is tagged private;
   type Production_Pointer is access Production_Class;
   type Constant_Production_Pointer is access constant Production_Class;

   function New_Production_Class return Production_Pointer;

   procedure Append
      (Self   : in out Production_Class;
       Symbol : in     Constant_Symbol_Pointer);

   function Symbol_Count(Self : Production_Class) return Natural;

   function Get_Symbol(Self : Production_Class; Symbol : Positive) return Constant_Symbol_Pointer;

   function Image(Self : Production_Class) return String_Type;

   procedure Clear
      (Self : in out Production_Class);

   procedure Set_Code
      (Self : in out Production_Class;
       Code : in     kv.apg.tokens.Token_Class);

   function Get_Code(Self : Production_Class) return String_Type;

   procedure Set_Rule
      (Self : in out Production_Class;
       Rule : in     Rule_Pointer);

   function Get_Rule(Self : Production_Class) return Rule_Pointer;

   function Matches_An_Empty_Sequence(Self : Production_Class) return Boolean; -- Is ɛ

   function Can_Disappear(Self : Production_Class) return Boolean; -- Could be ɛ if the correct terminal follows

   function Has_A_Terminal(Self : Production_Class) return Boolean;

   function Get_Number(Self : Production_Class) return Production_Index_Type;

   function Get_Precedence(Self : Production_Class) return kv.apg.enum.Token_Precedence_Type;




   package Production_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Production_Pointer);




   type Kernel_Class is tagged private;
   type Item_Class is new Kernel_Class with private;
   type Item_Pointer is access Kernel_Class'CLASS;
   type Constant_Item_Pointer is access constant Kernel_Class'CLASS;

   function New_Kernel_Class
      (Production   : Constant_Production_Pointer;
       Dot_Position : Natural) return Constant_Item_Pointer;

   function New_Next_Kernel_Class(Self : Kernel_Class) return Constant_Item_Pointer;

   function New_Item_Class
      (Production   : Constant_Production_Pointer;
       Dot_Position : Natural;
       Terminal     : Constant_Symbol_Pointer) return Constant_Item_Pointer;

   procedure Free
      (Item : in out Constant_Item_Pointer);

   function Image(Self : Kernel_Class) return String_Type;
   function Image(Self : Item_Class) return String_Type;

   function Has_Next(Self : Kernel_Class) return Boolean;

   function Get_Production_Number(Self : Kernel_Class) return Production_Index_Type;

   function Get_Big_A(Self : Kernel_Class) return Rule_Pointer;
   function Get_Little_Alpha(Self : Kernel_Class) return Constant_Symbol_Pointer;
   function Get_Big_B(Self : Kernel_Class) return Constant_Symbol_Pointer;
   function Get_Little_Beta(Self : Kernel_Class) return Constant_Symbol_Pointer;

   function Get_Little_A(Self : Kernel_Class) return Constant_Symbol_Pointer;
   function Get_Little_A(Self : Item_Class) return Constant_Symbol_Pointer;

   function "<"(L, R : Kernel_Class) return Boolean;
   function "="(L, R : Kernel_Class) return Boolean;
   function "<"(L, R : Item_Class) return Boolean;
   function "="(L, R : Item_Class) return Boolean;


   function "<"(L, R : Constant_Item_Pointer) return Boolean;
   function "="(L, R : Constant_Item_Pointer) return Boolean;

   package Item_Sets is new Ada.Containers.Ordered_Sets(Constant_Item_Pointer);




   type Rule_Class is tagged private;

   procedure Initialize
      (Self        : in out Rule_Class;
       Name        : in     kv.apg.tokens.Token_Class;
       Productions : in     Production_Vectors.Vector);

   procedure Set_Is_Start
      (Self     : in out Rule_Class;
       Is_Start : in     Boolean);

   function Is_Start(Self : Rule_Class) return Boolean;

   function Get_Name(Self : Rule_Class) return String_Type;

   function Production_Count(Self : Rule_Class) return Natural;

   function Get_Production(Self : Rule_Class; Production : Positive) return Production_Pointer;

   function Can_Disappear(Self : Rule_Class) return Boolean;

   function Has_An_Empty_Sequence(Self : Rule_Class) return Boolean;

   function First(Self : Rule_Class) return Terminal_Sets.Set;

   function Follow(Self : Rule_Class) return Terminal_Sets.Set;

   function Hash(Self : Rule_Pointer) return Ada.Containers.Hash_Type;

   function Get_Token(Self : Rule_Class) return kv.apg.tokens.Token_Class;

   function Get_Number(Self : Rule_Class) return Non_Terminal_Index_Type;

   function Get_Symbol(Self : Rule_Class) return Constant_Symbol_Pointer;

   function Equivalent_Elements(Left, Right : Rule_Pointer) return Boolean;




   package Rule_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Rule_Pointer,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");




private

   type Symbol_Class is abstract tagged
      record
         Token : kv.apg.tokens.Token_Class;
      end record;

   type Terminal_Class is new Symbol_Class with
      record
         Key : kv.apg.fast.Key_Type;
      end record;

   type Non_Terminal_Class is new Symbol_Class with
      record
         Rule_Number : Non_Terminal_Index_Type;
      end record;

   type Pre_Symbol_Class is new Symbol_Class with null record;

   type Production_Class is tagged
      record
         Symbols    : Symbol_Vectors.Vector;
         Code       : String_Type;
         Rule       : Rule_Pointer;
         Vanishable : Boolean;
         Number     : Production_Index_Type := Production_Index_Type'LAST; -- Will be nominal once it is resolved
         Precedence : kv.apg.enum.Token_Precedence_Type := 0;
      end record;

   type Kernel_Class is tagged
      record
         Production   : Constant_Production_Pointer;
         Dot_Position : Natural;
      end record;
   type Item_Class is new Kernel_Class with
      record
         Terminal     : Constant_Symbol_Pointer;
      end record;


   type Rule_Class is tagged
      record
         Me          : Rule_Pointer;
         Name_Token  : kv.apg.tokens.Token_Class;
         Productions : Production_Vectors.Vector;
         Firsts      : Terminal_Sets.Set;
         Follows     : Terminal_Sets.Set;
         My_Hash     : Ada.Containers.Hash_Type;
         Start_Rule  : Boolean := False;
         Number      : Non_Terminal_Index_Type := Non_Terminal_Index_Type'LAST; -- Will be nominal once it is resolved
      end record;


end kv.apg.rules;
