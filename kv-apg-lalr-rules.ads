
package kv.apg.lalr.rules is

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

   function Get_Symbols(Self : Production_Class) return Symbol_Vectors.Vector;
   function Get_First_Symbol_Cursor(Self : Production_Class) return Symbol_Vectors.Cursor;
   procedure Replace_Symbol
      (Self   : in out Production_Class;
       Cursor : in out Symbol_Vectors.Cursor;
       Update : in     Constant_Symbol_Pointer);

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
   procedure Set_Can_Disappear
      (Self          : in out Production_Class;
       Can_Disappear : in     Boolean);

   function Has_A_Terminal(Self : Production_Class) return Boolean;

   function Get_Number(Self : Production_Class) return Production_Index_Type;
   procedure Set_Number
      (Self  : in out Production_Class;
       Index : in     Production_Index_Type);

   function Get_Precedence(Self : Production_Class) return kv.apg.enum.Token_Precedence_Type;
   procedure Set_Precedence
      (Self       : in out Production_Class;
       Precedence : in     kv.apg.enum.Token_Precedence_Type);




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

   function Get_Production(Self : Kernel_Class) return Constant_Production_Pointer;
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

   procedure Add_First
      (Self : in out Rule_Class;
       This : in     Terminal_Index_Type);

   procedure Add_First
      (Self : in out Rule_Class;
       This : in     Terminal_Sets.Set);

   function Follow(Self : Rule_Class) return Terminal_Sets.Set;

   procedure Add_Follow
      (Self : in out Rule_Class;
       This : in     Terminal_Index_Type);

   procedure Add_Follow
      (Self : in out Rule_Class;
       This : in     Terminal_Sets.Set);

   function Hash(Self : Rule_Pointer) return Ada.Containers.Hash_Type;
   procedure Set_Hash
      (Self   : in out Rule_Class;
       Number : in     Ada.Containers.Hash_Type);

   function Get_Token(Self : Rule_Class) return kv.apg.tokens.Token_Class;

   function Get_Number(Self : Rule_Class) return Non_Terminal_Index_Type;
   procedure Set_Number
      (Self   : in out Rule_Class;
       Number : in     Non_Terminal_Index_Type);

   function Get_Symbol(Self : Rule_Class) return Constant_Symbol_Pointer;

   function Equivalent_Elements(Left, Right : Rule_Pointer) return Boolean;

   function Get_Productions(Self : Rule_Class) return Production_Vectors.Vector;

   procedure Set_Self_Pointer
      (Self : in out Rule_Class;
       Ptr  : in     Rule_Pointer);


   package Rule_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Rule_Pointer,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");


private



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

end kv.apg.lalr.rules;

