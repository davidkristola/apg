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

   type Rule_Class;
   type Rule_Pointer is access all Rule_Class;


   Unresolved_Error : exception;
   Rule_Not_Found_Error : exception;
   Production_Not_Found_Error : exception;


   type Element_Class is abstract tagged private;
   type Element_Pointer is access all Element_Class'CLASS;
   type Constant_Element_Pointer is access constant Element_Class'CLASS;

   function Can_Disappear(Self : Element_Class) return Boolean is abstract;
   function Is_Terminal(Self : Element_Class) return Boolean is abstract;
   function Is_Same_As(Self : Element_Class; Other : Element_Class'CLASS) return Boolean is abstract;

   function Name(Self : Element_Class) return String_Type;


   type Terminal_Class is new Element_Class with private;

   function Can_Disappear(Self : Terminal_Class) return Boolean is (False);
   function Is_Terminal(Self : Terminal_Class) return Boolean is (True);
   function Is_Same_As(Self : Terminal_Class; Other : Element_Class'CLASS) return Boolean;


   type Non_Terminal_Class is new Element_Class with private;

   function Can_Disappear(Self : Non_Terminal_Class) return Boolean;
   function Is_Terminal(Self : Non_Terminal_Class) return Boolean is (False);
   function Is_Same_As(Self : Non_Terminal_Class; Other : Element_Class'CLASS) return Boolean;



   type Pre_Element_Class is new Element_Class with private;

   function New_Pre_Element
      (Token : in     kv.apg.tokens.Token_Class) return Element_Pointer;

   procedure Free
      (Element : in out Element_Pointer);

   -- These will all raise Unresolved_Error
   function Can_Disappear(Self : Pre_Element_Class) return Boolean;
   function Is_Terminal(Self : Pre_Element_Class) return Boolean;
   function Is_Same_As(Self : Pre_Element_Class; Other : Element_Class'CLASS) return Boolean;







   package Element_Vector is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Element_Pointer);


   type Production_Class is tagged private;
   type Production_Pointer is access Production_Class;

   function New_Production_Class return Production_Pointer;

   procedure Append
      (Self    : in out Production_Class;
       Element : in     Element_Pointer);

   function Element_Count(Self : Production_Class) return Natural;

   function Get_Element(Self : Production_Class; Element : Positive) return Constant_Element_Pointer;

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

   function Matches_An_Empty_Sequence(Self : Production_Class) return Boolean; -- ɛ




   package Production_Vector is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Production_Pointer);




   type Rule_Class is tagged private;

   procedure Initialize
      (Self        : in out Rule_Class;
       Name        : in     kv.apg.tokens.Token_Class;
       Productions : in     Production_Vector.Vector);

   procedure Set_Is_Start
      (Self     : in out Rule_Class;
       Is_Start : in     Boolean);

   function Is_Start(Self : Rule_Class) return Boolean;

   function Get_Name(Self : Rule_Class) return String_Type;

   function Production_Count(Self : Rule_Class) return Natural;

   function Get_Production(Self : Rule_Class; Production : Positive) return Production_Pointer;

   function Can_Disappear(Self : Rule_Class) return Boolean;




   package Rule_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Rule_Pointer,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");


   type Grammar_Class is tagged private;

   procedure Initialize
      (Self   : in out Grammar_Class;
       Tokens : in     kv.apg.enum.Enumeration_Class);

   function Get_Error_Count(Self : Grammar_Class) return Natural;

   procedure Add_Rule
      (Self : in out Grammar_Class;
       Rule : in     Rule_Pointer);

   procedure Resolve_Rules
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer);

   procedure Validate
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer);

   function Find_Non_Terminal(Self : Grammar_Class; Name : String_Type) return Rule_Pointer;
   function Find_Terminal(Self : Grammar_Class; Name : String_Type) return Integer;

   function Production_Count(Self : Grammar_Class; Name : String_Type) return Natural;
   function Element_Count(Self : Grammar_Class; Name : String_Type; Production : Positive) return Natural;
   function Get_Element(Self : Grammar_Class; Name : String_Type; Production : Positive; Element : Positive) return Constant_Element_Pointer;


private

   type Element_Class is abstract tagged
      record
         Token : kv.apg.tokens.Token_Class;
      end record;

   type Terminal_Class is new Element_Class with
      record
         Key : kv.apg.fast.Key_Type;
      end record;

   type Non_Terminal_Class is new Element_Class with
      record
         Rule : Rule_Pointer;
      end record;

   type Pre_Element_Class is new Element_Class with null record;

   type Production_Class is tagged
      record
         Elements : Element_Vector.Vector;
         Code     : String_Type;
         Rule     : Rule_Pointer;
      end record;

   type Rule_Class is tagged
      record
         Name_Token  : kv.apg.tokens.Token_Class;
         Productions : Production_Vector.Vector;
         Start_Rule  : Boolean := False;
      end record;

   type Grammar_Class is tagged
      record
         Tokens : kv.apg.enum.Enumeration_Class;
         Rules  : Rule_Maps.Map;
         Start  : String_Type;
         Errors : Natural := 0;
      end record;

end kv.apg.rules;
