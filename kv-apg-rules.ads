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

   type Rule_Class;
   type Rule_Pointer is access all Rule_Class;
   type Non_Terminal_Index_Type is new Positive; -- Rule index


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

   package Terminal_Sets is new Ada.Containers.Ordered_Sets(Terminal_Index_Type);



   type Symbol_Class is abstract tagged private;
   type Symbol_Pointer is access all Symbol_Class'CLASS;
   type Constant_Symbol_Pointer is access constant Symbol_Class'CLASS;

   function Name(Self : Symbol_Class) return String_Type;

   function Can_Disappear(Self : Symbol_Class) return Boolean is abstract;
   function Is_Terminal(Self : Symbol_Class) return Boolean is abstract;
   function Is_Same_As(Self : Symbol_Class; Other : Symbol_Class'CLASS) return Boolean is abstract;
   function First(Self : Symbol_Class) return Terminal_Sets.Set is abstract;
   function Get_Number(Self : Symbol_Class) return Terminal_Index_Type is abstract;



   type Terminal_Class is new Symbol_Class with private;

   function End_Of_File_Terminal return Terminal_Class;
   function New_End_Of_File_Terminal return Constant_Symbol_Pointer;
   function Epsilon_Terminal return Terminal_Class;

   function Can_Disappear(Self : Terminal_Class) return Boolean is (False);
   function Is_Terminal(Self : Terminal_Class) return Boolean is (True);
   function Is_Same_As(Self : Terminal_Class; Other : Symbol_Class'CLASS) return Boolean;
   function First(Self : Terminal_Class) return Terminal_Sets.Set;
   function Get_Number(Self : Terminal_Class) return Terminal_Index_Type;


   type Non_Terminal_Class is new Symbol_Class with private;

   function Can_Disappear(Self : Non_Terminal_Class) return Boolean;
   function Is_Terminal(Self : Non_Terminal_Class) return Boolean is (False);
   function Is_Same_As(Self : Non_Terminal_Class; Other : Symbol_Class'CLASS) return Boolean;
   function First(Self : Non_Terminal_Class) return Terminal_Sets.Set;
   function Get_Number(Self : Non_Terminal_Class) return Terminal_Index_Type; -- will raise Terminal_Expected_Error



   type Pre_Symbol_Class is new Symbol_Class with private;

   function New_Pre_Symbol
      (Token : in     kv.apg.tokens.Token_Class) return Constant_Symbol_Pointer;

   procedure Free
      (Symbol : in out Constant_Symbol_Pointer);

   -- These will all raise Unresolved_Error
   function Can_Disappear(Self : Pre_Symbol_Class) return Boolean;
   function Is_Terminal(Self : Pre_Symbol_Class) return Boolean;
   function Is_Same_As(Self : Pre_Symbol_Class; Other : Symbol_Class'CLASS) return Boolean;
   function First(Self : Pre_Symbol_Class) return Terminal_Sets.Set;
   function Get_Number(Self : Pre_Symbol_Class) return Terminal_Index_Type;







   function Equal(L, R : Constant_Symbol_Pointer) return Boolean;

   package Symbol_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Constant_Symbol_Pointer,
       "=" => Equal);



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

   function First(Self : Production_Class) return Terminal_Sets.Set;

   function Get_Number(Self : Production_Class) return Production_Index_Type;




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

   function Get_Number(Self : Rule_Class) return Non_Terminal_Index_Type;

   function Get_Symbol(Self : Rule_Class) return Constant_Symbol_Pointer;





   package Rule_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Rule_Pointer,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");




   type Grammar_Class is tagged private;
   type Grammar_Pointer is access all Grammar_Class;

   procedure Initialize
      (Self   : in out Grammar_Class;
       Tokens : in     kv.apg.enum.Enumeration_Class);

   function Get_Error_Count(Self : Grammar_Class) return Natural;

   procedure Add_Rule
      (Self : in out Grammar_Class;
       Rule : in     Rule_Pointer);

   function Find_Start(Self : Grammar_Class) return Rule_Pointer;

   procedure Add_Meta_Rule
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer);

   procedure Resolve_Rules
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer);

   procedure Resolve_Productions
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer);

   procedure Resolve_Firsts
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer);

   procedure Resolve_Follows
      (Self    : in out Grammar_Class;
       Add_EOF : in     Boolean;
       Logger  : in     kv.apg.logger.Safe_Logger_Pointer);

   procedure Validate
      (Self   : in out Grammar_Class;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer);

   function Find_Non_Terminal(Self : Grammar_Class; Name : String_Type) return Rule_Pointer;
   function Find_Terminal(Self : Grammar_Class; Name : String_Type) return Integer;

   function Production_Count(Self : Grammar_Class; Name : String_Type) return Natural;
   function Symbol_Count(Self : Grammar_Class; Name : String_Type; Production : Positive) return Natural;
   function Get_Symbol(Self : Grammar_Class; Name : String_Type; Production : Positive; Symbol : Positive) return Constant_Symbol_Pointer;


   function Get_Production(Self : Grammar_Class; Number : Production_Index_Type) return Production_Pointer;
   function Get_Rule(Self : Grammar_Class; Number : Non_Terminal_Index_Type) return Rule_Pointer;

   function Rule_Number_Lo(Self : Grammar_Class) return Non_Terminal_Index_Type;
   function Rule_Number_Hi(Self : Grammar_Class) return Non_Terminal_Index_Type;
   function Production_Number_Lo(Self : Grammar_Class) return Production_Index_Type;
   function Production_Number_Hi(Self : Grammar_Class) return Production_Index_Type;
   function Terminal_Lo(Self : Grammar_Class) return Terminal_Index_Type;
   function Terminal_Hi(Self : Grammar_Class) return Terminal_Index_Type;

   function Grammar_Symbols(Self : Grammar_Class) return Symbol_Vectors.Vector;
   function Translate(Self : Grammar_Class; Terminal : Terminal_Index_Type) return Constant_Symbol_Pointer;
   function Translate(Self : Grammar_Class; Terminals : Terminal_Sets.Set) return Symbol_Vectors.Vector;

   function First_Kernel_Set(Self : Grammar_Class) return Item_Sets.Set;

   function Closure
      (Self   : Grammar_Class;
       Kernel : Item_Sets.Set;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return Item_Sets.Set;

   type State_Index_Type is new Natural;
   function Img(I : State_Index_Type) return String;


   function Goto_Step_Over_One -- goto(I, X)
      (Self   : Grammar_Class;
       Kernel : Constant_Item_Pointer;
       Index  : State_Index_Type;
       Symbol : Constant_Symbol_Pointer;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return Item_Sets.Set;

   function Goto_Step_Over -- goto(I, X)
      (Self   : Grammar_Class;
       Kernel : Item_Sets.Set;
       Index  : State_Index_Type;
       Symbol : Constant_Symbol_Pointer;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return Item_Sets.Set;

   function Goto_Step_Into_One -- goto(I, X)
      (Self   : Grammar_Class;
       Kernel : Constant_Item_Pointer;
       Index  : State_Index_Type;
       Symbol : Constant_Symbol_Pointer;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return Item_Sets.Set;

   function Goto_Step_Into -- goto(I, X)
      (Self   : Grammar_Class;
       Kernel : Item_Sets.Set;
       Index  : State_Index_Type;
       Symbol : Constant_Symbol_Pointer;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return Item_Sets.Set;




   type Action_Hint_Type is
      record
         Symbol     : Constant_Symbol_Pointer;
         From_State : State_Index_Type;
         To_State   : State_Index_Type;
      end record;
   package Action_Space is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Action_Hint_Type);



   type State_Definition_Type is
      record
         Index   : State_Index_Type;
         Kernels : Item_Sets.Set;
      end record;

   package State_Space is new Ada.Containers.Vectors
      (Index_Type   => State_Index_Type,
       Element_Type => State_Definition_Type);


   type State_Information_Type is
      record
         States : State_Space.Vector;
         Hints  : Action_Space.Vector;
      end record;


   function Generate_Parser_States(Self : Grammar_Class; Logger : kv.apg.logger.Safe_Logger_Pointer) return State_Information_Type;




   type Action_Type is (Shift, Reduce, Accept_Input, Error);

   type Action_Entry_Type(What : Action_Type := Error) is
      record
         case What is
            when Shift =>
               Where : State_Index_Type;
            when Reduce =>
               Production : Production_Index_Type;
            when others =>
               null;
         end case;
      end record;

   function Image(Action_Entry : Action_Entry_Type) return String;


   type Action_Table_Class is tagged private;
   procedure Initialize
      (Self        : in out Action_Table_Class;
       States      : in     State_Index_Type;
       Terminal_Lo : in     Terminal_Index_Type;
       Terminal_Hi : in     Terminal_Index_Type);

   procedure Set_Action
      (Self     : in out Action_Table_Class;
       Action   : in     Action_Entry_Type;
       State    : in     State_Index_Type;
       Terminal : in     Terminal_Index_Type;
       Logger   : in     kv.apg.logger.Safe_Logger_Pointer);

   function Get_Action
      (Self     : Action_Table_Class;
       State    : State_Index_Type;
       Terminal : Terminal_Index_Type) return Action_Entry_Type;

   function Error_Count(Self : Action_Table_Class) return Natural;


   type Goto_Table_Class is tagged private;
   procedure Initialize
      (Self      : in out Goto_Table_Class;
       States    : in     State_Index_Type;
       Symbol_Lo : in     Non_Terminal_Index_Type;
       Symbol_Hi : in     Non_Terminal_Index_Type);

   procedure Set_Goto
      (Self        : in out Goto_Table_Class;
       Destination : in     State_Index_Type;
       State       : in     State_Index_Type;
       Symbol      : in     Non_Terminal_Index_Type);

   function Get_Goto
      (Self   : Goto_Table_Class;
       State  : State_Index_Type;
       Symbol : Non_Terminal_Index_Type) return State_Index_Type;



   type State_Entry_Type is
      record
         Symbol : Constant_Symbol_Pointer;
         State  : State_Index_Type;
      end record;

   type Stack_Class is tagged private;
   procedure Push_State
      (Self  : in out Stack_Class;
       State : in     State_Entry_Type);
   function Pop_State(Self : in out Stack_Class) return State_Entry_Type;
   function Top_State(Self : Stack_Class) return State_Index_Type;



   type Parser_Engine_Class is tagged private;
   procedure Initialize
      (Self    : in out Parser_Engine_Class;
       Grammar : in     Grammar_Pointer;
       Logger  : in     kv.apg.logger.Safe_Logger_Pointer);

   procedure Parse_Token
      (Self   : in out Parser_Engine_Class;
       Token  : in     Terminal_Index_Type;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer);

   function Error_Count(Self : Parser_Engine_Class) return Natural;
   function Has_Accepted(Self : Parser_Engine_Class) return Boolean;

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
         Rule : Rule_Pointer;
      end record;

   type Pre_Symbol_Class is new Symbol_Class with null record;

   type Production_Class is tagged
      record
         Symbols    : Symbol_Vectors.Vector;
         Code       : String_Type;
         Rule       : Rule_Pointer;
         Vanishable : Boolean;
         Number     : Production_Index_Type := Production_Index_Type'LAST; -- Will be nominal once it is resolved
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

   type Grammar_Class is tagged
      record
         Tokens : kv.apg.enum.Enumeration_Class;
         Rules  : Rule_Maps.Map;
         All_Symbols : Symbol_Vectors.Vector;
         Start  : String_Type;
         Count  : Natural := 0; -- Count of rules
         Max_P  : Natural := 0;
         Max_T  : Natural := 0;
         Errors : Natural := 0;
      end record;


   type Action_Table_Matrix_Type is array (State_Index_Type range <>, Terminal_Index_Type range <>) of Action_Entry_Type;
   type Action_Table_Matrix_Pointer is access Action_Table_Matrix_Type;

   type Action_Table_Class is tagged
      record
         Table  : Action_Table_Matrix_Pointer;
         Errors : Natural := 0;
      end record;


   type Goto_Table_Matrix_Type is array (State_Index_Type range <>, Non_Terminal_Index_Type range <>) of State_Index_Type;
   type Goto_Table_Matrix_Pointer is access Goto_Table_Matrix_Type;
   type Goto_Table_Class is tagged
      record
         Table : Goto_Table_Matrix_Pointer;
      end record;

   package State_Vector is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => State_Entry_Type);

   type Stack_Class is tagged
      record
         Stack : State_Vector.Vector;
      end record;


   type Parser_Engine_Class is tagged
      record
         Grammar  : Grammar_Pointer;
         States   : State_Space.Vector;
         Stack    : Stack_Class;
         Actions  : Action_Table_Class;
         Gotos    : Goto_Table_Class;
         Accepted : Boolean := False;
         Errors   : Natural := 0;
      end record;


end kv.apg.rules;
