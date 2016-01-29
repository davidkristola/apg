with kv.apg.lalr.rules;

package kv.apg.lalr.grammars is


   type Grammar_Class is tagged private;
   type Grammar_Pointer is access all Grammar_Class;

   procedure Initialize
      (Self   : in out Grammar_Class;
       Tokens : in     kv.apg.enum.Enumeration_Class);

   function Get_Error_Count(Self : Grammar_Class) return Natural;

   function Get_Tokens(Self : Grammar_Class) return kv.apg.enum.Enumeration_Class;

   procedure Add_Rule
      (Self : in out Grammar_Class;
       Rule : in     kv.apg.lalr.rules.Rule_Pointer);

   function Find_Start(Self : Grammar_Class) return kv.apg.lalr.rules.Rule_Pointer;

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

   function Rule_Of(Self : Grammar_Class; Symbol : Constant_Symbol_Pointer) return kv.apg.lalr.rules.Rule_Pointer;
   function Find_Non_Terminal(Self : Grammar_Class; Name : String_Type) return kv.apg.lalr.rules.Rule_Pointer;
   function Find_Terminal(Self : Grammar_Class; Name : String_Type) return Integer;

   function First_Of(Self : Grammar_Class; Symbol : Constant_Symbol_Pointer) return Terminal_Sets.Set;


   function Production_Count(Self : Grammar_Class; Name : String_Type) return Natural;
   function Symbol_Count(Self : Grammar_Class; Name : String_Type; Production : Positive) return Natural;
   function Get_Symbol(Self : Grammar_Class; Name : String_Type; Production : Positive; Symbol : Positive) return Constant_Symbol_Pointer;


   function Get_Production(Self : Grammar_Class; Number : kv.apg.lalr.rules.Production_Index_Type) return kv.apg.lalr.rules.Production_Pointer;
   function Get_Rule(Self : Grammar_Class; Number : Non_Terminal_Index_Type) return kv.apg.lalr.rules.Rule_Pointer;

   function Rule_Number_Lo(Self : Grammar_Class) return Non_Terminal_Index_Type;
   function Rule_Number_Hi(Self : Grammar_Class) return Non_Terminal_Index_Type;
   function Production_Number_Lo(Self : Grammar_Class) return kv.apg.lalr.rules.Production_Index_Type;
   function Production_Number_Hi(Self : Grammar_Class) return kv.apg.lalr.rules.Production_Index_Type;
   function Terminal_Lo(Self : Grammar_Class) return Terminal_Index_Type;
   function Terminal_Hi(Self : Grammar_Class) return Terminal_Index_Type;

   function Grammar_Symbols(Self : Grammar_Class) return Symbol_Vectors.Vector;
   function Translate(Self : Grammar_Class; Terminal : Terminal_Index_Type) return Constant_Symbol_Pointer;
   function Translate(Self : Grammar_Class; Terminals : Terminal_Sets.Set) return Symbol_Vectors.Vector;

   function First_Kernel_Set(Self : Grammar_Class) return kv.apg.lalr.rules.Item_Sets.Set;

   function Closure
      (Self   : Grammar_Class;
       Kernel : kv.apg.lalr.rules.Item_Sets.Set;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return kv.apg.lalr.rules.Item_Sets.Set;

   type State_Index_Type is new Natural;
   function Img(I : State_Index_Type) return String;


   function Goto_Step_Over_One -- goto(I, X)
      (Self   : Grammar_Class;
       Kernel : kv.apg.lalr.rules.Constant_Item_Pointer;
       Index  : State_Index_Type;
       Symbol : Constant_Symbol_Pointer;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return kv.apg.lalr.rules.Item_Sets.Set;

   function Goto_Step_Over -- goto(I, X)
      (Self   : Grammar_Class;
       Kernel : kv.apg.lalr.rules.Item_Sets.Set;
       Index  : State_Index_Type;
       Symbol : Constant_Symbol_Pointer;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return kv.apg.lalr.rules.Item_Sets.Set;

   function Goto_Step_Into_One -- goto(I, X)
      (Self   : Grammar_Class;
       Kernel : kv.apg.lalr.rules.Constant_Item_Pointer;
       Index  : State_Index_Type;
       Symbol : Constant_Symbol_Pointer;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return kv.apg.lalr.rules.Item_Sets.Set;

   function Goto_Step_Into -- goto(I, X)
      (Self   : Grammar_Class;
       Kernel : kv.apg.lalr.rules.Item_Sets.Set;
       Index  : State_Index_Type;
       Symbol : Constant_Symbol_Pointer;
       Logger : kv.apg.logger.Safe_Logger_Pointer) return kv.apg.lalr.rules.Item_Sets.Set;




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
         Kernels : kv.apg.lalr.rules.Item_Sets.Set;
      end record;

   package State_Space is new Ada.Containers.Vectors
      (Index_Type   => State_Index_Type,
       Element_Type => State_Definition_Type);


   type State_Information_Type is tagged private;

   procedure Initialize
      (Self    : in out State_Information_Type;
       Grammar : in     Grammar_Pointer;
       Logger  : in     kv.apg.logger.Safe_Logger_Pointer);

   function Number_Of_States(Self : State_Information_Type) return State_Index_Type;
   function Get_States(Self : State_Information_Type) return State_Space.Vector;
   function Get_Hints(Self : State_Information_Type) return Action_Space.Vector;

--   function Generate_Parser_States(Self : Grammar_Pointer; Logger : kv.apg.logger.Safe_Logger_Pointer) return State_Information_Type;


private

   procedure Process_Source_Kernels
      (Self    : in out State_Information_Type;
       Grammar : in     Grammar_Pointer;
       Logger  : in     kv.apg.logger.Safe_Logger_Pointer;
       Current : in     State_Index_Type);

   procedure Add_State
      (Self    : in out State_Information_Type;
       Logger  : in     kv.apg.logger.Safe_Logger_Pointer;
       Kernels : in     kv.apg.lalr.rules.Item_Sets.Set);




   type Grammar_Class is tagged
      record
         Tokens : kv.apg.enum.Enumeration_Class;
         Rules  : kv.apg.lalr.rules.Rule_Maps.Map;
         All_Symbols : Symbol_Vectors.Vector;
         Start  : String_Type;
         Count  : Natural := 0; -- Count of rules
         Max_P  : Natural := 0;
         Max_T  : Natural := 0;
         Errors : Natural := 0;
      end record;


   type State_Information_Type is tagged
      record
         States : State_Space.Vector;
         Hints  : Action_Space.Vector;
         Count  : State_Index_Type := 0;
         Found_More : Boolean := False;
         Working_Hint : Action_Hint_Type;
      end record;


end kv.apg.lalr.grammars;
