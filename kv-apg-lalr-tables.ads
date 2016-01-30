with kv.apg.lalr.grammars;
with kv.apg.lalr.rules;

package kv.apg.lalr.tables is

   use kv.apg.lalr.grammars;

   type Action_Type is (Shift, Reduce, Accept_Input, Error);

   type Action_Entry_Type(What : Action_Type := Error) is
      record
         Precedence    : kv.apg.enum.Token_Precedence_Type := 0;
         Associativity : kv.apg.enum.Token_Associativity_Type := kv.apg.enum.Neither;
         case What is
            when Shift =>
               Where : State_Index_Type;
            when Reduce =>
               Production : kv.apg.lalr.rules.Production_Index_Type;
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

   procedure Replace_Action
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

   procedure Process_Hint_Action
      (Self   : in out Action_Table_Class;
       Tokens : in     kv.apg.enum.Enumeration_Class;
       Hint   : in     kv.apg.lalr.grammars.Action_Hint_Type;
       Logger : in     kv.apg.logger.Safe_Logger_Pointer);


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

   procedure Process_Hint_Goto
      (Self    : in out Goto_Table_Class;
       Hint    : in     kv.apg.lalr.grammars.Action_Hint_Type;
       Grammar : in     kv.apg.lalr.grammars.Grammar_Pointer;
       Logger  : in     kv.apg.logger.Safe_Logger_Pointer);

private

   type Action_Table_Matrix_Type is array (State_Index_Type range <>, Terminal_Index_Type range <>) of Action_Entry_Type;
   type Action_Table_Matrix_Pointer is access Action_Table_Matrix_Type;

   type Action_Table_Class is tagged
      record
         Table        : Action_Table_Matrix_Pointer;
         Errors       : Natural := 0;
         Replacements : Natural := 0;
         Keeps        : Natural := 0;
      end record;


   type Goto_Table_Matrix_Type is array (State_Index_Type range <>, Non_Terminal_Index_Type range <>) of State_Index_Type;
   type Goto_Table_Matrix_Pointer is access Goto_Table_Matrix_Type;
   type Goto_Table_Class is tagged
      record
         Table : Goto_Table_Matrix_Pointer;
      end record;

end kv.apg.lalr.tables;
