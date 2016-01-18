
with kv.apg.rules.stacks;

package kv.apg.rules.engines is


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

   type Parser_Engine_Class is tagged
      record
         Grammar  : Grammar_Pointer;
         States   : State_Space.Vector;
         Stack    : kv.apg.rules.stacks.Stack_Class;
         Actions  : Action_Table_Class;
         Gotos    : Goto_Table_Class;
         Accepted : Boolean := False;
         Errors   : Natural := 0;
      end record;


end kv.apg.rules.engines;
