with kv.apg.rules.tables;
with kv.apg.rules.stacks;
with kv.apg.rules.grammars;

package kv.apg.rules.engines is

   use kv.apg.rules.grammars;

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
         Actions  : kv.apg.rules.tables.Action_Table_Class;
         Gotos    : kv.apg.rules.tables.Goto_Table_Class;
         Accepted : Boolean := False;
         Errors   : Natural := 0;
      end record;


end kv.apg.rules.engines;
