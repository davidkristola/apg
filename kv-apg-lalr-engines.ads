with kv.apg.lalr.tables;
with kv.apg.lalr.stacks;
with kv.apg.lalr.grammars;

package kv.apg.lalr.engines is

   use kv.apg.lalr.grammars;

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
         Stack    : kv.apg.lalr.stacks.Stack_Class;
         Actions  : kv.apg.lalr.tables.Action_Table_Class;
         Gotos    : kv.apg.lalr.tables.Goto_Table_Class;
         Accepted : Boolean := False;
         Errors   : Natural := 0;
      end record;


end kv.apg.lalr.engines;
