
with kv.apg.directives;

package body kv.apg.lexgen is

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self   : in out Generator_Class;
       Parser : in     kv.apg.parse.Parser_Pointer_Type) is
   begin
      Self.Parser := Parser;
   end Initialize;

   ----------------------------------------------------------------------------
   function Token_Count(Self : Generator_Class) return Natural is
   begin
      return 0;
   end Token_Count;

end kv.apg.lexgen;
