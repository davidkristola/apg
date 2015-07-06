
with kv.apg.parse;

package kv.apg.lexgen is

   type Generator_Class is tagged private;

   procedure Initialize
      (Self   : in out Generator_Class;
       Parser : in     kv.apg.parse.Parser_Pointer_Type);

   function Token_Count(Self : Generator_Class) return Natural;

private

   type Generator_Class is tagged
      record
         Parser : kv.apg.parse.Parser_Pointer_Type;
      end record;

end kv.apg.lexgen;
