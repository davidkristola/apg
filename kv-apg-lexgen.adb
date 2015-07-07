
with kv.apg.directives;

package body kv.apg.lexgen is


   package Token_Count_Util is
      type Visitor_Class is new kv.apg.directives.Directive_Visitor_Class with
         record
            S : Natural := 0;
            T : Natural := 0;
         end record;
      overriding procedure Process_Set
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Set_Class'CLASS);

      overriding procedure Process_Token
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Token_Class'CLASS);
   end Token_Count_Util;

   package body Token_Count_Util is
      overriding procedure Process_Set
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Set_Class'CLASS) is
      begin
         Self.S := Self.S + 1;
      end Process_Set;

      overriding procedure Process_Token
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Token_Class'CLASS) is
      begin
         Self.T := Self.T + 1;
      end Process_Token;
   end Token_Count_Util;


   ----------------------------------------------------------------------------
   procedure Initialize
      (Self   : in out Generator_Class;
       Parser : in     kv.apg.parse.Parser_Pointer_Type) is
      V : Token_Count_Util.Visitor_Class;
   begin
      Self.Parser := Parser;
      Parser.Process_Directives(V);
      Self.Tokens := V.T;
   end Initialize;

   ----------------------------------------------------------------------------
   function Token_Count(Self : Generator_Class) return Natural is
   begin
      return Self.Tokens;
   end Token_Count;

end kv.apg.lexgen;
