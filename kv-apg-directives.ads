with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.regex;

package kv.apg.directives is

   type Directive_Class is abstract tagged
      record
         Name  : String_Type;
      end record;
   procedure Process(Self : in out Directive_Class) is abstract;
   function Get_Name(Self : in     Directive_Class) return String_Type;

   type Directive_Pointer_Type is access all Directive_Class'CLASS;
   procedure Free(Directive : in out Directive_Pointer_Type);

   type Set_Class is new Directive_Class with private;
   not overriding procedure Initialize
      (Self  : in out Set_Class;
       Name  : in     String_Type;
       Value : in     String_Type);
   overriding procedure Process(Self : in out Set_Class);
   not overriding function Get_Value(Self : in     Set_Class) return String_Type;

   type Token_Class is new Directive_Class with private;
   not overriding procedure Initialize
      (Self : in out Token_Class;
       Name : in     String_Type;
       Tree : in     kv.apg.regex.Node_Pointer_Type);
   overriding procedure Process(Self : in out Token_Class);
   not overriding function Get_Tree(Self : in     Token_Class) return kv.apg.regex.Node_Pointer_Type;

private

   type Set_Class is new Directive_Class with
      record
         Value : String_Type;
      end record;

   type Token_Class is new Directive_Class with
      record
         Tree : kv.apg.regex.Node_Pointer_Type;
      end record;

end kv.apg.directives;
