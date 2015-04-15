with kv.core.wwstr; use kv.core.wwstr;

package kv.apg.directives is

   type Directive_Class is abstract tagged null record;
   procedure Process(Self : in out Directive_Class) is abstract;
   function Get_Name(Self : in     Directive_Class) return String_Type is abstract;

   type Directive_Pointer_Type is access all Directive_Class'CLASS;
   procedure Free(Directive : in out Directive_Pointer_Type);

   type Set_Class is new Directive_Class with private;
   not overriding procedure Initialize
      (Self : in out Set_Class;
       Name : in     String_Type);
   overriding procedure Process(Self : in out Set_Class);
   overriding function Get_Name(Self : in     Set_Class) return String_Type;

private

   type Set_Class is new Directive_Class with
      record
         Name : String_Type;
      end record;


end kv.apg.directives;
