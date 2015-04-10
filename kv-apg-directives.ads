package kv.apg.directives is

   type Directive_Class is abstract tagged null record;
   procedure Process(Self : in out Directive_Class) is abstract;

   type Directive_Pointer_Type is access all Directive_Class'CLASS;

   type Set_Class is new Directive_Class with null record;
   procedure Process(Self : in out Set_Class);

end kv.apg.directives;
