with Ada.Containers.Vectors;

with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.directives;
with kv.apg.tokens;
with kv.apg.rules;

private package kv.apg.parse.rule is

   use kv.apg.tokens;

   package Token_Vector is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => kv.apg.tokens.Token_Class);


   type Production_Type is
      record
         Elements : Token_Vector.Vector;
         Code     : String_Type;
      end record;


   package Production_Vector is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Production_Type);




   type Expectation_Type is (Name, Equal, Flag_Or_Production, Production_Or_Eos, Element_Or_Causes, Code);
   type Rule_State_Class is new State_Class with
      record
         Expect      : Expectation_Type := Name;
         Name_Token  : kv.apg.tokens.Token_Class;
         Productions : Production_Vector.Vector;
         Working     : Production_Type;
      end record;

   overriding procedure Ingest_Token
      (Self  : in out Rule_State_Class;
       Token : in     kv.apg.tokens.Token_Class);
   overriding function Get_Directive(Self : Rule_State_Class) return kv.apg.directives.Directive_Pointer_Type;

end kv.apg.parse.rule;
