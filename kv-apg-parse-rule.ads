with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.directives;
with kv.apg.tokens;
with kv.apg.rules;

private package kv.apg.parse.rule is


   type Expectation_Type is (Name, Equal, Flag_Or_Production, Production_Or_Eos, Element_Or_Causes, Code);
   type Rule_State_Class is new State_Class with
      record
         Expect      : Expectation_Type := Name;
         Name_Token  : kv.apg.tokens.Token_Class;
         Productions : kv.apg.rules.Production_Vectors.Vector;
         Working     : kv.apg.rules.Production_Pointer;
         Start_Flag  : Boolean := False;
      end record;

   overriding procedure Ingest_Token
      (Self  : in out Rule_State_Class;
       Token : in     kv.apg.tokens.Token_Class);
   overriding function Get_Directive(Self : in out Rule_State_Class) return kv.apg.directives.Directive_Pointer_Type;

end kv.apg.parse.rule;
