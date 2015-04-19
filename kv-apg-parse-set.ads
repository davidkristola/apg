
with kv.apg.directives;
with kv.apg.tokens;

private package kv.apg.parse.set is

   type Set_Expectation_Type is (Set_Name, Set_Equal, Set_Value, Set_Eos);
   type Set_State_Class is new State_Class with
      record
         Expect      : Set_Expectation_Type := Set_Name;
         Name_Token  : kv.apg.tokens.Token_Class;
         Value_Token : kv.apg.tokens.Token_Class;
      end record;
   overriding procedure Ingest_Token
      (Self  : in out Set_State_Class;
       Token : in     kv.apg.tokens.Token_Class);
   overriding function Get_Directive(Self : Set_State_Class) return kv.apg.directives.Directive_Pointer_Type;

end kv.apg.parse.set;
