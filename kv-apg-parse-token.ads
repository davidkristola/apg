
with kv.apg.directives;
with kv.apg.tokens;
with kv.apg.regex;

private package kv.apg.parse.token is

   type Expectation_Type is (Name, Equal, Value, Value_Or_Eos, Eos);
   type Token_State_Class is new State_Class with
      record
         Expect     : Expectation_Type := Name;
         Name_Token : kv.apg.tokens.Token_Class;
         Tree       : kv.apg.regex.Node_Pointer_Type;
      end record;
   overriding procedure Ingest_Token
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class);
   overriding function Get_Directive(Self : Token_State_Class) return kv.apg.directives.Directive_Pointer_Type;

end kv.apg.parse.token;
