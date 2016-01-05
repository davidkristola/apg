
with kv.apg.directives;
with kv.apg.tokens;
with kv.apg.regex;

private package kv.apg.parse.token is

   function Is_Token_Variant(S : String) return Boolean;

   type Expectation_Type is (Name, Colon_Or_Equal, Flag, Flag_Or_Equal, Equal, Value, Value_Or_Eos);
   type Token_State_Class is new State_Class with
      record
         Kind       : kv.apg.directives.Token_Subtype;
         Expect     : Expectation_Type := Name;
         Name_Token : kv.apg.tokens.Token_Class;
         Tree       : kv.apg.regex.Regular_Expression_Tree_Type;
         Associativity : kv.apg.directives.Token_Associativity_Type := kv.apg.directives.Neither;
         Precedence : kv.apg.directives.Token_Precedence_Type := 0;
      end record;
   type Token_State_Pointer_Type is access Token_State_Class;
   not overriding procedure Initialize(Self : in out Token_State_Class; Token_Variant : in     String);
   overriding procedure Ingest_Token
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class);
   overriding function Get_Directive(Self : Token_State_Class) return kv.apg.directives.Directive_Pointer_Type;

end kv.apg.parse.token;
