with Ada.Containers.Vectors;

with kv.apg.directives;
with kv.apg.tokens;

private package kv.apg.parse.keywords is

   package Token_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => kv.apg.tokens.Token_Class,
       "=" => kv.apg.tokens."=");

   type Keywords_Expectation_Type is (Colon_Or_Equal, Pre_Or_Star, Star, Post_Or_Equal, Equal, Keyword, Keyword_Or_Eos);
   type Keywords_State_Class is new State_Class with
      record
         Expect       : Keywords_Expectation_Type := Colon_Or_Equal;
         Pre_Pattern  : kv.apg.tokens.Token_Class;
         Post_Pattern : kv.apg.tokens.Token_Class;
         Keywords     : Token_Vectors.Vector;
      end record;
   overriding procedure Ingest_Token
      (Self  : in out Keywords_State_Class;
       Token : in     kv.apg.tokens.Token_Class);
   overriding function Get_Directive(Self : in out Keywords_State_Class) return kv.apg.directives.Directive_Pointer_Type;

end kv.apg.parse.keywords;
