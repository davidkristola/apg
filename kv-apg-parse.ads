with kv.apg.tokens;
with kv.apg.directives;

package kv.apg.parse is

   type Parser_Class is tagged private;

   procedure Initialise
      (Self : in out Parser_Class);

   procedure Ingest_Token
      (Self  : in out Parser_Class;
       Token : in     kv.apg.tokens.Token_Class);

   function Inbetween_Directives
      (Self : in     Parser_Class) return Boolean;

   function Error_Count
      (Self : in     Parser_Class) return Natural;

   function Directive_Count
      (Self : in     Parser_Class) return Natural;

   function Next_Directive
      (Self : in out Parser_Class) return kv.apg.directives.Directive_Pointer_Type;

private

   type Expectation_Type is (Initialize, Directive,
      Set_Name, Set_Equal, Set_Value, Set_Eos, -- Set directive expectations
      -- Other directive expectations here
      Error_Eos); -- Look for a terminal so that the parser can recover from an error

   type Parser_Class is tagged
      record
         Expect : Expectation_Type := Initialize;
      end record;

end kv.apg.parse;
