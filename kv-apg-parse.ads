with kv.apg.tokens;

package kv.apg.parse is

   type Parser_Class is tagged private;

   procedure Initialise
      (Self : in out Parser_Class);

   procedure Ingest_Token
      (Self  : in out Parser_Class;
       Token : in     kv.apg.tokens.Token_Class);

private

   type Parser_Class is tagged
      record
         Where : Positive;
      end record;

end kv.apg.parse;
