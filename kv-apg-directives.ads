with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.tokens;
with kv.apg.regex;
with kv.apg.lalr;
with kv.apg.enum;

package kv.apg.directives is


   type Directive_Visitor_Class is abstract tagged null record;


   --
   -- Abstract base Directive_Class
   --

   type Directive_Class is abstract tagged
      record
         Name : kv.apg.tokens.Token_Class;
      end record;
   procedure Process
      (Self    : in out Directive_Class;
       Visitor : in out Directive_Visitor_Class'CLASS) is abstract;
   function Get_Name(Self : in     Directive_Class) return String_Type;
   function Get_Name_Token(Self : in     Directive_Class) return kv.apg.tokens.Token_Class;

   type Directive_Pointer_Type is access all Directive_Class'CLASS;
   procedure Free(Directive : in out Directive_Pointer_Type);


   --
   -- Set_Class
   --

   type Set_Class is new Directive_Class with private;
   not overriding procedure Initialize
      (Self  : in out Set_Class;
       Name  : in     kv.apg.tokens.Token_Class;
       Value : in     String_Type);
   overriding procedure Process(Self : in out Set_Class; Visitor : in out Directive_Visitor_Class'CLASS);
   not overriding function Get_Value(Self : in     Set_Class) return String_Type;


   --
   -- Token_Class
   --

   type Token_Subtype is (Accepting, Pattern, Skipover);

   type Token_Class is new Directive_Class with private;
   not overriding procedure Initialize
      (Self : in out Token_Class;
       Name : in     kv.apg.tokens.Token_Class;
       Tree : in     kv.apg.regex.Regular_Expression_Tree_Type;
       Kind : in     Token_Subtype);
   overriding procedure Process(Self : in out Token_Class; Visitor : in out Directive_Visitor_Class'CLASS);
   not overriding function Get_Tree(Self : in     Token_Class) return kv.apg.regex.Regular_Expression_Tree_Type;
   not overriding function Get_Subtype(Self : in     Token_Class) return Token_Subtype;
   not overriding function Get_Associativity(Self : Token_Class) return kv.apg.enum.Token_Associativity_Type;
   not overriding function Get_Precedence(Self : Token_Class) return kv.apg.enum.Token_Precedence_Type;
   not overriding procedure Set_Associativity(Self : in out Token_Class; Associativity : in kv.apg.enum.Token_Associativity_Type);
   not overriding procedure Set_Precedence(Self : in out Token_Class; Precedence : in kv.apg.enum.Token_Precedence_Type);


   --
   -- Rule_Class
   --

   type Rule_Class is new Directive_Class with private;
   not overriding procedure Initialize
      (Self : in out Rule_Class;
       Name : in     kv.apg.tokens.Token_Class;
       Rule : in     kv.apg.lalr.Rule_Pointer);
   overriding procedure Process(Self : in out Rule_Class; Visitor : in out Directive_Visitor_Class'CLASS);
   not overriding function Get_Rule(Self : Rule_Class) return kv.apg.lalr.Rule_Pointer;


   --
   -- Class methods for Directive_Visitor_Class
   --

   procedure Process_Set
      (Self      : in out Directive_Visitor_Class;
       Directive : in out Set_Class'CLASS) is null;

   procedure Process_Token
      (Self      : in out Directive_Visitor_Class;
       Directive : in out Token_Class'CLASS) is null;

   procedure Process_Rule
      (Self      : in out Directive_Visitor_Class;
       Directive : in out Rule_Class'CLASS) is null;

private

   type Set_Class is new Directive_Class with
      record
         Value : String_Type;
      end record;

   type Token_Class is new Directive_Class with
      record
         Tree : kv.apg.regex.Regular_Expression_Tree_Type;
         Kind : Token_Subtype;
         Associativity : kv.apg.enum.Token_Associativity_Type := kv.apg.enum.Neither;
         Precedence : kv.apg.enum.Token_Precedence_Type := 0;
      end record;

   type Rule_Class is new Directive_Class with
      record
         Rule : kv.apg.lalr.Rule_Pointer;
      end record;

end kv.apg.directives;
