with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.regex;

package kv.apg.directives is


   type Directive_Visitor_Class is abstract tagged null record;



   type Directive_Class is abstract tagged
      record
         Name  : String_Type;
      end record;
   procedure Process
      (Self    : in out Directive_Class;
       Visitor : in out Directive_Visitor_Class'CLASS) is abstract;
   function Get_Name(Self : in     Directive_Class) return String_Type;

   type Directive_Pointer_Type is access all Directive_Class'CLASS;
   procedure Free(Directive : in out Directive_Pointer_Type);



   type Set_Class is new Directive_Class with private;
   not overriding procedure Initialize
      (Self  : in out Set_Class;
       Name  : in     String_Type;
       Value : in     String_Type);
   overriding procedure Process(Self : in out Set_Class; Visitor : in out Directive_Visitor_Class'CLASS);
   not overriding function Get_Value(Self : in     Set_Class) return String_Type;



   type Token_Subtype is (Accepting, Pattern, Skipover);

   type Token_Class is new Directive_Class with private;
   not overriding procedure Initialize
      (Self : in out Token_Class;
       Name : in     String_Type;
       Tree : in     kv.apg.regex.Regular_Expression_Tree_Type;
       Kind : in     Token_Subtype);
   overriding procedure Process(Self : in out Token_Class; Visitor : in out Directive_Visitor_Class'CLASS);
   not overriding function Get_Tree(Self : in     Token_Class) return kv.apg.regex.Regular_Expression_Tree_Type;
   not overriding function Get_Subtype(Self : in     Token_Class) return Token_Subtype;


   procedure Process_Set
      (Self      : in out Directive_Visitor_Class;
       Directive : in out Set_Class'CLASS) is abstract;

   procedure Process_Token
      (Self      : in out Directive_Visitor_Class;
       Directive : in out Token_Class'CLASS) is abstract;

private

   type Set_Class is new Directive_Class with
      record
         Value : String_Type;
      end record;

   type Token_Class is new Directive_Class with
      record
         Tree : kv.apg.regex.Regular_Expression_Tree_Type;
         Kind : Token_Subtype;
      end record;

end kv.apg.directives;
