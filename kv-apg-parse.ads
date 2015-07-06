private with Ada.Containers.Doubly_Linked_Lists;

with kv.apg.tokens;
with kv.apg.directives;

package kv.apg.parse is

   type Parser_Class is tagged private;
   type Parser_Pointer_Type is access all Parser_Class;

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

   procedure Process_Directives
      (self    : in out Parser_Class;
       Visitor : in out kv.apg.directives.Directive_Visitor_Class'CLASS);

   procedure Delete_Directives
      (Self : in out Parser_Class);

private

   type Status_Type is (Working, Done_Good, Done_Error);
   subtype Done_Status_Type is Status_Type range Done_Good .. Done_Error;
   type State_Class is abstract tagged
      record
         Status : Status_Type := Working;
      end record;
   procedure Ingest_Token
      (Self  : in out State_Class;
       Token : in     kv.apg.tokens.Token_Class) is abstract;
   function Status(Self : State_Class) return Status_Type;
   function Get_Directive(Self : State_Class) return kv.apg.directives.Directive_Pointer_Type is abstract;

   type State_Pointer_Type is access all State_Class'CLASS;


   use kv.apg.directives; -- "="

   package Directive_List is new Ada.Containers.Doubly_Linked_Lists(kv.apg.directives.Directive_Pointer_Type);

   type Action_Type is (
      Scan,
      Process,
      Recover); -- Look for a terminal so that the parser can recover from an error

   type Parser_Class is tagged
      record
         Action     : Action_Type := Scan;
         Directives : Directive_List.List;
         Substate   : State_Pointer_Type;
         Errors     : Natural := 0;
      end record;

end kv.apg.parse;
