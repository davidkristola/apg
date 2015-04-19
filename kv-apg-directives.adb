with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Conversions;

package body kv.apg.directives is

   use Ada.Characters.Conversions;

   ----------------------------------------------------------------------------
   procedure Free_Instance is new Ada.Unchecked_Deallocation(Directive_Class'CLASS, Directive_Pointer_Type);

   ----------------------------------------------------------------------------
   procedure Free(Directive : in out Directive_Pointer_Type) is
   begin
      Free_Instance(Directive);
   end Free;

   ----------------------------------------------------------------------------
   function Get_Name(Self : in     Directive_Class) return String_Type is
   begin
      return Self.Name;
   end Get_Name;

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self  : in out Set_Class;
       Name  : in     String_Type;
       Value : in     String_Type) is
   begin
      --Put_Line("Set_Class.Initialize Name = '" & To_String(+Name) & "', value = '" & To_String(+Value) & "'");
      Self.Name := Name;
      Self.Value := Value;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Process(Self : in out Set_Class) is
   begin
      Put_Line("Set_Class::Process");
   end Process;

   ----------------------------------------------------------------------------
   function Get_Value(Self : in     Set_Class) return String_Type is
   begin
      return Self.Value;
   end Get_Value;


   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Token_Class;
       Name : in     String_Type;
       Tree : in     kv.apg.regex.Node_Pointer_Type) is
   begin
      Put_Line("Token_Class.Initialize Name = '" & To_String(+Name) & "'");
      Self.Name := Name;
      Self.Tree := Tree;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Process(Self : in out Token_Class) is
   begin
      Put_Line("Token_Class::Process");
   end Process;

   ----------------------------------------------------------------------------
   function Get_Tree(Self : in     Token_Class) return kv.apg.regex.Node_Pointer_Type is
   begin
      return Self.Tree;
   end Get_Tree;

end kv.apg.directives;
