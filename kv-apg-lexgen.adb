
with Ada.Strings.Wide_Wide_Unbounded;

with kv.apg.enum;
with kv.apg.directives;

package body kv.apg.lexgen is

   use Ada.Strings.Wide_Wide_Unbounded;

   package Token_Count_Util is
      type Visitor_Class is new kv.apg.directives.Directive_Visitor_Class with private;
      overriding procedure Process_Token
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Token_Class'CLASS);
      not overriding function Get_Token_Count(Self : Visitor_Class) return Natural;
   private
      type Visitor_Class is new kv.apg.directives.Directive_Visitor_Class with
         record
            T : Natural := 0;
         end record;
   end Token_Count_Util;

   package body Token_Count_Util is
      overriding procedure Process_Token
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Token_Class'CLASS) is
      begin
         Self.T := Self.T + 1;
      end Process_Token;
      not overriding function Get_Token_Count(Self : Visitor_Class) return Natural is
      begin
         return Self.T;
      end Get_Token_Count;
   end Token_Count_Util;


   ----------------------------------------------------------------------------
   procedure Initialize
      (Self         : in out Generator_Class;
       Parser       : in     kv.apg.parse.Parser_Pointer_Type;
       Package_Name : in     String_Type) is
      V : Token_Count_Util.Visitor_Class;
   begin
      Self.Parser := Parser;
      Self.Package_Name := Package_Name;
      Parser.Process_Directives(V);
      Self.Tokens := V.Get_Token_Count;
   end Initialize;

   ----------------------------------------------------------------------------
   overriding function Is_Multi_Line(Self : Generator_Class; Original : String_Type) return Boolean is
   begin
      return False;
   end Is_Multi_Line;

   ----------------------------------------------------------------------------
   overriding procedure Convert
      (Self      : in out Generator_Class;
       Template  : in     String_Type;
       Converted :    out String_Type) is
   begin
      if Template = +"package_name" then
         Converted := Self.Package_Name;
         return;
      end if;
      Converted := +"ERROR";
   end Convert;

   ----------------------------------------------------------------------------
   overriding function Convert
      (Self      : in out Generator_Class;
       Prefix    : in     String_Type;
       Postfix   : in     String_Type;
       Template  : in     String_Type) return kv.apg.writer.buffer.Buffer_Class'CLASS is
      Empty : kv.apg.writer.buffer.Buffer_Writer_Class;
   begin
      return kv.apg.writer.buffer.Buffer_Class'CLASS(Empty);
   end Convert;

   ----------------------------------------------------------------------------
   function Token_Count(Self : Generator_Class) return Natural is
   begin
      return Self.Tokens;
   end Token_Count;

   ----------------------------------------------------------------------------
   procedure Write_Spec
      (Self   : in     Generator_Class;
       Writer : in out kv.apg.writer.Writer_Class'CLASS) is
   begin
      Writer.Write_Line("-- This file is machine generated. Do not edit.");
      Writer.New_Line(1);
      Writer.Write_Some("package ");
      Writer.Write_Some(Self.Package_Name);
      Writer.Write_Line(" is");
      Writer.New_Line(1);
      --
      --
      --
      Writer.New_Line(1);
      Writer.Write_Some("end ");
      Writer.Write_Some(Self.Package_Name);
      Writer.Write_Line(";");
   end Write_Spec;

   ----------------------------------------------------------------------------
   procedure Write_Body
      (Self   : in     Generator_Class;
       Writer : in out kv.apg.writer.Writer_Class'CLASS) is
   begin
   null;
   end Write_Body;

end kv.apg.lexgen;
