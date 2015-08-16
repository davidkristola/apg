
with kv.apg.enum;
with kv.apg.directives;

package body kv.apg.lexgen is


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
