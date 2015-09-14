
with Ada.Strings.Wide_Wide_Unbounded;

with kv.apg.enum;
with kv.apg.directives;
with kv.core.wwstr;

package body kv.apg.lexgen is

   use Ada.Strings.Wide_Wide_Unbounded;
   use kv.core.wwstr;

   package Token_Count_Util is
      type Visitor_Class is new kv.apg.directives.Directive_Visitor_Class with private;
      not overriding procedure Initialize(Self : in out Visitor_Class);
      overriding procedure Process_Token
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Token_Class'CLASS);
      not overriding function Get_Token_Count(Self : Visitor_Class) return Natural;
      not overriding function Get_Enumeration(Self : Visitor_Class) return kv.apg.enum.Enumeration_Class;
   private
      type Visitor_Class is new kv.apg.directives.Directive_Visitor_Class with
         record
            T : Natural := 0;
            Enumeration : kv.apg.enum.Enumeration_Class;
         end record;
   end Token_Count_Util;

   package body Token_Count_Util is
      not overriding procedure Initialize(Self : in out Visitor_Class) is
      begin
         Self.Enumeration.Initialize(To_String_Type("Token_Type"));
         Self.Enumeration.Append(To_String_Type("Invalid"));
      end Initialize;
      overriding procedure Process_Token
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Token_Class'CLASS) is
      begin
         Self.Enumeration.Append(Directive.Get_Name);
         Self.T := Self.T + 1;
      end Process_Token;
      not overriding function Get_Token_Count(Self : Visitor_Class) return Natural is
      begin
         return Self.T;
      end Get_Token_Count;
      not overriding function Get_Enumeration(Self : Visitor_Class) return kv.apg.enum.Enumeration_Class is
      begin
         return Self.Enumeration;
      end Get_Enumeration;
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
      V.Initialize;
      Parser.Process_Directives(V);
      Self.Tokens := V.Get_Token_Count;
      Self.Token_Enum := V.Get_Enumeration;
   end Initialize;

   ----------------------------------------------------------------------------
   overriding function Convert
      (Self      : in out Generator_Class;
       Prefix    : in     String_Type;
       Postfix   : in     String_Type;
       Template  : in     String_Type) return kv.apg.writer.buffer.Buffer_Class'CLASS is
      Transformed : kv.apg.writer.buffer.Buffer_Writer_Class;
   begin
      Transformed.Write_Some(Prefix);
      if Template = +"package_name" then
         Self.Insert_Package_Name(Transformed);
      elsif Template = +"token_type" then
         Self.Insert_Token_Type(Transformed);
      else
         Transformed.Write_Some("ERROR");
      end if;
      Transformed.Write_Line(Postfix);
      return kv.apg.writer.buffer.Buffer_Class'CLASS(Transformed);
   end Convert;


   ----------------------------------------------------------------------------
   not overriding procedure Insert_Package_Name
      (Self   : in out Generator_Class;
       Buffer : in out kv.apg.writer.buffer.Buffer_Writer_Class) is
   begin
      Buffer.Write_Some(Self.Package_Name);
   end Insert_Package_Name;

   ----------------------------------------------------------------------------
   not overriding procedure Insert_Token_Type -- "token_type"
      (Self   : in out Generator_Class;
       Buffer : in out kv.apg.writer.buffer.Buffer_Writer_Class) is
   begin
--      Buffer.Write_Some(Self.Package_Name);
      Self.Token_Enum.Write(Buffer);
   end Insert_Token_Type;



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
