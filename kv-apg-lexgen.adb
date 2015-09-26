
with Ada.Strings.Wide_Wide_Unbounded;

with kv.apg.enum;
with kv.apg.directives;
with kv.core.wwstr;
with kv.apg.fa.nfa.convert;

package body kv.apg.lexgen is

   use Ada.Strings.Wide_Wide_Unbounded;
   use kv.core.wwstr;

   package Token_Count_Util is
      type Visitor_Class is new kv.apg.directives.Directive_Visitor_Class with private;
      not overriding procedure Initialize
         (Self : in out Visitor_Class;
          Nfas : access kv.apg.fa.nfa.Nfa_Array_Type);
      overriding procedure Process_Token
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Token_Class'CLASS);
      not overriding function Get_Token_Count(Self : Visitor_Class) return Natural;
      not overriding function Get_Enumeration(Self : Visitor_Class) return kv.apg.enum.Enumeration_Class;
   private
      type Visitor_Class is new kv.apg.directives.Directive_Visitor_Class with
         record
            Token_Number : Natural := 0;
            Enumeration  : kv.apg.enum.Enumeration_Class;
            Nfas         : access kv.apg.fa.nfa.Nfa_Array_Type;
         end record;
   end Token_Count_Util;

   package body Token_Count_Util is
      not overriding procedure Initialize
         (Self : in out Visitor_Class;
          Nfas : access kv.apg.fa.nfa.Nfa_Array_Type) is
      begin
         Self.Enumeration.Initialize(To_String_Type("Token_Type"));
         Self.Enumeration.Append(To_String_Type("Invalid"));
         Self.Token_Number := 0; -- Invalid is 0
         Self.Nfas := Nfas;
      end Initialize;

      overriding procedure Process_Token
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Token_Class'CLASS) is
      begin
         Self.Token_Number := Self.Token_Number + 1;
         Self.Enumeration.Append(Directive.Get_Name);
         Self.Nfas(Self.Token_Number) := kv.apg.directives.Token_Class'CLASS(Directive).Get_Tree.To_Nfa(kv.apg.fast.Key_Type(Self.Token_Number));
      end Process_Token;

      not overriding function Get_Token_Count(Self : Visitor_Class) return Natural is
      begin
         return Self.Token_Number;
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

      Visitor   : Token_Count_Util.Visitor_Class;
      Null_Nfa  : kv.apg.fa.nfa.Nfa_Class;
      Nfas      : aliased kv.apg.fa.nfa.Nfa_Array_Type := (1..Parser.Directive_Count => Null_Nfa);
      Combined  : kv.apg.fa.nfa.Nfa_Class;
      Converter : kv.apg.fa.nfa.convert.To_Cnfa_Class;

   begin
      Self.Parser := Parser;
      Self.Package_Name := Package_Name;
      Visitor.Initialize(Nfas'UNCHECKED_ACCESS);
      Parser.Process_Directives(Visitor);
      Self.Tokens := Visitor.Get_Token_Count;
      Self.Token_Enum := Visitor.Get_Enumeration;
      Combined.Initialize(Nfas);
      Converter.Nfa_To_Cnfa(Combined, Self.Cnfa);
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
      Self.Token_Enum.Write(Buffer);
   end Insert_Token_Type;


   ----------------------------------------------------------------------------
   not overriding function Get_Cnfa(Self : Generator_Class) return kv.apg.fa.nfa.Nfa_Class is
   begin
      return Self.Cnfa;
   end Get_Cnfa;


   ----------------------------------------------------------------------------
   not overriding function Get_States(Self : Generator_Class) return kv.apg.fast.State_List_Type is
      List : kv.apg.fast.State_List_Type(1..0);
      use kv.apg.fast;
   begin
      if Self.Cnfa.Get_State_List = null then
         return List;
      end if;
      return Self.Cnfa.Get_State_List.all;
   end Get_States;


   ----------------------------------------------------------------------------
   not overriding function Get_Tokens(Self : Generator_Class) return kv.apg.enum.Enumeration_Class is
   begin
      return Self.Token_Enum;
   end Get_Tokens;


   ----------------------------------------------------------------------------
   procedure Source_Code_States
      (States : in     kv.apg.fast.State_List_Type;
       Tokens : in     kv.apg.enum.Enumeration_Class;
       Buffer : in out kv.apg.writer.buffer.Buffer_Writer_Class) is
   begin
      --TODO
      Buffer.Write_Line("-- This file is machine generated. Do not edit.");
   end Source_Code_States;


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
