
with Ada.Strings.Wide_Wide_Unbounded;

with kv.core.wwstr;

with kv.apg.enum;
with kv.apg.directives;
with kv.apg.fa.nfa.convert;
with kv.apg.tokens;
with kv.apg.locations;

package body kv.apg.lexgen is

   use Ada.Strings.Wide_Wide_Unbounded;
   use kv.core.wwstr;

   --==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--
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

   --==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--
   package body Token_Count_Util is

      -------------------------------------------------------------------------
      not overriding procedure Initialize
         (Self : in out Visitor_Class;
          Nfas : access kv.apg.fa.nfa.Nfa_Array_Type) is
      begin
         Self.Enumeration.Initialize(To_String_Type("Token_Type"));
         Self.Enumeration.Append(kv.apg.tokens.Invalid_Token);
         Self.Token_Number := 0; -- Invalid is 0
         Self.Nfas := Nfas;
      end Initialize;

      -------------------------------------------------------------------------
      overriding procedure Process_Token
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Token_Class'CLASS) is
      begin
         Self.Token_Number := Self.Token_Number + 1;
         Self.Enumeration.Append(Directive.Get_Name_Token);
         Self.Nfas(Self.Token_Number) := kv.apg.directives.Token_Class'CLASS(Directive).Get_Tree.To_Nfa(kv.apg.fast.Key_Type(Self.Token_Number));
      end Process_Token;

      -------------------------------------------------------------------------
      not overriding function Get_Token_Count(Self : Visitor_Class) return Natural is
      begin
         return Self.Token_Number;
      end Get_Token_Count;

      -------------------------------------------------------------------------
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
      Combined.Initialize(Nfas(1..Self.Tokens)); -- There might be more space than tokens
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
      elsif Template = +"state_list" then
         Self.Insert_State_List(Transformed);
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
   not overriding procedure Insert_State_List -- "state_list"
      (Self   : in out Generator_Class;
       Buffer : in out kv.apg.writer.buffer.Buffer_Writer_Class) is
   begin
      Source_Code_States(Self.Get_States, Self.Get_Tokens, Buffer);
   end Insert_State_List;



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
   function Img(Index : Positive) return String is
      Image : String := Positive'IMAGE(Index);
   begin
      return Image(2..Image'LAST);
   end Img;


   ----------------------------------------------------------------------------
   procedure Write_Transitions
      (States : in     kv.apg.fast.State_List_Type;
       Buffer : in out kv.apg.writer.buffer.Buffer_Writer_Class) is
      use kv.apg.fast;
      Index : Positive := 1;
   begin
      for S of States loop
         if Get_Transitions(S) /= null then
            Buffer.Write_Line("   T"&Img(Index)&" : aliased constant Transition_List_Type := ("&Source_Code(Get_Transitions(S).all)&");");
         end if;
         Index := Index + 1;
      end loop;
   end Write_Transitions;

   ----------------------------------------------------------------------------
   -- Precondition: State is accepting
   function Get_State_Token
      (State  : in     kv.apg.fast.State_Type;
       Tokens : in     kv.apg.enum.Enumeration_Class) return String_Type is
      use kv.apg.fast;
      Token_Number : Positive;
   begin
      Token_Number := Positive(Get_Key(State) + 1); -- Add one because Invalid is the real first enumerate.
      return Tokens.Get(Token_Number);
   end Get_State_Token;

   ----------------------------------------------------------------------------
   procedure Write_Token_For_State
      (State  : in     kv.apg.fast.State_Type;
       Tokens : in     kv.apg.enum.Enumeration_Class;
       Buffer : in out kv.apg.writer.buffer.Buffer_Writer_Class) is
      use kv.apg.fast;
   begin
      if Is_Accepting(State) then
         Buffer.Write_Some(Get_State_Token(State, Tokens));
      else
         Buffer.Write_Some("Invalid");
      end if;
   end Write_Token_For_State;

   ----------------------------------------------------------------------------
   procedure Write_Transition_For_State
      (State  : in     kv.apg.fast.State_Type;
       Line   : in     String;
       Buffer : in out kv.apg.writer.buffer.Buffer_Writer_Class) is
      use kv.apg.fast;
   begin
      if Get_Transitions(State) = null then
         Buffer.Write_Some("null)");
      else
         Buffer.Write_Some("T" & Line & "'ACCESS)");
      end if;
   end Write_Transition_For_State;

   ----------------------------------------------------------------------------
   procedure Write_States
      (States : in     kv.apg.fast.State_List_Type;
       Tokens : in     kv.apg.enum.Enumeration_Class;
       Buffer : in out kv.apg.writer.buffer.Buffer_Writer_Class) is

      -------------------------------------------------------------------------
      procedure Write_State_Line
         (Line  : in     String;
          Done  : in     Boolean;
          State : in     kv.apg.fast.State_Type) is
      begin
         Buffer.Write_Some("   " & Line & " => (");
         Write_Token_For_State(State, Tokens, Buffer);
         Buffer.Write_Some(", ");
         Write_Transition_For_State(State, Line, Buffer);
         Buffer.Write_Line( (if Done then ");" else ",") );
      end Write_State_Line;

      Index : Positive := 1;

   begin
      for S of States loop
         Write_State_Line(Img(Index), (Index = States'LENGTH), S);
         Index := Index + 1;
      end loop;
   end Write_States;


   ----------------------------------------------------------------------------
   procedure Source_Code_States
      (States : in     kv.apg.fast.State_List_Type;
       Tokens : in     kv.apg.enum.Enumeration_Class;
       Buffer : in out kv.apg.writer.buffer.Buffer_Writer_Class) is

   begin
      Write_Transitions(States, Buffer);
      Buffer.Write_Line("   State_List : aliased constant State_List_Type := (");
      Write_States(States, Tokens, Buffer);
      Buffer.Write_Line("   Nfa_Definition : aliased constant Nfa_Class := (Start => 1, States => State_List'ACCESS);");
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
