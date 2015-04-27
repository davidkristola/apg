with Ada.Text_IO;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Deallocation;

with kv.apg.tokens;
with kv.apg.lex;
with kv.core.wwstr;

package body kv.apg.parse.token is

   use Ada.Text_IO;
   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Ada.Characters.Conversions;
   use kv.apg.tokens;
   use kv.apg.regex;
   use kv.apg.lex;
   use kv.core.wwstr;

   -------------------------------------------------------------------------
   procedure Expect_Name
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Word then
         Self.Name_Token := Token;
         Self.Expect := Equal;
      else
         Self.Status := Done_Error;
      end if;
   end Expect_Name;

   -------------------------------------------------------------------------
   procedure Expect_Equal
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Symbol and then To_String(+Token.Get_Data) = "=" then
         Self.Expect := Value;
      else
         Self.Status := Done_Error;
      end if;
   end Expect_Equal;


   -------------------------------------------------------------------------
   -- Check to see if the tree is complete as it currently stands and
   -- set the expect value accordingly.
   --
   procedure Set_Expect(Self : in out Token_State_Class) is
   begin
      if Self.Tree.Is_Complete then
         Self.Expect := Value_Or_Eos;
      else
         Self.Expect := Value_To_Complete;
      end if;
   end Set_Expect;

   -------------------------------------------------------------------------
   procedure Ingest_Constant
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
      Node : kv.apg.regex.Node_Pointer_Type;
   begin
      Node := Allocate_Node(Token);
      Self.Tree.Append(Node);
      Set_Expect(Self);
   end Ingest_Constant;

   -------------------------------------------------------------------------
   procedure Ingest_Dot --TODO: same as Ingest_Constant
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
      Node : kv.apg.regex.Node_Pointer_Type;
   begin
      Node := Allocate_Node(Token);
      Self.Tree.Append(Node);
      Set_Expect(Self);
   end Ingest_Dot;

   -------------------------------------------------------------------------
   procedure Ingest_Or
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
      Or_Node : kv.apg.regex.Or_Node_Pointer_Type;
      A : kv.apg.regex.Node_Pointer_Type;
   begin
--      Put_Line("Starting or token processing");
      Or_Node := new kv.apg.regex.Or_Node_Class;
      Or_Node.Initialize;
      Self.Tree.Detach(A); --!@#$ this isn't right. need to detach node from the working site, not the top level list
--      Put_Line("A is " & (if (A = null) then "null" else "good"));
      Or_Node.Set_A(A);
      Self.Tree.Append(kv.apg.regex.Node_Pointer_Type(Or_Node));

      Set_Expect(Self);
--      Put_Line("Next expect state is " & Expectation_Type'IMAGE(Self.Expect));
   end Ingest_Or;

   -------------------------------------------------------------------------
   procedure Ingest_Star
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
      Star_Node : kv.apg.regex.Star_Node_Pointer_Type;
      A : kv.apg.regex.Node_Pointer_Type;
   begin
--      Put_Line("Starting star token processing");
      Star_Node := new kv.apg.regex.Star_Node_Class;
      Star_Node.Initialize;
      Self.Tree.Detach(A); --!@#$ this isn't right. need to detach node from the working site, not the top level list
--      Put_Line("A is " & (if (A = null) then "null" else "good"));
      Star_Node.Set_A(A);
      Self.Tree.Append(kv.apg.regex.Node_Pointer_Type(Star_Node));

      Set_Expect(Self);
--      Put_Line("Next expect state is " & Expectation_Type'IMAGE(Self.Expect));
   end Ingest_Star;

   -------------------------------------------------------------------------
   procedure Ingest_Subsequence
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
      Sub_Node : Subsequence_Node_Pointer_Type;
   begin
--      Put_Line("Starting Subsequence token processing");
      Sub_Node := new kv.apg.regex.Subsequence_Node_Class;
      Sub_Node.Initialize;
      Self.Tree.Append(kv.apg.regex.Node_Pointer_Type(Sub_Node));

      Set_Expect(Self);
--      Put_Line("Next expect state is " & Expectation_Type'IMAGE(Self.Expect));
   end Ingest_Subsequence;

   -------------------------------------------------------------------------
   procedure Ingest_Symbol
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) with Pre => (Token.Get_Kind = A_Symbol) is
   begin
--      Put_Line("Got symbol " & Token.Get_Data_As_String);
      if Token.Get_Data_As_String = "." then
         Ingest_Dot(Self, Token);
      elsif Token.Get_Data_As_String = "|" then
         Ingest_Or(Self, Token);
      elsif Token.Get_Data_As_String = "*" then
         Ingest_Star(Self, Token);
      elsif Token.Get_Data_As_String = "(" then
         Ingest_Subsequence(Self, Token);
      else
         Self.Status := Done_Error; --TODO
      end if;
   end Ingest_Symbol;

   -------------------------------------------------------------------------
   procedure Expect_Value
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      --TODO: finish this logic
      if Token.Get_Kind = A_Char or else Token.Get_Kind = A_String or else Token.Get_Kind = A_Block then
         Ingest_Constant(Self, Token);
      elsif Token.Get_Kind = A_Symbol then
         Ingest_Symbol(Self, Token);
      else
         Self.Status := Done_Error;
      end if;
   end Expect_Value;

   -------------------------------------------------------------------------
   procedure Ingest_Completion
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is

      Root : kv.apg.regex.Node_Pointer_Type;
      Incomplete : kv.apg.regex.Node_Pointer_Type;
   begin
--      Put_Line("Ingest_Completion");

      Root := Self.Tree.Get_Root;
--      Put_Line("Root is " & (if (Root = null) then "null" else "good")); -- To_String(+Root.Image_This)
      Incomplete := Root.Get_Incomplete;
--      Put_Line("Incomplete is " & (if (Incomplete = null) then "null" else "good"));
      Incomplete.Complete_With(Token);

      Set_Expect(Self);
--      Put_Line("Next expect state is " & Expectation_Type'IMAGE(Self.Expect));
   end Ingest_Completion;

   -------------------------------------------------------------------------
   procedure Expect_Eos
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Is_Eos then
         Self.Status := Done_Good;
      else
         Self.Status := Done_Error;
      end if;
   end Expect_Eos;

   -------------------------------------------------------------------------
   procedure Ingest_Token
      (Self  : in out Token_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      case Self.Expect is
         when Name =>
            Expect_Name(Self, Token);
         when Equal =>
            Expect_Equal(Self, Token);
         when Value_Or_Eos =>
            if Self.Expect = Value_Or_Eos and then Token.Is_Eos then
               Self.Status := Done_Good;
            else
               Expect_Value(Self, Token);
            end if;
         when Value =>
            Expect_Value(Self, Token);
         when Value_To_Complete =>
            Ingest_Completion(Self, Token);
         when Eos =>
            Expect_Eos(Self, Token);
      end case;
   end Ingest_Token;

   -------------------------------------------------------------------------
   function Get_Directive(Self : Token_State_Class) return kv.apg.directives.Directive_Pointer_Type is
      Directive : access kv.apg.directives.Token_Class;
   begin
      Directive := new kv.apg.directives.Token_Class;
      Directive.Initialize(Name => Self.Name_Token.Get_Data, Tree => Self.Tree);
      return kv.apg.directives.Directive_Pointer_Type(Directive);
   end Get_Directive;

end kv.apg.parse.token;
