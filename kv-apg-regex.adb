with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Wide_Wide_Unbounded;

with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;


package body kv.apg.regex is

   use Ada.Strings.Wide_Wide_Unbounded; -- &
   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Characters.Conversions;
   use kv.apg.tokens;

   Quotation : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Quotation);

   -------------------------------------------------------------------------
   procedure Linear_Detach(Self : in out Node_Class; Node : out Node_Pointer_Type) is
   begin
      Put_Line("Default Linear_Detach for " & To_String(+Self.Image_Tree));
      Node := Self.Previous;
      if Node /= null then
         Self.Previous := Node.Previous;
         Node.Previous := null;
         Put_Line("detached " & To_String(+Node.Image_Tree) & ", leaving " & To_String(+Self.Image_Tree));
      else
         Put_Line("nothing to detach, returning null.");
      end if;
   end Linear_Detach;

   -------------------------------------------------------------------------
   procedure Linear_Attach(Self : in out Node_Class; Node : in  Node_Pointer_Type) is
   begin
      Put_Line("Default (push down) Linear_Attach for " & To_String(+Self.Image_Tree));
      Node.Prepare_For_Graft(Self);
      Node.Previous := Self.Previous;
      Self.Previous := Node;
      Put_Line("result: " & To_String(+Self.Image_Tree));
   end Linear_Attach;

   -------------------------------------------------------------------------
   procedure Process_Tree(Self : in out Node_Class) is
   begin
      if Self.Previous /= null then
         Self.Previous.Process_Tree;
      end if;
      Node_Class'CLASS(Self).Process_This;
   end Process_Tree;

   -------------------------------------------------------------------------
   procedure Set_Previous(Self : in out Node_Class; Node : in Node_Pointer_Type) is
   begin
      Self.Previous := Node;
   end Set_Previous;

   -------------------------------------------------------------------------
   function Is_Complete(Self : Node_Class) return Boolean is
   begin
      return True; -- Default behavior
   end Is_Complete;

   -------------------------------------------------------------------------
   function Get_Incomplete(Self : in out Node_Class) return Node_Pointer_Type is
   begin
      return null; -- Default behavior
   end Get_Incomplete;


   -------------------------------------------------------------------------
   function Image_Tree(Self : in out Node_Class) return String_Type is
      Answer : String_Type := To_String_Type("");
   begin
      if Self.Previous /= null then
         Answer := Self.Previous.Image_Tree & To_String_Type(" ");
      end if;
      Answer := Answer & Node_Class'CLASS(Self).Image_This;
      return Answer;
   end Image_Tree;


   -------------------------------------------------------------------------
   not overriding procedure Initialize(Self : in out Match_Node_Class; Value : in     String_Type) is
   begin
      Self.Value := Value;
   end Initialize;

   -------------------------------------------------------------------------
   procedure Process_This(Self : in out Match_Node_Class) is
   begin
      Put_Line("Match_Node_Class.Process_This");
   end Process_This;

   -------------------------------------------------------------------------
   procedure Complete_With
      (Self  : in out Node_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      -- Default behavior
      raise Inappropriate_Action_Error;
   end Complete_With;

   -------------------------------------------------------------------------
   overriding function Image_This(Self : in out Match_Node_Class) return String_Type is
   begin
      return Quotation & Self.Value & Quotation;
   end Image_This;

   -------------------------------------------------------------------------
   procedure Prepare_For_Graft
      (Self  : in out Node_Class;
       Box   : in out Reg_Ex_Container_Class'CLASS) is
   begin
      null; -- Default behavior: normally nothing needs to be done
      Put_Line("Default (do nothing) Prepare_For_Graft for " & To_String(+Self.Image_Tree));
   end Prepare_For_Graft;

   -------------------------------------------------------------------------
   procedure Graft_To_Tree
      (Self : in out Node_Class;
       Node : in     Node_Pointer_Type) is
   begin
      Self.Linear_Attach(Node); -- Default behavior for nodes that are always complete
      Put_Line("default graft_to_tree result: " & To_String(+Self.Image_Tree));
   end Graft_To_Tree;



   -------------------------------------------------------------------------
   procedure Detach
      (Tree : in out Node_Pointer_Type;
       Last :    out Node_Pointer_Type) is
   begin
      Last := Tree;
      Tree := Tree.Previous;
      Last.Previous := null;
   end Detach;





   -------------------------------------------------------------------------
   procedure Linear_Detach(Self : in out Regular_Expression_Tree_Type; Node : out Node_Pointer_Type) is
   begin
      Put_Line("Regular_Expression_Tree_Type.Linear_Detach for " & To_String(+Self.Image_Tree));
      Node := Self.Root;
      Self.Root := Node.Previous;
      Node.Previous := null;
      Put_Line("result: " & To_String(+Self.Image_Tree));
   end Linear_Detach;

   -------------------------------------------------------------------------
   procedure Linear_Attach(Self : in out Regular_Expression_Tree_Type; Node : in  Node_Pointer_Type) is
   begin
      Put_Line("Regular_Expression_Tree_Type.Linear_Attach for " & To_String(+Self.Image_Tree));
      Node.Prepare_For_Graft(Self);
      Node.Previous := Self.Root;
      Self.Root := Node;
      Put_Line("result: " & To_String(+Self.Image_Tree));
   end Linear_Attach;


   -------------------------------------------------------------------------
   procedure Detach
      (Tree : in out Regular_Expression_Tree_Type;
       Last :    out Node_Pointer_Type) is
   begin
      Last := Tree.Root;
      Tree.Root := Last.Previous;
      Last.Previous := null;
   end Detach;

   -------------------------------------------------------------------------
   procedure Append
      (Tree : in out Regular_Expression_Tree_Type;
       Last : in     Node_Pointer_Type) is
   begin
      Last.Previous := Tree.Root;
      Tree.Root := Last;
   end Append;

   -------------------------------------------------------------------------
   function Get_Root
      (Tree : in     Regular_Expression_Tree_Type) return Node_Pointer_Type is
   begin
      return Tree.Root;
   end Get_Root;

   -------------------------------------------------------------------------
   function Is_Empty
      (Tree : in     Regular_Expression_Tree_Type) return Boolean is
   begin
      return Tree.Root = null;
   end Is_Empty;

   -------------------------------------------------------------------------
   function Is_Complete
      (Tree : Regular_Expression_Tree_Type) return Boolean is
   begin
      return Tree.Root.Is_Complete;
   end Is_Complete;


   -------------------------------------------------------------------------
   function Get_Incomplete
      (Tree : in     Regular_Expression_Tree_Type) return Node_Pointer_Type is
      Current : Node_Pointer_Type := Tree.Root;
      Next : Node_Pointer_Type;
   begin
      while Current /= null loop
         Next := Current.Get_Incomplete;
         if (Next = null) or else Next.Is_Complete then
            return Current;
         end if;
      end loop;
      return null;
   end Get_Incomplete;

   -------------------------------------------------------------------------
   function Image_Tree(Tree : in     Regular_Expression_Tree_Type) return String_Type is
   begin
      if Tree.Root = null then
         return To_String_Type("null");
      end if;
      return Tree.Root.Image_Tree;
   end Image_Tree;

   -------------------------------------------------------------------------
   function Allocate_Node
      (Token : in     kv.apg.tokens.Token_Class) return Node_Pointer_Type is

      Match_Node : Match_Node_Pointer_Type;
      Wild_Node : Match_Any_Node_Pointer_Type;
      Star_Node : Star_Node_Pointer_Type;
      Sub_Node : Subsequence_Node_Pointer_Type;
      Or_Node : Or_Node_Pointer_Type;

   begin
      Put_Line("(reg ex) Allocate_Node called with <" & Token.Get_Data_As_String & ">");
      --!@#$ horrible assumption!
      if Token.Get_Kind = A_Char or else Token.Get_Kind = A_String or else Token.Get_Kind = A_Block then
         Match_Node := new Match_Node_Class;
         Match_Node.Initialize(Token.Get_Data);
         return Node_Pointer_Type(Match_Node);
      elsif Token.Get_Kind = A_Symbol then
         if Token.Get_Data_As_String = "." then
            Wild_Node := new Match_Any_Node_Class;
            Wild_Node.Initialize;
            return Node_Pointer_Type(Wild_Node);
         elsif Token.Get_Data_As_String = "*" then
            Star_Node := new Star_Node_Class;
            Star_Node.Initialize;
            return Node_Pointer_Type(Star_Node);
         elsif Token.Get_Data_As_String = "(" then
            Sub_Node := new Subsequence_Node_Class;
            Sub_Node.Initialize;
            return Node_Pointer_Type(Sub_Node);
         elsif Token.Get_Data_As_String = "|" then
            Or_Node := new Or_Node_Class;
            Or_Node.Initialize;
            return Node_Pointer_Type(Or_Node);
         end if;
      end if;
      Put_Line("Allocate_Node returning null");
      return null;
   end Allocate_Node;

   -------------------------------------------------------------------------
   procedure Graft_To_Tree
      (Tree  : in out Regular_Expression_Tree_Type;
       Token : in     kv.apg.tokens.Token_Class) is
      Current : Node_Pointer_Type := Tree.Root;
      Addition : Node_Pointer_Type := Allocate_Node(Token);
   begin
      Put_Line("Regular_Expression_Tree_Type.Graft_To_Tree");
      if Current = null then
         Put_Line("Tree is empty, adding first node!");
         Tree.Linear_Attach(Addition);
      elsif Current.Is_Complete then
         Put_Line("Is_Complete, Addition.Prepare_For_Graft...");
--         Addition.Prepare_For_Graft(Tree);
         Put_Line("Tree.Linear_Attach(Addition)...");
         Tree.Linear_Attach(Addition);
      else
         Put_Line("NOT Is_Complete");
         Current.Graft_To_Tree(Addition);
      end if;
      Put_Line("result: " & To_String(+Tree.Image_Tree));
   end Graft_To_Tree;















   -------------------------------------------------------------------------
   not overriding procedure Initialize(Self : in out Match_Any_Node_Class) is
   begin
      null;
   end Initialize;

   -------------------------------------------------------------------------
   procedure Process_This(Self : in out Match_Any_Node_Class) is
   begin
      Put_Line("Match_Any_Node_Class.Process_This");
   end Process_This;

   -------------------------------------------------------------------------
   overriding function Image_This(Self : in out Match_Any_Node_Class) return String_Type is
   begin
      return To_String_Type(".");
   end Image_This;





   -------------------------------------------------------------------------
   not overriding procedure Initialize(Self : in out Or_Node_Class) is
   begin
      null;
   end Initialize;

   -------------------------------------------------------------------------
   overriding procedure Prepare_For_Graft
      (Self  : in out Or_Node_Class;
       Box   : in out Reg_Ex_Container_Class'CLASS) is

      Left : Node_Pointer_Type;

   begin
      Put_Line("Or_Node_Class.Prepare_For_Graft");
      Box.Linear_Detach(Left);
      Put_Line("Or_Node_Class.Prepare_For_Graft, detached and setting A with " & To_String(+Left.Image_Tree));
      Self.Set_A(Left);
      Put_Line("result: " & To_String(+Self.Image_Tree));
   end Prepare_For_Graft;

   -------------------------------------------------------------------------
   not overriding procedure Set_A(Self : in out Or_Node_Class; A : in     Node_Pointer_Type) is
   begin
      Self.A := A;
   end Set_A;

   -------------------------------------------------------------------------
   not overriding procedure Set_B(Self : in out Or_Node_Class; B : in     Node_Pointer_Type) is
   begin
      Self.B := B;
   end Set_B;

   -------------------------------------------------------------------------
   procedure Process_This(Self : in out Or_Node_Class) is
   begin
      Put_Line("Or_Node_Class.Process_This");
   end Process_This;

   -------------------------------------------------------------------------
   overriding function Is_Complete(Self : Or_Node_Class) return Boolean is
   begin
      -- Both A and B must be set and then B is complete
      return (Self.A /= null) and ((Self.B /= null) and then (Self.B.Is_Complete));
   end Is_Complete;

   -------------------------------------------------------------------------
   overriding function Get_Incomplete(Self : in out Or_Node_Class) return Node_Pointer_Type is
   begin
      if Self.B = null then
         return Self'UNCHECKED_ACCESS; --TODO fix this
      elsif (not Self.B.Is_Complete) then
         return Self.B;
      end if;
      return null;
   end Get_Incomplete;

   -------------------------------------------------------------------------
   procedure Complete_With
      (Self  : in out Or_Node_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
--      Put_Line("Or_Node_Class.Complete_With " & Token.Get_Data_As_String);
      Self.B := Allocate_Node(Token);
   end Complete_With;

   -------------------------------------------------------------------------
   overriding function Image_This(Self : in out Or_Node_Class) return String_Type is
   begin
      if Self.B = null then
         return To_String_Type("(") & Self.A.Image_This & To_String_Type("|null");
      end if;
      return To_String_Type("(") & Self.A.Image_This & To_String_Type("|") & Self.B.Image_This & To_String_Type(")");
   end Image_This;

   -------------------------------------------------------------------------
   overriding procedure Graft_To_Tree
      (Self : in out Or_Node_Class;
       Node : in     Node_Pointer_Type) is
   begin
      Put_Line("Or_Node_Class.Graft_To_Tree");
      if Self.Is_Complete then
         Put_Line("Is_Complete");
--         Node.Prepare_For_Graft(Self);
         Put_Line("Self.Linear_Attach(Node)...");
         Self.Linear_Attach(Node);
      else
         --TODO: is it so that only B can be incomplete?
         if Self.B = null then
            Put_Line("Filling in OR's B!");
            Self.B := Node;
         else
            Put_Line("graft to B...");
            Self.B.Graft_To_Tree(Node);
         end if;
      end if;
      Put_Line("result: " & To_String(+Self.Image_Tree));
   end Graft_To_Tree;




   -------------------------------------------------------------------------
   not overriding procedure Initialize(Self : in out Star_Node_Class) is
   begin
      null;
   end Initialize;

   -------------------------------------------------------------------------
   not overriding procedure Set_A(Self : in out Star_Node_Class; A : in     Node_Pointer_Type) is
   begin
      Self.A := A;
   end Set_A;

   -------------------------------------------------------------------------
   overriding procedure Process_This(Self : in out Star_Node_Class) is
   begin
      Put_Line("Star_Node_Class.Process_This");
   end Process_This;

   -------------------------------------------------------------------------
   overriding function Image_This(Self : in out Star_Node_Class) return String_Type is
   begin
      return To_String_Type("(") & Self.A.Image_This & To_String_Type(")") & To_String_Type("*");
   end Image_This;

   -------------------------------------------------------------------------
   overriding procedure Prepare_For_Graft
      (Self  : in out Star_Node_Class;
       Box   : in out Reg_Ex_Container_Class'CLASS) is

      Left : Node_Pointer_Type;

   begin
      Put_Line("Star_Node_Class.Prepare_For_Graft");
      Box.Linear_Detach(Left);
      Put_Line("Star_Node_Class.Prepare_For_Graft, detached and setting A with " & To_String(+Left.Image_Tree));
      Self.Set_A(Left);
      Put_Line("result: " & To_String(+Self.Image_Tree));
   end Prepare_For_Graft;






   -------------------------------------------------------------------------
   not overriding procedure Initialize(Self : in out Subsequence_Node_Class) is
   begin
      null;
   end Initialize;

   -------------------------------------------------------------------------
   not overriding procedure Set_A(Self : in out Subsequence_Node_Class; A : in     Node_Pointer_Type) is
   begin
      Self.A := A;
   end Set_A;

   -------------------------------------------------------------------------
   overriding procedure Process_This(Self : in out Subsequence_Node_Class) is
   begin
      Put_Line("Subsequence_Node_Class.Process_This");
   end Process_This;

   -------------------------------------------------------------------------
   overriding function Is_Complete(Self : Subsequence_Node_Class) return Boolean is
   begin
      if (Self.A = null) or else (not Self.A.Is_Complete) then
         return False;
      else
         return Self.Complete;
      end if;
   end Is_Complete;

   -------------------------------------------------------------------------
   overriding function Get_Incomplete(Self : in out Subsequence_Node_Class) return Node_Pointer_Type is
   begin
      if (Self.A /= null) and then (not Self.A.Is_Complete) then
         return Self.A;
      else
         return Self'UNCHECKED_ACCESS; --TODO fix this
      end if;
   end Get_Incomplete;

   -------------------------------------------------------------------------
   procedure Complete_With
      (Self  : in out Subsequence_Node_Class;
       Token : in     kv.apg.tokens.Token_Class) is
      Node : Node_Pointer_Type;
   begin
      if Token.Get_Data_As_String = ")" then
--         Put_Line("Complete_With: the subsequence is: " & To_String(+Self.A.Image_This));
         Self.Complete := True; --TODO: what if the subsequence isn't complete?!?!?!
      else
         --TODO: make this real
--         if Self.A = null then
--            Put_Line("A is null");
--         else
--            Put_Line("A is self-complete, and is: " & To_String(+Self.A.Image_This));
--         end if;
--         Put_Line("Allocating and appending " & Token.Get_Data_As_String);
         Node := Allocate_Node(Token);
         Node.Set_Previous(Self.A);
         Self.A := Node;
--         Put_Line("A is now: " & To_String(+Self.A.Image_Tree));
      end if;
   end Complete_With;

   -------------------------------------------------------------------------
   overriding function Image_This(Self : in out Subsequence_Node_Class) return String_Type is
   begin
      if Self.A = null then
         return To_String_Type("(...");
      end if;
      if not Self.Complete then
         return To_String_Type("(") & Self.A.Image_Tree & To_String_Type("...");
      end if;
      return To_String_Type("(") & Self.A.Image_Tree & To_String_Type(")");
   end Image_This;

   -------------------------------------------------------------------------
   overriding procedure Graft_To_Tree
      (Self : in out Subsequence_Node_Class;
       Node : in     Node_Pointer_Type) is
   begin
      Put_Line("Subsequence_Node_Class.Graft_To_Tree");
      if Self.A = null then
         Put_Line("A empty, Self.Linear_Attach(Node) at this level");
         Self.Linear_Attach(Node);
      elsif Self.A.Is_Complete then
         Put_Line("A is complete");
         if Node = null then
            Put_Line("null complete!");
            Self.Complete := True; -- Must have been the non-token ')'
         else
            Put_Line("Self.Linear_Attach(Node) at this level");
            Self.Linear_Attach(Node);
         end if;
      else
         Put_Line("A is not complete, going down...");
         Self.A.Graft_To_Tree(Node);
      end if;
      Put_Line("result: " & To_String(+Self.Image_Tree));
   end Graft_To_Tree;

   -------------------------------------------------------------------------
   overriding procedure Linear_Detach(Self : in out Subsequence_Node_Class; Node : out Node_Pointer_Type) is
   begin
      Put_Line("Subsequence_Node_Class.Linear_Detach for " & To_String(+Self.Image_Tree));
      Node := Self.A;
      Self.A := Node.Previous;
      Node.Previous := null;
      Put_Line("result: " & To_String(+Self.Image_Tree));
   end Linear_Detach;

   -------------------------------------------------------------------------
   overriding procedure Linear_Attach(Self : in out Subsequence_Node_Class; Node : in  Node_Pointer_Type) is
   begin
      Put_Line("Subsequence_Node_Class.Linear_Attach for " & To_String(+Self.Image_Tree));
      Node.Prepare_For_Graft(Self);
      Node.Previous := Self.A;
      Self.A := Node;
      Put_Line("result: " & To_String(+Self.Image_Tree));
   end Linear_Attach;

end kv.apg.regex;
