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


   Debug : Boolean := False;

   -------------------------------------------------------------------------
   procedure Set_Debug(Value : in Boolean) is
   begin
      Debug := Value;
   end Set_Debug;

   -------------------------------------------------------------------------
   function Allocate_Node
      (Token : in     kv.apg.tokens.Token_Class) return Node_Pointer_Type is

      Match_Node : Match_Node_Pointer_Type;
      Wild_Node : Match_Any_Node_Pointer_Type;
      Star_Node : Star_Node_Pointer_Type;
      Plus_Node : Plus_Node_Pointer_Type;
      Sub_Node : Subsequence_Node_Pointer_Type;
      Or_Node : Or_Node_Pointer_Type;

   begin
      if Debug then Put_Line("(reg ex) Allocate_Node called with <" & Token.Get_Data_As_String & ">"); end if;
      if Token.Get_Kind = A_Char or else Token.Get_Kind = A_String or else Token.Get_Kind = A_Block then
         Match_Node := new Match_Node_Class;
         Match_Node.Initialize(Token.Get_Data);
         return Node_Pointer_Type(Match_Node);
      elsif Token.Get_Kind = A_Symbol then
         if Token.Get_Data_As_String = "." then
            Wild_Node := new Match_Any_Node_Class;
            return Node_Pointer_Type(Wild_Node);
         elsif Token.Get_Data_As_String = "*" then
            Star_Node := new Star_Node_Class;
            return Node_Pointer_Type(Star_Node);
         elsif Token.Get_Data_As_String = "+" then
            Plus_Node := new Plus_Node_Class;
            return Node_Pointer_Type(Plus_Node);
         elsif Token.Get_Data_As_String = "(" then
            Sub_Node := new Subsequence_Node_Class;
            return Node_Pointer_Type(Sub_Node);
         elsif Token.Get_Data_As_String = "|" then
            Or_Node := new Or_Node_Class;
            return Node_Pointer_Type(Or_Node);
         end if;
      end if;
      if Debug then Put_Line("Allocate_Node returning null"); end if;
      return null;
   end Allocate_Node;


   -------------------------------------------------------------------------
   procedure Linear_Detach(Self : in out Node_Class; Node : out Node_Pointer_Type) is
   begin
      raise Inappropriate_Action_Error;
   end Linear_Detach;

   -------------------------------------------------------------------------
   procedure Linear_Attach(Self : in out Node_Class; Node : in  Node_Pointer_Type) is
   begin
      raise Inappropriate_Action_Error;
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
   function Is_Complete(Self : Node_Class) return Boolean is
   begin
      return True; -- Default behavior
   end Is_Complete;

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
   procedure Prepare_For_Graft
      (Self  : in out Node_Class;
       Box   : in out Reg_Ex_Container_Class'CLASS) is
   begin
      null; -- Default behavior: normally nothing needs to be done
      if Debug then Put_Line("Default (do nothing) Prepare_For_Graft for " & To_String(+Self.Image_Tree)); end if;
   end Prepare_For_Graft;

   -------------------------------------------------------------------------
   procedure Graft_To_Tree
      (Self : in out Node_Class;
       Node : in     Node_Pointer_Type) is
   begin
      Self.Linear_Attach(Node); -- Default behavior for nodes that are always complete
      if Debug then Put_Line("default graft_to_tree result: " & To_String(+Self.Image_Tree)); end if;
   end Graft_To_Tree;







   -------------------------------------------------------------------------
   procedure Linear_Detach(Self : in out Regular_Expression_Tree_Type; Node : out Node_Pointer_Type) is
   begin
      if Debug then Put_Line("Regular_Expression_Tree_Type.Linear_Detach for " & To_String(+Self.Image_Tree)); end if;
      Node := Self.Root;
      Self.Root := Node.Previous;
      Node.Previous := null;
      if Debug then Put_Line("result: " & To_String(+Self.Image_Tree)); end if;
   end Linear_Detach;

   -------------------------------------------------------------------------
   procedure Linear_Attach(Self : in out Regular_Expression_Tree_Type; Node : in  Node_Pointer_Type) is
   begin
      if Debug then Put_Line("Regular_Expression_Tree_Type.Linear_Attach for " & To_String(+Self.Image_Tree)); end if;
      Node.Prepare_For_Graft(Self);
      Node.Previous := Self.Root;
      Self.Root := Node;
      if Debug then Put_Line("result: " & To_String(+Self.Image_Tree)); end if;
   end Linear_Attach;







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
   function Image_Tree(Tree : in     Regular_Expression_Tree_Type) return String_Type is
   begin
      if Tree.Root = null then
         return To_String_Type("null");
      end if;
      return Tree.Root.Image_Tree;
   end Image_Tree;

   -------------------------------------------------------------------------
   procedure Graft_To_Tree
      (Tree  : in out Regular_Expression_Tree_Type;
       Token : in     kv.apg.tokens.Token_Class) is
      Current : Node_Pointer_Type := Tree.Root;
      Addition : Node_Pointer_Type := Allocate_Node(Token);
   begin
      if Debug then Put_Line("Regular_Expression_Tree_Type.Graft_To_Tree"); end if;
      if Current = null then
         if Debug then Put_Line("Tree is empty, adding first node!"); end if;
         Tree.Linear_Attach(Addition);
      elsif Current.Is_Complete then
         if Debug then Put_Line("Is_Complete, Tree.Linear_Attach(Addition)..."); end if;
         Tree.Linear_Attach(Addition);
      else
         if Debug then Put_Line("NOT Is_Complete"); end if;
         Current.Graft_To_Tree(Addition);
      end if;
      if Debug then Put_Line("result: " & To_String(+Tree.Image_Tree)); end if;
   end Graft_To_Tree;


   -------------------------------------------------------------------------
   function To_Nfa(Tree : Regular_Expression_Tree_Type; Key : Key_Type) return kv.apg.nfa.Nfa_Class is
      RegEx_NFA : kv.apg.nfa.Nfa_Class;
      States : Natural;
      Start_State : State_Id_Type := 1;
   begin
      if Debug then Put_Line("Regular_Expression_Tree_Type.To_Nfa"); end if;
      States := Tree.Root.Count_Nfa_Transition_Sets + 1;
      if Debug then Put_Line("RegEx tree NFA states = " & Natural'IMAGE(States)); end if;
      RegEx_NFA.Initialize(States, True, Key);
      --RegEx_NFA.Set_State_Accepting(State_Id_Type(States), Key);
      Tree.Root.Set_Nfa_Transitions(RegEx_NFA, Start_State);
      return RegEx_NFA;
   end To_Nfa;










   -------------------------------------------------------------------------
   not overriding procedure Initialize(Self : in out Match_Node_Class; Value : in     String_Type) is
   begin
      Self.Value := Value;
   end Initialize;

   -------------------------------------------------------------------------
   procedure Process_This(Self : in out Match_Node_Class) is
   begin
      if Debug then Put_Line("Match_Node_Class.Process_This"); end if;
   end Process_This;

   -------------------------------------------------------------------------
   overriding function Image_This(Self : in out Match_Node_Class) return String_Type is
   begin
      return Quotation & Self.Value & Quotation;
   end Image_This;

   -------------------------------------------------------------------------
   overriding function Count_Nfa_Transition_Sets(Self : Match_Node_Class) return Natural is
   begin
      return Ada.Strings.Wide_Wide_Unbounded.Length(Self.Value) + (if Self.Previous = null then 0 else Self.Previous.Count_Nfa_Transition_Sets);
   end Count_Nfa_Transition_Sets;

   -------------------------------------------------------------------------
   overriding procedure Set_Nfa_Transitions
      (Self  : in out Match_Node_Class;
       NFA   : in out kv.apg.nfa.Nfa_Class;
       Start : in out State_Id_Type) is
      Transition : Transition_Type;
      First_And_Only_Transition : constant := 1;
      Destination : State_Id_Type;
   begin
      if Debug then Put_Line("Match_Node_Class.Set_Nfa_Transitions starting at " & State_Id_Type'IMAGE(Start)); end if;
      if Self.Previous /= null then
         Self.Previous.Set_Nfa_Transitions(NFA, Start);
      end if;
      for WWC of Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String(Self.Value) loop
         --NFA.Set_State_Non_Accepting(Start, First_And_Only_Transition);
         Destination := Start + 1;
         Set_Match(Transition, Destination, WWC);
         --NFA.Set_State_Transition(Start, First_And_Only_Transition, Transition);
         NFA.Append_State_Transition(Start, Transition);
         Start := Start + 1;
      end loop;
   end Set_Nfa_Transitions;






   -------------------------------------------------------------------------
   procedure Process_This(Self : in out Match_Any_Node_Class) is
   begin
      if Debug then Put_Line("Match_Any_Node_Class.Process_This"); end if;
   end Process_This;

   -------------------------------------------------------------------------
   overriding function Image_This(Self : in out Match_Any_Node_Class) return String_Type is
   begin
      return To_String_Type(".");
   end Image_This;

   -------------------------------------------------------------------------
   overriding function Count_Nfa_Transition_Sets(Self : Match_Any_Node_Class) return Natural is
   begin
      return 1 + (if Self.Previous = null then 0 else Self.Previous.Count_Nfa_Transition_Sets);
   end Count_Nfa_Transition_Sets;

   -------------------------------------------------------------------------
   overriding procedure Set_Nfa_Transitions
      (Self  : in out Match_Any_Node_Class;
       NFA   : in out kv.apg.nfa.Nfa_Class;
       Start : in out State_Id_Type) is
      Transition : Transition_Type;
      Destination : State_Id_Type;
   begin
      if Debug then Put_Line("Match_Any_Node_Class.Set_Nfa_Transitions starting at " & State_Id_Type'IMAGE(Start)); end if;
      if Self.Previous /= null then
         Self.Previous.Set_Nfa_Transitions(NFA, Start);
      end if;
      Destination := Start + 1;
      Set_Any(Transition, Destination);
      NFA.Append_State_Transition(Start, Transition);
      Start := Start + 1;
   end Set_Nfa_Transitions;




   -------------------------------------------------------------------------
   overriding procedure Prepare_For_Graft
      (Self  : in out Or_Node_Class;
       Box   : in out Reg_Ex_Container_Class'CLASS) is

      Left : Node_Pointer_Type;

   begin
      if Debug then Put_Line("Or_Node_Class.Prepare_For_Graft"); end if;
      Box.Linear_Detach(Left);
      if Debug then Put_Line("Or_Node_Class.Prepare_For_Graft, detached and setting A with " & To_String(+Left.Image_Tree)); end if;
      Self.A := Left;
      if Debug then Put_Line("result: " & To_String(+Self.Image_Tree)); end if;
   end Prepare_For_Graft;

   -------------------------------------------------------------------------
   procedure Process_This(Self : in out Or_Node_Class) is
   begin
      if Debug then Put_Line("Or_Node_Class.Process_This"); end if;
   end Process_This;

   -------------------------------------------------------------------------
   overriding function Is_Complete(Self : Or_Node_Class) return Boolean is
   begin
      -- Both A and B must be set and then B is complete
      return (Self.A /= null) and ((Self.B /= null) and then (Self.B.Is_Complete));
   end Is_Complete;

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
      if Debug then Put_Line("Or_Node_Class.Graft_To_Tree"); end if;
      if Self.Is_Complete then
         if Debug then Put_Line("Is_Complete, Self.Linear_Attach(Node)..."); end if;
         Self.Linear_Attach(Node);
      else
         -- Only B can be incomplete
         if Self.B = null then
            if Debug then Put_Line("Filling in OR's B!"); end if;
            Self.B := Node;
         else
            if Debug then Put_Line("graft to B..."); end if;
            Self.B.Graft_To_Tree(Node);
         end if;
      end if;
      if Debug then Put_Line("result: " & To_String(+Self.Image_Tree)); end if;
   end Graft_To_Tree;

   -------------------------------------------------------------------------
   overriding function Count_Nfa_Transition_Sets(Self : Or_Node_Class) return Natural is
   begin
      -- The OR part contributes A + B - 1 transition sets because the first set includes going to A *or* B.
      return (Self.A.Count_Nfa_Transition_Sets + Self.B.Count_Nfa_Transition_Sets - 1) + (if Self.Previous = null then 0 else Self.Previous.Count_Nfa_Transition_Sets);
   end Count_Nfa_Transition_Sets;

   -------------------------------------------------------------------------
   overriding procedure Set_Nfa_Transitions
      (Self  : in out Or_Node_Class;
       NFA   : in out kv.apg.nfa.Nfa_Class;
       Start : in out State_Id_Type) is
   begin
      if Debug then Put_Line("Or_Node_Class.Set_Nfa_Transitions starting at " & State_Id_Type'IMAGE(Start)); end if;
      if Self.Previous /= null then
         Self.Previous.Set_Nfa_Transitions(NFA, Start);
      end if;
      Self.A.Set_Nfa_Transitions(NFA, Start);
      Self.B.Set_Nfa_Transitions(NFA, Start);
      if Debug then Put_Line("result: " & NFA.Image); end if;
   end Set_Nfa_Transitions;





   -------------------------------------------------------------------------
   overriding procedure Process_This(Self : in out Star_Node_Class) is
   begin
      if Debug then Put_Line("Star_Node_Class.Process_This"); end if;
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
      if Debug then Put_Line("Star_Node_Class.Prepare_For_Graft"); end if;
      Box.Linear_Detach(Left);
      if Debug then Put_Line("Star_Node_Class.Prepare_For_Graft, detached and setting A with " & To_String(+Left.Image_Tree)); end if;
      Self.A := Left;
      if Debug then Put_Line("result: " & To_String(+Self.Image_Tree)); end if;
   end Prepare_For_Graft;

   -------------------------------------------------------------------------
   overriding function Count_Nfa_Transition_Sets(Self : Star_Node_Class) return Natural is
   begin
      return Self.A.Count_Nfa_Transition_Sets + (if Self.Previous = null then 0 else Self.Previous.Count_Nfa_Transition_Sets);
   end Count_Nfa_Transition_Sets;

   -------------------------------------------------------------------------
   overriding procedure Set_Nfa_Transitions
      (Self  : in out Star_Node_Class;
       NFA   : in out kv.apg.nfa.Nfa_Class;
       Start : in out State_Id_Type) is
       Start_Of_Star : State_Id_Type;
       Transition : Transition_Type;
   begin
      if Debug then Put_Line("Star_Node_Class.Set_Nfa_Transitions starting at " & State_Id_Type'IMAGE(Start)); end if;
      if Self.Previous /= null then
         Self.Previous.Set_Nfa_Transitions(NFA, Start);
      end if;
      Start_Of_Star := Start + 1;
      Self.A.Set_Nfa_Transitions(NFA, Start);
      if Debug then Put_Line("Star jump back to " & State_Id_Type'IMAGE(Start_Of_Star) & " from " & State_Id_Type'IMAGE(Start)); end if;
      --Set_Match(Transition, Start_Of_Star, );
      Transition := NFA.Get_Transition(Start_Of_Star - 1, 1);
      if Debug then Put_Line("Copied transition = " & Image(Transition)); end if;
      if Debug then Put_Line("Current NFA = " & NFA.Image); end if;
      --Set_Dest(Transition, Start_Of_Star);
      NFA.Append_State_Transition(Start, Transition);
   end Set_Nfa_Transitions;




   -------------------------------------------------------------------------
   overriding procedure Process_This(Self : in out Plus_Node_Class) is
   begin
      if Debug then Put_Line("Plus_Node_Pointer_Type.Process_This"); end if;
   end Process_This;

   -------------------------------------------------------------------------
   overriding function Image_This(Self : in out Plus_Node_Class) return String_Type is
   begin
      return To_String_Type("(") & Self.A.Image_This & To_String_Type(")") & To_String_Type("+");
   end Image_This;

   -------------------------------------------------------------------------
   overriding procedure Prepare_For_Graft
      (Self  : in out Plus_Node_Class;
       Box   : in out Reg_Ex_Container_Class'CLASS) is

      Left : Node_Pointer_Type;

   begin
      if Debug then Put_Line("Plus_Node_Pointer_Type.Prepare_For_Graft"); end if;
      Box.Linear_Detach(Left);
      if Debug then Put_Line("Plus_Node_Pointer_Type.Prepare_For_Graft, detached and setting A with " & To_String(+Left.Image_Tree)); end if;
      Self.A := Left;
      if Debug then Put_Line("result: " & To_String(+Self.Image_Tree)); end if;
   end Prepare_For_Graft;

   -------------------------------------------------------------------------
   overriding function Count_Nfa_Transition_Sets(Self : Plus_Node_Class) return Natural is
   begin
      return (Self.A.Count_Nfa_Transition_Sets * 2) + (if Self.Previous = null then 0 else Self.Previous.Count_Nfa_Transition_Sets);
   end Count_Nfa_Transition_Sets;

   -------------------------------------------------------------------------
   overriding procedure Set_Nfa_Transitions
      (Self  : in out Plus_Node_Class;
       NFA   : in out kv.apg.nfa.Nfa_Class;
       Start : in out State_Id_Type) is
   begin
      if Debug then Put_Line("Plus_Node_Class.Set_Nfa_Transitions starting at " & State_Id_Type'IMAGE(Start)); end if;
      if Self.Previous /= null then
         Self.Previous.Set_Nfa_Transitions(NFA, Start);
      end if;
   end Set_Nfa_Transitions;





   -------------------------------------------------------------------------
   overriding procedure Process_This(Self : in out Subsequence_Node_Class) is
   begin
      if Debug then Put_Line("Subsequence_Node_Class.Process_This"); end if;
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
      if Debug then Put_Line("Subsequence_Node_Class.Graft_To_Tree"); end if;
      if Self.A = null then
         if Debug then Put_Line("A empty, Self.Linear_Attach(Node) at this level"); end if;
         Self.Linear_Attach(Node);
      elsif Self.A.Is_Complete then
         if Debug then Put_Line("A is complete"); end if;
         if Node = null then
            if Debug then Put_Line("null complete!"); end if;
            Self.Complete := True; -- Must have been the non-token ')'
         else
            if Debug then Put_Line("Self.Linear_Attach(Node) at this level"); end if;
            Self.Linear_Attach(Node);
         end if;
      else
         if Debug then Put_Line("A is not complete, going down..."); end if;
         Self.A.Graft_To_Tree(Node);
      end if;
      if Debug then Put_Line("result: " & To_String(+Self.Image_Tree)); end if;
   end Graft_To_Tree;

   -------------------------------------------------------------------------
   overriding procedure Linear_Detach(Self : in out Subsequence_Node_Class; Node : out Node_Pointer_Type) is
   begin
      if Debug then Put_Line("Subsequence_Node_Class.Linear_Detach for " & To_String(+Self.Image_Tree)); end if;
      Node := Self.A;
      Self.A := Node.Previous;
      Node.Previous := null;
      if Debug then Put_Line("result: " & To_String(+Self.Image_Tree)); end if;
   end Linear_Detach;

   -------------------------------------------------------------------------
   overriding procedure Linear_Attach(Self : in out Subsequence_Node_Class; Node : in  Node_Pointer_Type) is
   begin
      if Debug then Put_Line("Subsequence_Node_Class.Linear_Attach for " & To_String(+Self.Image_Tree)); end if;
      Node.Prepare_For_Graft(Self);
      Node.Previous := Self.A;
      Self.A := Node;
      if Debug then Put_Line("result: " & To_String(+Self.Image_Tree)); end if;
   end Linear_Attach;

   -------------------------------------------------------------------------
   overriding function Count_Nfa_Transition_Sets(Self : Subsequence_Node_Class) return Natural is
   begin
      return Self.A.Count_Nfa_Transition_Sets + (if Self.Previous = null then 0 else Self.Previous.Count_Nfa_Transition_Sets);
   end Count_Nfa_Transition_Sets;

   -------------------------------------------------------------------------
   overriding procedure Set_Nfa_Transitions
      (Self  : in out Subsequence_Node_Class;
       NFA   : in out kv.apg.nfa.Nfa_Class;
       Start : in out State_Id_Type) is
   begin
      if Debug then Put_Line("Subsequence_Node_Class.Set_Nfa_Transitions starting at " & State_Id_Type'IMAGE(Start)); end if;
      if Self.Previous /= null then
         Self.Previous.Set_Nfa_Transitions(NFA, Start);
      end if;
      Self.A.Set_Nfa_Transitions(NFA, Start);
   end Set_Nfa_Transitions;

end kv.apg.regex;
