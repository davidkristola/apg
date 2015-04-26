with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Wide_Wide_Unbounded;

with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;


package body kv.apg.regex is

   use Ada.Strings.Wide_Wide_Unbounded; -- &
   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Characters.Conversions;

   Quotation : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Quotation);

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
   overriding function Is_Complete(Self : Match_Node_Class) return Boolean is
   begin
      return True; -- Match nodes are always complete
   end Is_Complete;


   -------------------------------------------------------------------------
   procedure Complete_With(Self : in out Node_Class; Node : in Node_Pointer_Type) is
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
   procedure Detach
      (Tree : in out Node_Pointer_Type;
       Last :    out Node_Pointer_Type) is
   begin
      Last := Tree;
      Tree := Tree.Previous;
      Last.Previous := null;
   end Detach;


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
      return Tree.Root.Image_Tree;
   end Image_Tree;

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
   overriding function Is_Complete(Self : Match_Any_Node_Class) return Boolean is
   begin
      return True; -- Match Any nodes are always complete
   end Is_Complete;

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
      return (Self.A /= null) and (Self.B /= null); -- Both A and B must be set
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
   overriding procedure Complete_With(Self : in out Or_Node_Class; Node : in Node_Pointer_Type) is
   begin
      Self.B := Node;
   end Complete_With;

   -------------------------------------------------------------------------
   overriding function Image_This(Self : in out Or_Node_Class) return String_Type is
   begin
--      return To_String_Type("|");
      return Self.A.Image_This & To_String_Type("|") & Self.B.Image_This;
   end Image_This;

end kv.apg.regex;
