with Ada.Finalization;

with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.tokens;

package kv.apg.regex is

   Inappropriate_Action_Error : exception;

   type Regular_Expression_Tree_Type is new Ada.Finalization.Controlled with private;

   type Node_Class;
   type Node_Pointer_Type is access all Node_Class'CLASS;
   type Node_Class is abstract tagged
      record
         Previous : Node_Pointer_Type;
      end record;

   procedure Process_This(Self : in out Node_Class) is abstract;
   procedure Process_Tree(Self : in out Node_Class); -- Template method
   procedure Set_Previous(Self : in out Node_Class; Node : in Node_Pointer_Type);
   function Is_Complete(Self : Node_Class) return Boolean; -- Default behavior
   function Get_Incomplete(Self : in out Node_Class) return Node_Pointer_Type; -- Default behavior
   procedure Complete_With
      (Self  : in out Node_Class;
       Token : in     kv.apg.tokens.Token_Class); -- Default behavior
   function Image_This(Self : in out Node_Class) return String_Type is abstract;
   function Image_Tree(Self : in out Node_Class) return String_Type; -- Template method


   -- Detach the last node from the tree.
   procedure Detach
      (Tree : in out Node_Pointer_Type;
       Last :    out Node_Pointer_Type);





   -- Detach the last node from the tree.
   procedure Detach
      (Tree : in out Regular_Expression_Tree_Type;
       Last :    out Node_Pointer_Type);

   procedure Append
      (Tree : in out Regular_Expression_Tree_Type;
       Last : in     Node_Pointer_Type);

   function Get_Root
      (Tree : in     Regular_Expression_Tree_Type) return Node_Pointer_Type;

   function Is_Empty
      (Tree : in     Regular_Expression_Tree_Type) return Boolean;

   function Is_Complete
      (Tree : Regular_Expression_Tree_Type) return Boolean;

   function Get_Incomplete
      (Tree : in     Regular_Expression_Tree_Type) return Node_Pointer_Type;

   function Image_Tree(Tree : in     Regular_Expression_Tree_Type) return String_Type;

   --TODO: make this a Regular_Expression_Tree_Type method
   function Allocate_Node
      (Token : in     kv.apg.tokens.Token_Class) return Node_Pointer_Type;





   type Match_Node_Class is new Node_Class with
      record
         Value : String_Type;
      end record;
   type Match_Node_Pointer_Type is access all Match_Node_Class;
   not overriding procedure Initialize(Self : in out Match_Node_Class; Value : in     String_Type);
   overriding procedure Process_This(Self : in out Match_Node_Class);
   overriding function Image_This(Self : in out Match_Node_Class) return String_Type;


   type Match_Any_Node_Class is new Node_Class with null record;
   type Match_Any_Node_Pointer_Type is access all Match_Any_Node_Class;
   not overriding procedure Initialize(Self : in out Match_Any_Node_Class);
   overriding procedure Process_This(Self : in out Match_Any_Node_Class);
   overriding function Image_This(Self : in out Match_Any_Node_Class) return String_Type;


   type Or_Node_Class is new Node_Class with
      record
         A : Node_Pointer_Type;
         B : Node_Pointer_Type;
      end record;
   type Or_Node_Pointer_Type is access all Or_Node_Class;
   not overriding procedure Initialize(Self : in out Or_Node_Class);
   not overriding procedure Set_A(Self : in out Or_Node_Class; A : in     Node_Pointer_Type);
   not overriding procedure Set_B(Self : in out Or_Node_Class; B : in     Node_Pointer_Type);
   overriding procedure Process_This(Self : in out Or_Node_Class);
   overriding function Is_Complete(Self : Or_Node_Class) return Boolean;
   overriding function Get_Incomplete(Self : in out Or_Node_Class) return Node_Pointer_Type;
   procedure Complete_With
      (Self  : in out Or_Node_Class;
       Token : in     kv.apg.tokens.Token_Class);
   overriding function Image_This(Self : in out Or_Node_Class) return String_Type;

   type Star_Node_Class is new Node_Class with
      record
         A : Node_Pointer_Type;
      end record;
   type Star_Node_Pointer_Type is access all Star_Node_Class;
   not overriding procedure Initialize(Self : in out Star_Node_Class);
   not overriding procedure Set_A(Self : in out Star_Node_Class; A : in     Node_Pointer_Type);
   overriding procedure Process_This(Self : in out Star_Node_Class);
   overriding function Image_This(Self : in out Star_Node_Class) return String_Type;

   type Subsequence_Node_Class is new Node_Class with
      record
         A : Node_Pointer_Type;
         Complete : Boolean := False;
      end record;
   type Subsequence_Node_Pointer_Type is access all Subsequence_Node_Class;
   not overriding procedure Initialize(Self : in out Subsequence_Node_Class);
   not overriding procedure Set_A(Self : in out Subsequence_Node_Class; A : in     Node_Pointer_Type);
   overriding procedure Process_This(Self : in out Subsequence_Node_Class);
   overriding function Is_Complete(Self : Subsequence_Node_Class) return Boolean;
   overriding function Get_Incomplete(Self : in out Subsequence_Node_Class) return Node_Pointer_Type;
   procedure Complete_With
      (Self  : in out Subsequence_Node_Class;
       Token : in     kv.apg.tokens.Token_Class);
   overriding function Image_This(Self : in out Subsequence_Node_Class) return String_Type;

private

   type Regular_Expression_Tree_Type is new Ada.Finalization.Controlled with
      record
         Root : Node_Pointer_Type;
      end record;

end kv.apg.regex;
