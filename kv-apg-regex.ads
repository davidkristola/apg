with Ada.Finalization;

with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.tokens;
with kv.apg.fa.nfa;
with kv.apg.fast; use kv.apg.fast;
with kv.apg.logger;

package kv.apg.regex is

   Inappropriate_Action_Error : exception;

   -- Abstract base class in the (private) regular expression tree
   type Node_Class;
   type Node_Pointer_Type is access all Node_Class'CLASS;

   -- (Private) interface for treating everything as a container
   -- Supply attach and detach routines
   type Reg_Ex_Container_Class is interface;
   procedure Linear_Detach(Self : in out Reg_Ex_Container_Class; Node : out Node_Pointer_Type) is abstract;
   procedure Linear_Attach(Self : in out Reg_Ex_Container_Class; Node : in  Node_Pointer_Type) is abstract;


   type Node_Class is abstract new Reg_Ex_Container_Class with private;
   overriding procedure Linear_Detach(Self : in out Node_Class; Node : out Node_Pointer_Type);
   overriding procedure Linear_Attach(Self : in out Node_Class; Node : in  Node_Pointer_Type);

   procedure Process_This(Self : in out Node_Class) is abstract;
   procedure Process_Tree(Self : in out Node_Class); -- Template method
   function Is_Complete(Self : Node_Class) return Boolean; -- Default behavior
   function Image_This(Self : in out Node_Class) return String_Type is abstract;
   function Image_Tree(Self : in out Node_Class) return String_Type; -- Template method
   procedure Prepare_For_Graft
      (Self  : in out Node_Class;
       Box   : in out Reg_Ex_Container_Class'CLASS); -- Default behavior
   procedure Graft_To_Tree
      (Self : in out Node_Class;
       Node : in     Node_Pointer_Type); -- Default behavior for nodes that are always complete
   function Count_Nfa_Transition_Sets(Self : Node_Class) return Natural is abstract;
   procedure Set_Nfa_Transitions
      (Self  : in out Node_Class;
       NFA   : in out kv.apg.fa.nfa.Nfa_Class;
       Start : in out State_Id_Type) is abstract;






   type Regular_Expression_Tree_Type is new Ada.Finalization.Controlled and Reg_Ex_Container_Class with private;

   overriding procedure Linear_Detach(Self : in out Regular_Expression_Tree_Type; Node : out Node_Pointer_Type);
   overriding procedure Linear_Attach(Self : in out Regular_Expression_Tree_Type; Node : in  Node_Pointer_Type);


   function Is_Empty
      (Tree : in     Regular_Expression_Tree_Type) return Boolean;

   function Is_Complete
      (Tree : Regular_Expression_Tree_Type) return Boolean;

   procedure Diagnose_To_Log
      (Tree   : in     Regular_Expression_Tree_Type;
       Logger : in out kv.apg.logger.Logger_Pointer);

   function Image_Tree(Tree : in     Regular_Expression_Tree_Type) return String_Type;

   procedure Graft_To_Tree
      (Tree  : in out Regular_Expression_Tree_Type;
       Token : in     kv.apg.tokens.Token_Class);


   function To_Nfa(Tree : Regular_Expression_Tree_Type; Key : Key_Type) return kv.apg.fa.nfa.Nfa_Class;


   -- Control debug output of package
   procedure Set_Debug(Value : in Boolean);

private

   type Node_Class is abstract new Reg_Ex_Container_Class with
      record
         Token    : kv.apg.tokens.Token_Class;
         Previous : Node_Pointer_Type;
      end record;

   procedure Set_Token
      (Self  : in out Node_Class;
       Token : in     kv.apg.tokens.Token_Class);

   type Regular_Expression_Tree_Type is new Ada.Finalization.Controlled and Reg_Ex_Container_Class with
      record
         Root : Node_Pointer_Type;
      end record;

   -- Now define the private children of Node_Class

   type Match_Node_Class is new Node_Class with
      record
         Value : String_Type;
      end record;
   type Match_Node_Pointer_Type is access all Match_Node_Class;
   not overriding procedure Initialize(Self : in out Match_Node_Class; Value : in     String_Type);
   overriding procedure Process_This(Self : in out Match_Node_Class);
   overriding function Image_This(Self : in out Match_Node_Class) return String_Type;
   overriding function Count_Nfa_Transition_Sets(Self : Match_Node_Class) return Natural;
   overriding procedure Set_Nfa_Transitions
      (Self  : in out Match_Node_Class;
       NFA   : in out kv.apg.fa.nfa.Nfa_Class;
       Start : in out State_Id_Type);


   type Match_Any_Node_Class is new Node_Class with null record;
   type Match_Any_Node_Pointer_Type is access all Match_Any_Node_Class;
   overriding procedure Process_This(Self : in out Match_Any_Node_Class);
   overriding function Image_This(Self : in out Match_Any_Node_Class) return String_Type;
   overriding function Count_Nfa_Transition_Sets(Self : Match_Any_Node_Class) return Natural;
   overriding procedure Set_Nfa_Transitions
      (Self  : in out Match_Any_Node_Class;
       NFA   : in out kv.apg.fa.nfa.Nfa_Class;
       Start : in out State_Id_Type);


   type Or_Node_Class is new Node_Class with
      record
         A : Node_Pointer_Type;
         B : Node_Pointer_Type;
      end record;
   type Or_Node_Pointer_Type is access all Or_Node_Class;
   overriding procedure Prepare_For_Graft
      (Self  : in out Or_Node_Class;
       Box   : in out Reg_Ex_Container_Class'CLASS);
   overriding procedure Process_This(Self : in out Or_Node_Class);
   overriding function Is_Complete(Self : Or_Node_Class) return Boolean;
   overriding function Image_This(Self : in out Or_Node_Class) return String_Type;
   overriding procedure Graft_To_Tree
      (Self : in out Or_Node_Class;
       Node : in     Node_Pointer_Type);
   overriding function Count_Nfa_Transition_Sets(Self : Or_Node_Class) return Natural;
   overriding procedure Set_Nfa_Transitions
      (Self  : in out Or_Node_Class;
       NFA   : in out kv.apg.fa.nfa.Nfa_Class;
       Start : in out State_Id_Type);



   type Star_Node_Class is new Node_Class with
      record
         A : Node_Pointer_Type;
      end record;
   type Star_Node_Pointer_Type is access all Star_Node_Class;
   overriding procedure Process_This(Self : in out Star_Node_Class);
   overriding function Image_This(Self : in out Star_Node_Class) return String_Type;
   overriding procedure Prepare_For_Graft
      (Self  : in out Star_Node_Class;
       Box   : in out Reg_Ex_Container_Class'CLASS);
   overriding function Count_Nfa_Transition_Sets(Self : Star_Node_Class) return Natural;
   overriding procedure Set_Nfa_Transitions
      (Self  : in out Star_Node_Class;
       NFA   : in out kv.apg.fa.nfa.Nfa_Class;
       Start : in out State_Id_Type);



   type Plus_Node_Class is new Node_Class with
      record
         A : Node_Pointer_Type;
      end record;
   type Plus_Node_Pointer_Type is access all Plus_Node_Class;
   overriding procedure Process_This(Self : in out Plus_Node_Class);
   overriding function Image_This(Self : in out Plus_Node_Class) return String_Type;
   overriding procedure Prepare_For_Graft
      (Self  : in out Plus_Node_Class;
       Box   : in out Reg_Ex_Container_Class'CLASS);
   overriding function Count_Nfa_Transition_Sets(Self : Plus_Node_Class) return Natural;
   overriding procedure Set_Nfa_Transitions
      (Self  : in out Plus_Node_Class;
       NFA   : in out kv.apg.fa.nfa.Nfa_Class;
       Start : in out State_Id_Type);



   type Subsequence_Node_Class is new Node_Class with
      record
         A : Node_Pointer_Type;
         Complete : Node_Pointer_Type; -- The End_Node_Class, or null (incomplete)
      end record;
   type Subsequence_Node_Pointer_Type is access all Subsequence_Node_Class;
   overriding procedure Process_This(Self : in out Subsequence_Node_Class);
   overriding function Is_Complete(Self : Subsequence_Node_Class) return Boolean;
   overriding function Image_This(Self : in out Subsequence_Node_Class) return String_Type;
   overriding procedure Graft_To_Tree
      (Self : in out Subsequence_Node_Class;
       Node : in     Node_Pointer_Type);
   overriding procedure Linear_Detach(Self : in out Subsequence_Node_Class; Node : out Node_Pointer_Type);
   overriding procedure Linear_Attach(Self : in out Subsequence_Node_Class; Node : in  Node_Pointer_Type);
   overriding function Count_Nfa_Transition_Sets(Self : Subsequence_Node_Class) return Natural;
   overriding procedure Set_Nfa_Transitions
      (Self  : in out Subsequence_Node_Class;
       NFA   : in out kv.apg.fa.nfa.Nfa_Class;
       Start : in out State_Id_Type);


   type End_Node_Class is new Node_Class with null record;
   type End_Node_Pointer_Type is access all End_Node_Class;
   overriding procedure Process_This(Self : in out End_Node_Class);
   overriding function Is_Complete(Self : End_Node_Class) return Boolean;
   overriding function Image_This(Self : in out End_Node_Class) return String_Type;
   overriding function Count_Nfa_Transition_Sets(Self : End_Node_Class) return Natural;
   overriding procedure Set_Nfa_Transitions
      (Self  : in out End_Node_Class;
       NFA   : in out kv.apg.fa.nfa.Nfa_Class;
       Start : in out State_Id_Type);


   -- Zero or one ("+")
   type Zoro_Node_Class is new Node_Class with
      record
         A : Node_Pointer_Type;
      end record;
   type Zoro_Node_Pointer_Type is access all Zoro_Node_Class;
   overriding procedure Process_This(Self : in out Zoro_Node_Class);
   overriding function Image_This(Self : in out Zoro_Node_Class) return String_Type;
   overriding procedure Prepare_For_Graft
      (Self  : in out Zoro_Node_Class;
       Box   : in out Reg_Ex_Container_Class'CLASS);
   overriding function Count_Nfa_Transition_Sets(Self : Zoro_Node_Class) return Natural;
   overriding procedure Set_Nfa_Transitions
      (Self  : in out Zoro_Node_Class;
       NFA   : in out kv.apg.fa.nfa.Nfa_Class;
       Start : in out State_Id_Type);


   type Range_Node_Class is new Node_Class with
      record
         Lower : Node_Pointer_Type; -- Wide_Wide_Character;
         Upper : Node_Pointer_Type; -- Wide_Wide_Character;
      end record;
   type Range_Node_Pointer_Type is access all Range_Node_Class;
   overriding procedure Prepare_For_Graft
      (Self  : in out Range_Node_Class;
       Box   : in out Reg_Ex_Container_Class'CLASS);
   overriding procedure Process_This(Self : in out Range_Node_Class);
   overriding function Is_Complete(Self : Range_Node_Class) return Boolean;
   overriding function Image_This(Self : in out Range_Node_Class) return String_Type;
   overriding procedure Graft_To_Tree
      (Self : in out Range_Node_Class;
       Node : in     Node_Pointer_Type);
   overriding function Count_Nfa_Transition_Sets(Self : Range_Node_Class) return Natural;
   overriding procedure Set_Nfa_Transitions
      (Self  : in out Range_Node_Class;
       NFA   : in out kv.apg.fa.nfa.Nfa_Class;
       Start : in out State_Id_Type);


end kv.apg.regex;
