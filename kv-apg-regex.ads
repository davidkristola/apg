
with kv.core.wwstr; use kv.core.wwstr;

package kv.apg.regex is

   type Node_Class;
   type Node_Pointer_Type is access all Node_Class'CLASS;
   type Node_Class is abstract tagged
      record
         Previous : Node_Pointer_Type;
      end record;

   procedure Process_This(Self : in out Node_Class) is abstract;
   procedure Process_Tree(Self : in out Node_Class); -- left, self, right
   procedure Set_Previous(Self : in out Node_Class; Node : in Node_Pointer_Type);

   function Image_This(Self : in out Node_Class) return String_Type is abstract;
   function Image_Tree(Self : in out Node_Class) return String_Type; -- left & self & right


   -- Detach the last node from the tree.
   procedure Detach
      (Tree : in out Node_Pointer_Type;
       Last :    out Node_Pointer_Type);



   type Match_Node_Class is new Node_Class with
      record
         Value : String_Type;
      end record;
   not overriding procedure Initialize(Self : in out Match_Node_Class; Value : in     String_Type);
   overriding procedure Process_This(Self : in out Match_Node_Class);
   overriding function Image_This(Self : in out Match_Node_Class) return String_Type;


   type Match_Any_Node_Class is new Node_Class with null record;
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
   overriding function Image_This(Self : in out Or_Node_Class) return String_Type;


end kv.apg.regex;
