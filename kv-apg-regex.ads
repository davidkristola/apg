
with kv.core.wwstr; use kv.core.wwstr;

package kv.apg.regex is

   type Node_Class;
   type Node_Pointer_Type is access all Node_Class'CLASS;
   type Node_Class is abstract tagged
      record
         Left  : Node_Pointer_Type;
         Right : Node_Pointer_Type;
      end record;

   procedure Process_This(Self : in out Node_Class) is abstract;
   procedure Process_Tree(Self : in out Node_Class); -- left, self, right
   procedure Set_Left(Self : in out Node_Class; Node : in Node_Pointer_Type);
   procedure Set_Right(Self : in out Node_Class; Node : in Node_Pointer_Type);

   function Image_This(Self : in out Node_Class) return String_Type is abstract;
   function Image_Tree(Self : in out Node_Class) return String_Type; -- left & self & right

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


   type Or_Node_Class is new Node_Class with null record;
   not overriding procedure Initialize(Self : in out Or_Node_Class);
   overriding procedure Process_This(Self : in out Or_Node_Class);
   overriding function Image_This(Self : in out Or_Node_Class) return String_Type;


end kv.apg.regex;
