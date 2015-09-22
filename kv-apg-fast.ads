
with kv.core.wwstr; use kv.core.wwstr;

-- finite automata states and transitions
package kv.apg.fast is

   type State_Universe_Type is new Natural;
   subtype State_Id_Type is State_Universe_Type range 1 .. State_Universe_Type'LAST;
   Invalid_State : constant State_Universe_Type := 0;

   type State_Id_List_Type is array (State_Id_Type range <>) of State_Universe_Type;
   type State_Id_List_Pointer_Type is access State_Id_List_Type;


   type Active_State_List_Type is array (State_Id_Type range <>) of Boolean;
   type Active_State_List_Pointer_Type is access all Active_State_List_Type;


   type Acceptance_Type is (Epsilon, Match, Any, From_To);

   type Transition_Type(Criteria : Acceptance_Type := Any) is
      record
         To_State : State_Universe_Type := Invalid_State;
         case Criteria is
            when Match =>
               Value : Wide_Wide_Character;
            when From_To =>
               Lower : Wide_Wide_Character;
               Upper : Wide_Wide_Character;
            when Epsilon | Any =>
               null;
         end case;
      end record;
   type Transition_List_Type is array (Positive range <>) of Transition_Type;
   type Transition_List_Pointer_Type is access all Transition_List_Type;

   procedure Set_Any(Self : in out Transition_Type; Dest : in State_Id_Type);
   procedure Set_Match(Self : in out Transition_Type; Dest : in State_Id_Type; Value : in Wide_Wide_Character);
   procedure Set_Range(Self : in out Transition_Type; Dest : in State_Id_Type; Lower : in Wide_Wide_Character; Upper : in Wide_Wide_Character);
   procedure Set_Epsilon(Self : in out Transition_Type; Dest : in State_Id_Type);
   procedure Set_Dest(Self : in out Transition_Type; Dest : in State_Id_Type);
   procedure Renumber(Self : in out Transition_Type; Offset : in     Integer);
   function Get_Dest(Self : Transition_Type) return State_Universe_Type;
   function Image(Self : Transition_Type) return String;
   function Source_Code(Self : Transition_Type) return String;

   function Move(Self : in Transition_Type; Check : in Wide_Wide_Character) return State_Universe_Type;
   function Epsilon_Move(Self : in Transition_Type) return State_Universe_Type;



   type Key_Type is new Integer;

   type State_Type is
      record
         Id           : State_Universe_Type := Invalid_State;
         Accepting    : Boolean := False;
         Accepted_Key : Key_Type := 0;
         Transitions  : Transition_List_Pointer_Type;
      end record;

   procedure Set_Accepting
      (Self  : in out State_Type;
       Id    : in     State_Id_Type;
       Key   : in     Key_Type;
       Alloc : in     Natural := 0);
   procedure Set_Non_Accepting
      (Self  : in out State_Type;
       Id    : in     State_Id_Type;
       Alloc : in     Natural);
   procedure Set_Transition
      (Self  : in out State_Type;
       Index : in     Positive;
       Trans : in     Transition_Type);
   procedure Append_Transition
      (Self  : in out State_Type;
       Trans : in     Transition_Type);
   procedure Renumber(Self : in out State_Type; Offset : in     Integer);
   procedure Init_By_Deep_Copy
      (Self  : in out State_Type;
       Other : in     State_Type);


   function Get_Id(Self : State_Type) return State_Universe_Type;
   function Is_Accepting(Self : State_Type) return Boolean;
   function Get_Key(Self : State_Type) return Key_Type;
   function Get_Transition_Count(Self : State_Type) return Natural;
   function Get_Transition(Self : State_Type; Index : Positive) return Transition_Type;
   function Image(Self : State_Type) return String;

   function Compute_Transitions
      (Self  : in     State_Type;
       Value : in     Wide_Wide_Character) return State_Universe_Type;
   procedure Mark_Transitions
      (Self  : in     State_Type;
       Value : in     Wide_Wide_Character;
       Next  : in     Active_State_List_Pointer_Type;
       Count :    out Natural);
   procedure Mark_Epsilon_Transitions
      (Self  : in     State_Type;
       Next  : in     Active_State_List_Pointer_Type);

   type State_List_Type is array (State_Id_Type range <>) of State_Type;
   type State_List_Pointer_Type is access all State_List_Type;

   -- Useful helper functions
   function Img(ID : State_Universe_Type) return String;
   function Img(Char : Wide_Wide_Character) return String;
   function Img(Key : Key_Type) return String;
   function Code_Point(Char : Wide_Wide_Character) return String;

   -- Control debug output of package
   procedure Set_Debug(Value : in Boolean);

end kv.apg.fast;

