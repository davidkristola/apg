-- This file is machine generated. Do not edit.

with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.UTF_Encoding;


package «package_name».States is

   type State_Universe_Type is new Natural;
   subtype State_Id_Type is State_Universe_Type range 1 .. State_Universe_Type'LAST;
   Invalid_State : constant State_Universe_Type := 0;

   type State_Id_List_Type is array (State_Id_Type range <>) of State_Universe_Type;
   type State_Id_List_Pointer_Type is access State_Id_List_Type;

   type Active_State_List_Type is array (State_Id_Type range <>) of Boolean;
   type Active_State_List_Pointer_Type is access all Active_State_List_Type;


   type Acceptance_Type is (Match, Any, From_To);

   type Transition_Type(Criteria : Acceptance_Type := Any) is
      record
         To_State : State_Universe_Type := Invalid_State;
         case Criteria is
            when Match =>
               Value : Wide_Wide_Character;
            when From_To =>
               Lower : Wide_Wide_Character;
               Upper : Wide_Wide_Character;
            when Any =>
               null;
         end case;
      end record;
   type Transition_List_Type is array (Positive range <>) of Transition_Type;
   type Transition_List_Pointer_Type is access constant Transition_List_Type;


   function Move(Self : in Transition_Type; Check : in Wide_Wide_Character) return State_Universe_Type;


   type State_Type is
      record
         Accepted_Token : Token_Type := Invalid;
         Transitions    : Transition_List_Pointer_Type;
      end record;
   type State_List_Type is array (State_Id_Type range <>) of State_Type;
   type State_List_Pointer_Type is access constant State_List_Type;

   function Is_Accepting(Self : State_Type) return Boolean;
   function Get_Token(Self : State_Type) return Token_Type;
   function Get_Transition_Count(Self : State_Type) return Natural;
   function Get_Transition(Self : State_Type; Index : Positive) return Transition_Type;


   procedure Mark_Transitions
      (Self  : in     State_Type;
       Value : in     Wide_Wide_Character;
       Next  : in     Active_State_List_Pointer_Type;
       Count :    out Natural);


end «package_name».States;
