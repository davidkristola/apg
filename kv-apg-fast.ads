
with kv.core.wwstr; use kv.core.wwstr;

-- finite automata states and transitions
package kv.apg.fast is

   type State_Universe_Type is new Integer range -1 .. Integer'LAST;
   subtype State_Id_Type is State_Universe_Type range 0 .. State_Universe_Type'LAST;
   Invalid_State : constant State_Universe_Type := -1;

   type State_Id_List_Type is array (Natural range <>) of State_Id_Type;
   type State_Id_List_Pointer_Type is access State_Id_List_Type;


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
   type Transition_List_Type is array (Natural range <>) of Transition_Type;
   type Transition_List_Pointer_Type is access Transition_List_Type;

   procedure Set_Any(Self : in out Transition_Type; Dest : in State_Id_Type);
   procedure Set_Match(Self : in out Transition_Type; Dest : in State_Id_Type; Value : in Wide_Wide_Character);
   procedure Set_Range(Self : in out Transition_Type; Dest : in State_Id_Type; Lower : in Wide_Wide_Character; Upper : in Wide_Wide_Character);

   function Go_To(Self : in Transition_Type; Check : in Wide_Wide_Character) return State_Universe_Type;

   type State_Class is
      record
         Id          : State_Universe_Type := Invalid_State;
         Accepting   : Boolean := False;
         Payload     : Integer := 0;
         Transitions : Transition_List_Pointer_Type;
      end record;

   type State_List_Type is array (State_Id_Type range <>) of State_Class;
   type State_List_Pointer_Type is access State_List_Type;

end kv.apg.fast;

