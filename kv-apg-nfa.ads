
with kv.core.wwstr; use kv.core.wwstr;

package kv.apg.nfa is

   type Acceptance_Type is (Match, Any, From_To);

   type State_Id_Type is new Natural;

   type Nfa_Class is tagged private;
   type Nfa_Pointer_Type is access all Nfa_Class;

   procedure Initialize
      (Self  : in out Nfa_Class;
       Alloc : in     Positive := 20);

   function Get_State_Count
      (Self : in     Nfa_Class) return Natural;


   type Nfa_State_Class is tagged private;
   type Nfa_State_Pointer_Type is access all Nfa_State_Class;

   procedure Initialize
      (Self : in out Nfa_State_Class;
       Nfa  : in     Nfa_Pointer_Type);



private


   type State_Id_List_Type is array (Natural range <>) of State_Id_Type;
   type State_Id_List_Pointer_Type is access State_Id_List_Type;

   type Transition_Type(Criteria : Acceptance_Type := Any) is
      record
         To_State : State_Id_Type;
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

   type State_Class is
      record
         Id          : State_Id_Type;
         Accepting   : Boolean;
         Payload     : Integer;
         Transitions : Transition_List_Pointer_Type;
      end record;

   type State_List_Type is array (State_Id_Type range <>) of State_Class;
   type State_List_Pointer_Type is access State_List_Type;


   type Nfa_Class is tagged
      record
         Start   : State_Id_Type := 0;
         States  : State_List_Pointer_Type;
         Count   : Natural := 0;
      end record;

   type Nfa_State_Class is tagged
      record
         Nfa     : Nfa_Pointer_Type;
         Active  : Natural := 0; -- Count of active cursors
         Cursors : State_Id_List_Pointer_Type;
      end record;

end kv.apg.nfa;
