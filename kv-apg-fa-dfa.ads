with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.fast; use kv.apg.fast;

package kv.apg.fa.dfa is

   type Dfa_Class is new Finite_Automata_Class with null record;
   type Dfa_Pointer_Type is access all Dfa_Class;

   procedure Initialize
      (Self     : in out Dfa_Class;
       Existing : in     State_List_Pointer_Type);

   function Compute_Transitions
      (Self  : in     Dfa_Class;
       State : in     State_Id_Type;
       Value : in     Wide_Wide_Character) return State_Universe_Type;


   type Dfa_State_Class is tagged private;
   type Dfa_State_Pointer_Type is access all Dfa_State_Class;

   procedure Initialize
      (Self : in out Dfa_State_Class;
       Dfa  : in     Dfa_Pointer_Type);

   procedure Reset
      (Self : in out Dfa_State_Class);

   procedure Ingest
      (Self  : in out Dfa_State_Class;
       Input : in     Wide_Wide_Character);

   function Is_Accepting(Self : Dfa_State_Class) return Boolean;
   function Is_Terminal(Self : Dfa_State_Class) return Boolean;
   function Is_Failed(Self : Dfa_State_Class) return Boolean;
   function Move_Count(Self : Dfa_State_Class) return Natural;
   function Get_Key(Self : Dfa_State_Class) return Key_Type;

   -- Control debug output of package
   procedure Set_Debug(Value : in Boolean);


private

   type Dfa_State_Class is tagged
      record
         Dfa   : Dfa_Pointer_Type;
         State : State_Universe_Type;
         Moves : Natural := 0;
      end record;

end kv.apg.fa.dfa;
