with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.fast; use kv.apg.fast;

package kv.apg.dfa is

   type Dfa_Class is tagged private;
   type Dfa_Pointer_Type is access all Dfa_Class;

   procedure Initialize
      (Self     : in out Dfa_Class;
       Existing : in     State_List_Pointer_Type);

--   function Image(Self : Dfa_Class) return String;
   function Get_Start_State(Self : Dfa_Class) return State_Id_Type;
   function Is_Accepting(Self : Dfa_Class; State : State_Id_Type) return Boolean;
   function Is_Terminal(Self : Dfa_Class; State : State_Id_Type) return Boolean;
--   function Transition_Count(Self : Dfa_Class; State : State_Id_Type) return Natural;
--   function Get_Transition(Self : Dfa_Class; State : State_Id_Type; Index : Positive) return Transition_Type;
   function Get_Key(Self : Dfa_Class; State : State_Id_Type) return Key_Type;

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

   type Dfa_Class is tagged
      record
         Start   : State_Id_Type := State_Id_Type'FIRST;
         States  : State_List_Pointer_Type;
      end record;

   type Dfa_State_Class is tagged
      record
         Dfa   : Dfa_Pointer_Type;
         State : State_Universe_Type;
         Moves : Natural := 0;
      end record;

end kv.apg.dfa;
