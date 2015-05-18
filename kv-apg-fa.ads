
with kv.core.wwstr; use kv.core.wwstr;
with kv.apg.fast; use kv.apg.fast;

package kv.apg.fa is

   function Img(Char : Wide_Wide_Character) return String;

   type Finite_Automata_Class is abstract tagged private;

   procedure Initialize
      (Self     : in out Finite_Automata_Class;
       Existing : in     State_List_Pointer_Type);

   function Get_State_Count(Self : Finite_Automata_Class) return Natural;
   function Get_Start_State(Self : Finite_Automata_Class) return State_Id_Type;
   function Image(Self : Finite_Automata_Class) return String;
   function Is_Accepting(Self : Finite_Automata_Class; State : State_Id_Type) return Boolean;
   function Is_Terminal(Self : Finite_Automata_Class; State : State_Id_Type) return Boolean;
   function Transition_Count(Self : Finite_Automata_Class; State : State_Id_Type) return Natural;
   function Get_Transition(Self : Finite_Automata_Class; State : State_Id_Type; Index : Positive) return Transition_Type;
   function Get_Key(Self : Finite_Automata_Class; State : State_Id_Type) return Key_Type;



   type Finite_Automata_State_Class is interface;

   procedure Reset
      (Self : in out Finite_Automata_State_Class) is abstract;

   procedure Ingest
      (Self  : in out Finite_Automata_State_Class;
       Input : in     Wide_Wide_Character) is abstract;

   function Is_Accepting(Self : Finite_Automata_State_Class) return Boolean is abstract;
   function Is_Terminal(Self : Finite_Automata_State_Class) return Boolean is abstract;
   function Is_Failed(Self : Finite_Automata_State_Class) return Boolean is abstract;
   function Move_Count(Self : Finite_Automata_State_Class) return Natural is abstract;
   function Get_Key(Self : Finite_Automata_State_Class) return Key_Type is abstract;


private

   type Finite_Automata_Class is abstract tagged
      record
         Start  : State_Id_Type := State_Id_Type'FIRST;
         States : State_List_Pointer_Type;
      end record;

end kv.apg.fa;
