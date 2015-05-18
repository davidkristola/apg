
with kv.core.wwstr; use kv.core.wwstr;
with kv.apg.fast; use kv.apg.fast;

package kv.apg.fa is

   function Img(Char : Wide_Wide_Character) return String;

   type Finite_Automata_Class is abstract tagged private;

--   function Image(Self : Finite_Automata_Class) return String;
   function Get_Start_State(Self : Finite_Automata_Class) return State_Id_Type;
   function Is_Accepting(Self : Finite_Automata_Class; State : State_Id_Type) return Boolean;
   function Is_Terminal(Self : Finite_Automata_Class; State : State_Id_Type) return Boolean;
--   function Transition_Count(Self : Finite_Automata_Class; State : State_Id_Type) return Natural;
--   function Get_Transition(Self : Finite_Automata_Class; State : State_Id_Type; Index : Positive) return Transition_Type;
   function Get_Key(Self : Finite_Automata_Class; State : State_Id_Type) return Key_Type;

private

   type Finite_Automata_Class is abstract tagged
      record
         Start  : State_Id_Type := State_Id_Type'FIRST;
         States : State_List_Pointer_Type;
      end record;

end kv.apg.fa;
