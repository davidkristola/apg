
with Interfaces;

package body kv.apg.fa is

   ----------------------------------------------------------------------------
   function Img(Char : Wide_Wide_Character) return String is
      Decimal_Image : constant String := Interfaces.Unsigned_32'IMAGE(Interfaces.Unsigned_32(Wide_Wide_Character'POS(Char)));
   begin
      return Decimal_Image(2..Decimal_Image'LAST);
   end Img;

   ----------------------------------------------------------------------------
   function Get_Start_State(Self : Finite_Automata_Class) return State_Id_Type is
   begin
      return Self.Start;
   end Get_Start_State;

   ----------------------------------------------------------------------------
   function Is_Accepting(Self : Finite_Automata_Class; State : State_Id_Type) return Boolean is
   begin
      return Is_Accepting(Self.States(State));
   end Is_Accepting;

   ----------------------------------------------------------------------------
   function Is_Terminal(Self : Finite_Automata_Class; State : State_Id_Type) return Boolean is
   begin
      return Get_Transition_Count(Self.States(State)) = 0; -- Has no exit transitions
   end Is_Terminal;

   ----------------------------------------------------------------------------
   function Get_Key(Self : Finite_Automata_Class; State : State_Id_Type) return Key_Type is
   begin
      return Get_Key(Self.States(State));
   end Get_Key;


end kv.apg.fa;
