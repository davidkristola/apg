
with Interfaces;

package body kv.apg.fa is

   ----------------------------------------------------------------------------
   function Img(Char : Wide_Wide_Character) return String is
      Decimal_Image : constant String := Interfaces.Unsigned_32'IMAGE(Interfaces.Unsigned_32(Wide_Wide_Character'POS(Char)));
   begin
      return Decimal_Image(2..Decimal_Image'LAST);
   end Img;

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self     : in out Finite_Automata_Class;
       Existing : in     State_List_Pointer_Type) is
   begin
      Self.States := Existing;
   end Initialize;

   ----------------------------------------------------------------------------
   function Get_State_Count(Self : Finite_Automata_Class) return Natural is
   begin
      if Self.States = null then
         return 0;
      else
         return Self.States'LENGTH;
      end if;
   end Get_State_Count;

   ----------------------------------------------------------------------------
   function Get_State_List(Self : Finite_Automata_Class) return State_List_Pointer_Type is
   begin
      return Self.States;
   end Get_State_List;

   ----------------------------------------------------------------------------
   function Get_Start_State(Self : Finite_Automata_Class) return State_Id_Type is
   begin
      return Self.Start;
   end Get_Start_State;

   ----------------------------------------------------------------------------
   function Recursive_Image(Self : Finite_Automata_Class; Index : Natural) return String is
   begin
      if Index = 1 then
         return Image(Self.States(1));
      else
         return Recursive_Image(Self, Index-1) & "/" & Image(Self.States(State_Id_Type(Index)));
      end if;
   end Recursive_Image;

   ----------------------------------------------------------------------------
   function Image(Self : Finite_Automata_Class) return String is
   begin
      if Self.States = null then
         return "[null]";
      end if;
      return "[" & Recursive_Image(Self, Self.States'LENGTH) & "]";
   end Image;

   ----------------------------------------------------------------------------
   function Transition_Count(Self : Finite_Automata_Class; State : State_Id_Type) return Natural is
   begin
      return Get_Transition_Count(Self.States(State));
   end Transition_Count;

   ----------------------------------------------------------------------------
   function Get_Transition(Self : Finite_Automata_Class; State : State_Id_Type; Index : Positive) return Transition_Type is
   begin
      return Get_Transition(Self.States(State), Index);
   end Get_Transition;

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
