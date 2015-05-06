
package body kv.apg.fast is

   ----------------------------------------------------------------------------
   procedure Set_Any(Self : in out Transition_Type; Dest : in State_Id_Type) is
   begin
      Self := (Criteria => Any, To_State => Dest);
   end Set_Any;

   ----------------------------------------------------------------------------
   procedure Set_Match(Self : in out Transition_Type; Dest : in State_Id_Type; Value : in Wide_Wide_Character) is
   begin
      Self := (Criteria => Match, To_State => Dest, Value => Value);
   end Set_Match;

   ----------------------------------------------------------------------------
   procedure Set_Range(Self : in out Transition_Type; Dest : in State_Id_Type; Lower : in Wide_Wide_Character; Upper : in Wide_Wide_Character) is
   begin
      Self := (Criteria => From_To, To_State => Dest, Lower => Lower, Upper => Upper);
   end Set_Range;

   ----------------------------------------------------------------------------
   function Go_To(Self : in Transition_Type; Check : in Wide_Wide_Character) return State_Universe_Type is
   begin
      case Self.Criteria is
         when Any =>
            return Self.To_State;
         when Match =>
            if Check = Self.Value then
               return Self.To_State;
            else
               return Invalid_State;
            end if;
         when From_To =>
            if Check in Self.Lower .. Self.Upper then
               return Self.To_State;
            else
               return Invalid_State;
            end if;
      end case;
   end Go_To;




   ----------------------------------------------------------------------------
   procedure Allocate_Transition_Array
      (Self  : in out State_Type;
       Alloc : in     Natural) is
   begin
      if Alloc /= 0 then
         Self.Transitions := new Transition_List_Type(1..Alloc);
      end if;
   end Allocate_Transition_Array;

   ----------------------------------------------------------------------------
   procedure Set_Accepting
      (Self  : in out State_Type;
       Id    : in     State_Id_Type;
       Key   : in     Key_Type;
       Alloc : in     Natural := 0) is
   begin
      Self.Id := Id;
      Self.Accepting := True;
      Self.Accepted_Key := Key;
      Allocate_Transition_Array(Self, Alloc);
   end ;

   ----------------------------------------------------------------------------
   procedure Set_Non_Accepting
      (Self  : in out State_Type;
       Id    : in     State_Id_Type;
       Alloc : in     Natural) is
   begin
      Self.Id := Id;
      Self.Accepting := False;
      Allocate_Transition_Array(Self, Alloc);
   end Set_Non_Accepting;

   ----------------------------------------------------------------------------
   procedure Set_Transition
      (Self  : in out State_Type;
       Index : in     Positive;
       Trans : in     Transition_Type) is
   begin
      -- Fail fast
      Self.Transitions(Index) := Trans;
   end Set_Transition;

   ----------------------------------------------------------------------------
   function Get_Id(Self : State_Type) return State_Universe_Type is
   begin
      return Self.Id;
   end Get_Id;

   ----------------------------------------------------------------------------
   function Is_Accepting(Self : State_Type) return Boolean is
   begin
      return Self.Accepting;
   end Is_Accepting;

   ----------------------------------------------------------------------------
   function Get_Key(Self : State_Type) return Key_Type is
   begin
      return Self.Accepted_Key;
   end Get_Key;

   ----------------------------------------------------------------------------
   function Get_Transition_Count(Self : State_Type) return Natural is
   begin
      if Self.Transitions = null then
         return 0;
      else
         return Self.Transitions'LENGTH;
      end if;
   end Get_Transition_Count;

   ----------------------------------------------------------------------------
   function Get_Transition(Self : State_Type; Index : Positive) return Transition_Type is
   begin
      return Self.Transitions(Index);
   end Get_Transition;

   ----------------------------------------------------------------------------
   procedure Mark_Transitions
      (Self  : in     State_Type;
       Value : in     Wide_Wide_Character;
       Next  : in     Active_State_List_Pointer_Type;
       Count :    out Natural) is
      Destination : State_Universe_Type;
   begin
      Count := 0;
      for I in Self.Transitions'RANGE loop
         Destination := Go_To(Self.Transitions(I), Value);
         if Destination /= Invalid_State then
            Count := Count + 1;
            Next(Destination) := True;
         end if;
      end loop;
   end Mark_Transitions;

end kv.apg.fast;

