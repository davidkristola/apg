
package body kv.apg.fast is

   procedure Set_Any(Self : in out Transition_Type; Dest : in State_Id_Type) is
   begin
      Self := (Criteria => Any, To_State => Dest);
   end Set_Any;

   procedure Set_Match(Self : in out Transition_Type; Dest : in State_Id_Type; Value : in Wide_Wide_Character) is
   begin
      Self := (Criteria => Match, To_State => Dest, Value => Value);
   end Set_Match;

   procedure Set_Range(Self : in out Transition_Type; Dest : in State_Id_Type; Lower : in Wide_Wide_Character; Upper : in Wide_Wide_Character) is
   begin
      Self := (Criteria => From_To, To_State => Dest, Lower => Lower, Upper => Upper);
   end Set_Range;

   function Go_To(Self : in Transition_Type; Check : in Wide_Wide_Character) return State_Universe_Type is
   begin
      if Self.Criteria = Any then
         return Self.To_State;
      end if;
      return Invalid_State;
   end Go_To;


end kv.apg.fast;

