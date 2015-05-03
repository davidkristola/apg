
package body kv.apg.nfa is

   procedure Initialize
      (Self  : in out Nfa_Class;
       Alloc : in     Positive := 20) is
   begin
      Self.States := new State_List_Type(0..State_Id_Type(Alloc-1));
   end Initialize;

   function Get_State_Count
      (Self : in     Nfa_Class) return Natural is
   begin
      return Self.Count;
   end Get_State_Count;

   procedure Initialize
      (Self : in out Nfa_State_Class;
       Nfa  : in     Nfa_Pointer_Type) is
   begin
      Self.Nfa := Nfa;
      Self.Active := 0;
      Self.Cursors := new State_Id_List_Type(0..Nfa.Get_State_Count);
   end Initialize;


end kv.apg.nfa;
