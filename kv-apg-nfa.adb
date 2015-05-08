
package body kv.apg.nfa is

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self  : in out Nfa_Class;
       Alloc : in     Positive) is
   begin
      Self.States := new State_List_Type(1..State_Id_Type(Alloc));
   end Initialize;

   ----------------------------------------------------------------------------
   function Get_State_Count
      (Self : in     Nfa_Class) return Natural is
   begin
      return Self.States'LENGTH;
   end Get_State_Count;

   ----------------------------------------------------------------------------
   procedure Set_State_Accepting
      (Self  : in out Nfa_Class;
       State : in     State_Id_Type;
       Key   : in     Key_Type;
       Alloc : in     Natural := 0) is
   begin
      Set_Accepting(Self.States(State), State, Key, Alloc);
   end Set_State_Accepting;

   ----------------------------------------------------------------------------
   procedure Set_State_Non_Accepting
      (Self  : in out Nfa_Class;
       State : in     State_Id_Type;
       Alloc : in     Natural) is
   begin
      Set_Non_Accepting(Self.States(State), State, Alloc);
   end Set_State_Non_Accepting;

   ----------------------------------------------------------------------------
   procedure Set_State_Transition
      (Self  : in out Nfa_Class;
       State : in     State_Id_Type;
       Index : in     Positive;
       Trans : in     Transition_Type) is
   begin
      Set_Transition(Self.States(State), Index, Trans);
   end Set_State_Transition;

   ----------------------------------------------------------------------------
   function Recursive_Image(Self : Nfa_Class; Index : Natural) return String is
   begin
      if Index = 1 then
         return Image(Self.States(1));
      else
         return Recursive_Image(Self, Index-1) & "/" & Image(Self.States(State_Id_Type(Index)));
      end if;
   end Recursive_Image;

   ----------------------------------------------------------------------------
   function Image(Self : Nfa_Class) return String is
   begin
      return "[" & Recursive_Image(Self, Self.States'LENGTH) & "]";
   end Image;


   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Nfa_State_Class;
       Nfa  : in     Nfa_Pointer_Type) is
   begin
      Self.Nfa := Nfa;
      Self.Active := 0;
      Self.Cursors := new State_Id_List_Type(1..State_Id_Type(Nfa.Get_State_Count));
   end Initialize;

   ----------------------------------------------------------------------------
   function Is_Accepting(Self : Nfa_State_Class) return Boolean is
   begin
   return False;
   end;

   ----------------------------------------------------------------------------
   function Is_Terminal(Self : Nfa_State_Class) return Boolean is
   begin
   return False;
   end;

   ----------------------------------------------------------------------------
   function Is_Failed(Self : Nfa_State_Class) return Boolean is
   begin
   return False;
   end;

   ----------------------------------------------------------------------------
   function Active_State_Count(Self : Nfa_State_Class) return Natural is
   begin
   return 0;
   end;

   ----------------------------------------------------------------------------
   function Move_Count(Self : Nfa_State_Class) return Natural is
   begin
   return 0;
   end;


end kv.apg.nfa;
