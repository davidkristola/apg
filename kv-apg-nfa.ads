
with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.fast; use kv.apg.fast;

package kv.apg.nfa is

   type Nfa_Class is tagged private;
   type Nfa_Pointer_Type is access all Nfa_Class;
   type Nfa_Array_Type is array (Positive range <>) of aliased Nfa_Class;

   procedure Initialize
      (Self   : in out Nfa_Class;
       Alloc  : in     Positive;
       Preset : in     Boolean := False;
       Key    : in     Key_Type := 0);

   procedure Initialize
      (Self     : in out Nfa_Class;
       Existing : in     State_List_Pointer_Type);

   procedure Initialize
      (Self    : in out Nfa_Class;
       Combine : in     Nfa_Array_Type);

   function Get_State_Count
      (Self : in     Nfa_Class) return Natural;

   procedure Set_State_Accepting
      (Self  : in out Nfa_Class;
       State : in     State_Id_Type;
       Key   : in     Key_Type;
       Alloc : in     Natural := 0);
   procedure Set_State_Non_Accepting
      (Self  : in out Nfa_Class;
       State : in     State_Id_Type;
       Alloc : in     Natural);
   procedure Set_State_Transition
      (Self  : in out Nfa_Class;
       State : in     State_Id_Type;
       Index : in     Positive;
       Trans : in     Transition_Type);
   procedure Append_State_Transition
      (Self  : in out Nfa_Class;
       State : in     State_Id_Type;
       Trans : in     Transition_Type);

   function Image(Self : Nfa_Class) return String;
   function Get_Start_State(Self : Nfa_Class) return State_Id_Type;
   function Is_Accepting(Self : Nfa_Class; State : State_Id_Type) return Boolean;
   function Is_Terminal(Self : Nfa_Class; State : State_Id_Type) return Boolean;
   function Transition_Count(Self : Nfa_Class; State : State_Id_Type) return Natural;
   function Get_Transition(Self : Nfa_Class; State : State_Id_Type; Index : Positive) return Transition_Type;
   function Get_Key(Self : Nfa_Class; State : State_Id_Type) return Key_Type;

   procedure Mark_Transitions
      (Self  : in     Nfa_Class;
       These : in     Active_State_List_Pointer_Type;
       Next  : in     Active_State_List_Pointer_Type;
       Value : in     Wide_Wide_Character);

   procedure Epsilon_Transitions
      (Self  : in     Nfa_Class;
       Next  : in     Active_State_List_Pointer_Type);



   type Nfa_State_Class is tagged private;
   type Nfa_State_Pointer_Type is access all Nfa_State_Class;

   procedure Initialize
      (Self : in out Nfa_State_Class;
       Nfa  : in     Nfa_Pointer_Type);

   procedure Reset
      (Self : in out Nfa_State_Class);

   procedure Ingest
      (Self  : in out Nfa_State_Class;
       Input : in     Wide_Wide_Character);

   function Is_Accepting(Self : Nfa_State_Class) return Boolean;
   function Is_Terminal(Self : Nfa_State_Class) return Boolean;
   function Is_Failed(Self : Nfa_State_Class) return Boolean;
   function Active_State_Count(Self : Nfa_State_Class) return Natural;
   function Move_Count(Self : Nfa_State_Class) return Natural;
   function Get_Key(Self : Nfa_State_Class) return Key_Type;

   -- Control debug output of package
   procedure Set_Debug(Value : in Boolean);

private

   type Nfa_Class is tagged
      record
         Start   : State_Id_Type := State_Id_Type'FIRST;
         States  : State_List_Pointer_Type;
      end record;

   type Nfa_State_Class is tagged
      record
         Nfa      : Nfa_Pointer_Type;
         Previous : Active_State_List_Pointer_Type;
         Next     : Active_State_List_Pointer_Type;
         Moves    : Natural := 0;
      end record;

   function Active_Count
      (List : in     Active_State_List_Pointer_Type) return Natural;

end kv.apg.nfa;
