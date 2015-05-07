
with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.fast; use kv.apg.fast;

package kv.apg.nfa is

   type Nfa_Class is tagged private;
   type Nfa_Pointer_Type is access all Nfa_Class;

   procedure Initialize
      (Self  : in out Nfa_Class;
       Alloc : in     Positive);

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

   function Image(Self : Nfa_Class) return String;





   type Nfa_State_Class is tagged private;
   type Nfa_State_Pointer_Type is access all Nfa_State_Class;

   procedure Initialize
      (Self : in out Nfa_State_Class;
       Nfa  : in     Nfa_Pointer_Type);



private

   type Nfa_Class is tagged
      record
         Start   : State_Id_Type := State_Id_Type'FIRST;
         States  : State_List_Pointer_Type;
      end record;

   type Nfa_State_Class is tagged
      record
         Nfa     : Nfa_Pointer_Type;
         Active  : Natural := 0; -- Count of active cursors
         Cursors : State_Id_List_Pointer_Type; --!@#$ use a boolean array (or two: cur/nxt)
      end record;

end kv.apg.nfa;
