
with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.fast; use kv.apg.fast;

package kv.apg.nfa is

   type Nfa_Class is tagged private;
   type Nfa_Pointer_Type is access all Nfa_Class;

   procedure Initialize
      (Self  : in out Nfa_Class;
       Alloc : in     Positive := 20);

   function Get_State_Count
      (Self : in     Nfa_Class) return Natural;


   type Nfa_State_Class is tagged private;
   type Nfa_State_Pointer_Type is access all Nfa_State_Class;

   procedure Initialize
      (Self : in out Nfa_State_Class;
       Nfa  : in     Nfa_Pointer_Type);



private



   type Nfa_Class is tagged
      record
         Start   : State_Id_Type := 0;
         States  : State_List_Pointer_Type;
         Count   : Natural := 0;
      end record;

   type Nfa_State_Class is tagged
      record
         Nfa     : Nfa_Pointer_Type;
         Active  : Natural := 0; -- Count of active cursors
         Cursors : State_Id_List_Pointer_Type; --!@#$ use a boolean array (or two: cur/nxt)
      end record;

end kv.apg.nfa;
