with Ada.Finalization;
with Ada.Containers.Vectors;

with kv.core.wwstr; use kv.core.wwstr;

with kv.apg.fast; use kv.apg.fast;

package kv.apg.fa.nfa is

   type Nfa_Class is new Finite_Automata_Class with null record;
   type Nfa_Pointer_Type is access all Nfa_Class;
   type Nfa_Array_Type is array (Positive range <>) of aliased Nfa_Class;

   not overriding procedure Initialize
      (Self   : in out Nfa_Class;
       Alloc  : in     Positive;
       Preset : in     Boolean := False;
       Key    : in     Key_Type := 0);

   not overriding procedure Initialize
      (Self    : in out Nfa_Class;
       Combine : in     Nfa_Array_Type);

   not overriding procedure Set_State_Accepting
      (Self  : in out Nfa_Class;
       State : in     State_Id_Type;
       Key   : in     Key_Type;
       Alloc : in     Natural := 0);
   not overriding procedure Set_State_Non_Accepting
      (Self  : in out Nfa_Class;
       State : in     State_Id_Type;
       Alloc : in     Natural);
   not overriding procedure Set_State_Transition
      (Self  : in out Nfa_Class;
       State : in     State_Id_Type;
       Index : in     Positive;
       Trans : in     Transition_Type);
   not overriding procedure Append_State_Transition
      (Self  : in out Nfa_Class;
       State : in     State_Id_Type;
       Trans : in     Transition_Type);

   not overriding procedure Mark_Transitions
      (Self  : in     Nfa_Class;
       These : in     Active_State_List_Pointer_Type;
       Next  : in     Active_State_List_Pointer_Type;
       Value : in     Wide_Wide_Character);

   not overriding procedure Epsilon_Transitions
      (Self  : in     Nfa_Class;
       Next  : in     Active_State_List_Pointer_Type);



   type Nfa_State_Class is new Finite_Automata_State_Class with private;
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





   type Working_Nfa_Class is tagged private;

   function Image(Self : Working_Nfa_Class) return String;
   procedure Internal_Set_Nfa
      (Self : in out Working_Nfa_Class;
       Nfa  : in     Nfa_Class'CLASS);



   -- Control debug output of package
   procedure Set_Debug(Value : in Boolean);

private

   not overriding function Get_States(Self : Nfa_Class) return State_List_Pointer_Type;

   type Nfa_State_Class is new Finite_Automata_State_Class with
      record
         Nfa      : Nfa_Pointer_Type;
         Previous : Active_State_List_Pointer_Type;
         Next     : Active_State_List_Pointer_Type;
         Moves    : Natural := 0;
      end record;

   function Active_Count
      (List : in     Active_State_List_Pointer_Type) return Natural;



   package Transition_Vector is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Transition_Type,
       "=" => "=");

   type Working_State_Type is
      record
         State : State_Type;
         Trans : Transition_Vector.Vector;
      end record;

   function Image(Self : Working_State_Type) return String;
   function Equals(L, R : Working_State_Type) return Boolean;

   package State_Vector is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Working_State_Type,
       "=" => Equals);

   procedure Append_State
      (List  : in out State_Vector.Vector;
       State : in     State_Type);
   function Convert_To_Vector_Form
      (Nfa_States : State_List_Pointer_Type) return State_Vector.Vector;
   procedure Copy_State
      (List  : in     State_List_Pointer_Type;
       Index : in     State_Universe_Type;
       State : in     Working_State_Type);
   function Convert_To_Array_Form
      (Working_States : State_Vector.Vector) return State_List_Pointer_Type;


   procedure Initialize
      (Self : in out Working_Nfa_Class);
   procedure Adjust
      (Self : in out Working_Nfa_Class);
   procedure Finalize
      (Self : in out Working_Nfa_Class);

   type Working_Nfa_Class is new Ada.Finalization.Controlled with
      record
         Working_States : State_Vector.Vector;
      end record;


end kv.apg.fa.nfa;
