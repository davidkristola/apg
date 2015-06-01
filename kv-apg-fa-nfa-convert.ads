with Ada.Finalization;
with Ada.Containers.Vectors;

with kv.apg.fa.dfa;
with kv.apg.fast;

package kv.apg.fa.nfa.convert is

   type Converter_Class is tagged private;

   function Image(Self : Converter_Class) return String;

   procedure Nfa_To_Dfa
      (Self : in out Converter_Class;
       Nfa  : in     Nfa_Class;
       Dfa  :    out kv.apg.fa.dfa.Dfa_Class);

   -- The following internal routines are exposed for unit testing.
   -- You should not call these directly.

   procedure Internal_Set_Nfa
      (Self : in out Converter_Class;
       Nfa  : in     Nfa_Class);

   procedure Internal_Unmix_Epsilon_Transitions
      (Self : in out Converter_Class);

   procedure Internal_Bump_Non_Epsilon_At
      (Self  : in out Converter_Class;
       Index : in     Positive);

   function Internal_Get_Dfa
      (Self : Converter_Class) return kv.apg.fa.dfa.Dfa_Class;

private

   use kv.apg.fast;

   package Transition_Vector is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Transition_Type,
       "=" => "=");

   type Working_State_Type is
      record
         State : State_Type;
         Trans : Transition_Vector.Vector;
      end record;

   function Equals(L, R : Working_State_Type) return Boolean;

   package State_Vector is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Working_State_Type,
       "=" => Equals);


   procedure Initialize
      (Self : in out Converter_Class);
   procedure Adjust
      (Self : in out Converter_Class);
   procedure Finalize
      (Self : in out Converter_Class);

   type Converter_Class is new Ada.Finalization.Controlled with
      record
         Working_States : State_Vector.Vector;
      end record;

end kv.apg.fa.nfa.convert;
