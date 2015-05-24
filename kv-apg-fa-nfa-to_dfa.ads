with Ada.Finalization;

with kv.apg.fa.dfa;

package kv.apg.fa.nfa.to_dfa is

   type Converter_Class is tagged private;

   procedure Set_Nfa
      (Self : in out Converter_Class;
       Nfa  : in     Nfa_Class);

   function Get_Dfa
      (Self : Converter_Class) return kv.apg.fa.dfa.Dfa_Class;

private

   procedure Initialize
      (Self : in out Converter_Class);
   procedure Adjust
      (Self : in out Converter_Class);
   procedure Finalize
      (Self : in out Converter_Class);

   type Converter_Class is new Ada.Finalization.Controlled with
      record
         Ref : Integer;
      end record;

end kv.apg.fa.nfa.to_dfa;
