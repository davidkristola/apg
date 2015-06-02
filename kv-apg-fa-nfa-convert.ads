
with kv.apg.fa.dfa;
with kv.apg.fast;

package kv.apg.fa.nfa.convert is

   -- This converter takes an NFA and converts it to a DFA.
   --
   type To_Dfa_Class is new Working_Nfa_Class with null record;

   procedure Nfa_To_Dfa
      (Self : in out To_Dfa_Class;
       Nfa  : in     Nfa_Class;
       Dfa  :    out kv.apg.fa.dfa.Dfa_Class);

   -- The following internal routines are exposed for unit testing.
   -- You should not call these directly.

   procedure Internal_Subset_Construction
      (Self : in out To_Dfa_Class);

   function Internal_Get_Dfa
      (Self : To_Dfa_Class) return kv.apg.fa.dfa.Dfa_Class;




   -- This converter takes an NFA and cleans out the epsilon transitions.
   --
   type To_Cnfa_Class is new Working_Nfa_Class with null record;

   procedure Nfa_To_Cnfa
      (Self : in out To_Cnfa_Class;
       Nfa  : in     Nfa_Class;
       Cnfa :    out Nfa_Class);

   -- The following internal routines are exposed for unit testing.
   -- You should not call these directly.

   procedure Internal_Unmix_Epsilon_Transitions
      (Self : in out To_Cnfa_Class);

   procedure Internal_Bump_Non_Epsilon_At
      (Self  : in out To_Cnfa_Class;
       Index : in     Positive);

   procedure Internal_Unchain_Epsilon_Transitions
      (Self : in out To_Cnfa_Class);

   function Internal_Get_Cnfa
      (Self : To_Cnfa_Class) return kv.apg.fa.nfa.Nfa_Class;

end kv.apg.fa.nfa.convert;
