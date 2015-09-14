-- This file is machine generated. Do not edit.

with my_lex_example.States;
with my_lex_example.Nfa;

package my_lex_example.Static_Nfa_Definition is

   use my_lex_example.States;
   use my_lex_example.Nfa;

   subtype WWC is Wide_Wide_Character;

   T1 : aliased constant Transition_List_Type := (1 => (Match, 2, WWC'VAL(97)));
   T2 : aliased constant Transition_List_Type := (1 => (Match, 3, WWC'VAL(98)), 2 => (Match, 2, WWC'VAL(97)));
   T3 : aliased constant Transition_List_Type := (1 => (Match, 4, WWC'VAL(99)), 2 => (Match, 2, WWC'VAL(97)), 3 => (Match, 3, WWC'VAL(98)));
   T4 : aliased constant Transition_List_Type := (1 => (Match, 5, WWC'VAL(100)), 2 => (Match, 2, WWC'VAL(97)), 3 => (Match, 3, WWC'VAL(98)), 4 => (Match, 4, WWC'VAL(99)));
   T5 : aliased constant Transition_List_Type := (1 => (Match, 5, WWC'VAL(100)));
   State_List : aliased constant State_List_Type :=
      (1 => (1, False, Invalid, T1'ACCESS),
       2 => (2, False, Invalid, T2'ACCESS),
       3 => (3, False, Invalid, T3'ACCESS),
       4 => (4, False, Invalid, T4'ACCESS),
       5 => (5, True, One, T5'ACCESS)
       );
   Nfa_Definition : aliased constant Nfa_Class := (Start => 1, States => State_List'ACCESS);

end my_lex_example.Static_Nfa_Definition;
