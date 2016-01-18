-- This file is machine generated. Do not edit.

package my_lex_example.Static_Parser_Tables is

   subtype Terminal_Index_Type is Token_Type range End_Of_File .. key_plus;
   subtype Non_Terminal_Index_Type is Token_Type range rule_S .. Meta_Start_Rule;

   type State_Index_Type is new Natural range 0 .. 8;

   type Production_Index_Type is new Positive range 1 .. 5;

   type Action_Type is (Shift, Reduce, Accept_Input, Error);

   type Action_Entry_Type(What : Action_Type := Error) is
      record
         case What is
            when Shift  => Destination : State_Index_Type;
            when Reduce => Production  : Production_Index_Type;
            when others => null;
         end case;
      end record;

   e   : constant Action_Entry_Type := (What => Error);
   acc : constant Action_Entry_Type := (What => Accept_Input);
   s1 : constant Action_Entry_Type := (Shift, 1);
   s2 : constant Action_Entry_Type := (Shift, 2);
   r3 : constant Action_Entry_Type := (Reduce, 3);
   r5 : constant Action_Entry_Type := (Reduce, 5);

   s : constant Action_Type := Shift;
   r : constant Action_Type := Reduce;

   Actions : constant array(State_Index_Type, Terminal_Index_Type) of Action_Entry_Type :=
      (00 => ( e, e, (s, 1), e, e, (s, 2)),
       01 => ( e, e, (r, 3), e, e, (r, 5)),
       02 => (s1, e, s2, e, e,  e),
       03 => (s1, e, s2, e, (s, 3),  e),
       04 => (s1, e, s2, e, e,  e),
       05 => (s1, e, s2, e, e,  e),
       06 => (s1, e, s2, e, e,  e),
       07 => (s1, e, s2, e, e,  e),
       08 => (s1, e, s2, e, e,  e));


end my_lex_example.Static_Parser_Tables;
