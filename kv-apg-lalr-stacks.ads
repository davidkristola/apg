with kv.apg.lalr.grammars;

package kv.apg.lalr.stacks is

   type State_Entry_Type is
      record
         Symbol : Constant_Symbol_Pointer;
         State  : kv.apg.lalr.grammars.State_Index_Type;
      end record;

   type Stack_Class is tagged private;
   procedure Push_State
      (Self  : in out Stack_Class;
       State : in     State_Entry_Type);
   function Pop_State(Self : in out Stack_Class) return State_Entry_Type;
   function Top_State(Self : Stack_Class) return kv.apg.lalr.grammars.State_Index_Type;

private

   package State_Vector is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => State_Entry_Type);

   type Stack_Class is tagged
      record
         Stack : State_Vector.Vector;
      end record;

end kv.apg.lalr.stacks;
