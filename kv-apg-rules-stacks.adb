with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;

with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Tags;

with Ada.Text_IO; use Ada.Text_IO;

with kv.apg.locations;
with kv.apg.incidents;

package body kv.apg.rules.stacks is

   use Ada.Strings.UTF_Encoding;
   use Ada.Strings.UTF_Encoding.Strings;

   use kv.apg.incidents; -- Severity_Type

   ----------------------------------------------------------------------------
   procedure Push_State
      (Self  : in out Stack_Class;
       State : in     State_Entry_Type) is
   begin
      Self.Stack.Append(State);
   end Push_State;

   ----------------------------------------------------------------------------
   function Pop_State(Self : in out Stack_Class) return State_Entry_Type is
      Answer : State_Entry_Type;
   begin
      Answer := Self.Stack.Last_Element;
      Self.Stack.Delete_Last;
      return Answer;
   end Pop_State;

   ----------------------------------------------------------------------------
   function Top_State(Self : Stack_Class) return State_Index_Type is
      Answer : State_Entry_Type;
   begin
      Answer := Self.Stack.Last_Element;
      return Answer.State;
   end Top_State;

end kv.apg.rules.stacks;
