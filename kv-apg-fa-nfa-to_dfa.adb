
package body kv.apg.fa.nfa.to_dfa is

   ----------------------------------------------------------------------------
   procedure Set_Nfa
      (Self : in out Converter_Class;
       Nfa  : in     Nfa_Class) is
   begin
      null;
   end Set_Nfa;

   ----------------------------------------------------------------------------
   function Get_Dfa
      (Self : Converter_Class) return kv.apg.fa.dfa.Dfa_Class is

      Tmp : kv.apg.fa.dfa.Dfa_Class;

   begin
      return Tmp;
   end Get_Dfa;

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Converter_Class) is
   begin
      Self.Ref := 8;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Adjust
      (Self : in out Converter_Class) is
   begin
      null;
   end Adjust;

   ----------------------------------------------------------------------------
   procedure Finalize
      (Self : in out Converter_Class) is
   begin
      null;
   end Finalize;

end kv.apg.fa.nfa.to_dfa;
