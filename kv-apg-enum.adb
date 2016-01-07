with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body kv.apg.enum is

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Enumeration_Class;
       Name : in     String_Type) is
   begin
      Self.Name := Name;
      Self.First := 0;
      Self.Last := 0;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Append
      (Self : in out Enumeration_Class;
       Name : in     kv.apg.tokens.Token_Class) is
      V : Value_Type;
   begin
      Self.Last := Self.Last + 1;
      V.Value := Self.Last;
      V.Name := Name;
      Self.Values.Append(V);
   end Append;

   ----------------------------------------------------------------------------
   procedure Append
      (Self          : in out Enumeration_Class;
       Name          : in     kv.apg.tokens.Token_Class;
       Associativity : in     Token_Associativity_Type;
       Precedence    : in     Token_Precedence_Type) is
      V : Value_Type;
   begin
      Self.Last := Self.Last + 1;
      V.Value := Self.Last;
      V.Name := Name;
      V.Associativity := Associativity;
      V.Precedence := Precedence;
      Self.Values.Append(V);
   end Append;

   ----------------------------------------------------------------------------
   function Get_Count(Self : Enumeration_Class) return Natural is
   begin
      return Natural(Self.Values.Length);
   end Get_Count;

   ----------------------------------------------------------------------------
   procedure Write
      (Self   : in     Enumeration_Class;
       Writer : in out kv.apg.writer.Writer_Class'CLASS) is

      Count : Positive := 1;
      Last : Natural := Self.Get_Count;

   begin
      Writer.Write_Line("   type " & To_UTF(+Self.Name) & " is");
      for V of Self.Values loop
         if Count = 1 then
            Writer.Write_Line("      (" & To_UTF(+V.Name.Get_Data) & ",");
         elsif Count = Last then
            Writer.Write_Line("       " & To_UTF(+V.Name.Get_Data) & ");");
         else
            Writer.Write_Line("       " & To_UTF(+V.Name.Get_Data) & ",");
         end if;
         Count := Count + 1;
      end loop;
   end Write;

   ----------------------------------------------------------------------------
   function Get(Self : Enumeration_Class; Index : Positive) return String_Type is
   begin
      for V of Self.Values loop
         if V.Value = Index then
            return V.Name.Get_Data;
         end if;
      end loop;
      return +"";
   end Get;

   ----------------------------------------------------------------------------
   function Get(Self : Enumeration_Class; Index : Positive) return kv.apg.tokens.Token_Class is
   begin
      for V of Self.Values loop
         if V.Value = Index then
            return V.Name;
         end if;
      end loop;
      return kv.apg.tokens.Invalid_Token;
   end Get;

   ----------------------------------------------------------------------------
   function Get(Self : Enumeration_Class; Name : String_Type) return Integer is
   begin
      for V of Self.Values loop
         if V.Name.Get_Data = Name then
            return V.Value;
         end if;
      end loop;
      return Not_Found_Error;
   end Get;

   ----------------------------------------------------------------------------
   function Get_Associativity(Self : Enumeration_Class; Index : Positive) return Token_Associativity_Type is
   begin
      for V of Self.Values loop
         if V.Value = Index then
            return V.Associativity;
         end if;
      end loop;
      return Neither;
   end Get_Associativity;

   ----------------------------------------------------------------------------
   function Get_Precedence(Self : Enumeration_Class; Index : Positive) return Token_Precedence_Type is
   begin
      for V of Self.Values loop
         if V.Value = Index then
            return V.Precedence;
         end if;
      end loop;
      return 0;
   end Get_Precedence;

end kv.apg.enum;
