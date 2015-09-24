
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
       Name : in     String_Type) is
      V : Value_Type;
   begin
      Self.Last := Self.Last + 1;
      V.Value := Self.Last;
      V.Name := Name;
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
            Writer.Write_Line("      (" & To_UTF(+V.Name) & ",");
         elsif Count = Last then
            Writer.Write_Line("       " & To_UTF(+V.Name) & ");");
         else
            Writer.Write_Line("       " & To_UTF(+V.Name) & ",");
         end if;
         Count := Count + 1;
      end loop;
   end Write;

end kv.apg.enum;