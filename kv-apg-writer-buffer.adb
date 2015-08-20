package body kv.apg.writer.buffer is

   use Ada.Strings.Wide_Wide_Unbounded;

   ----------------------------------------------------------------------------
   overriding procedure Write_Line
      (Self : in out Buffer_Writer_Class;
       Line : in     String_Type) is
      Full_Line : String_Type := Self.Unfinished & Line;
   begin
      Self.Lines.Append(Full_Line);
      Self.Unfinished := To_String_Type("");
   end Write_Line;

   ----------------------------------------------------------------------------
   overriding procedure Write_Line
      (Self : in out Buffer_Writer_Class;
       Line : in     String) is
   begin
      Self.Write_Line(To_String_Type(Line));
   end Write_Line;

   ----------------------------------------------------------------------------
   overriding procedure Write_Some
      (Self : in out Buffer_Writer_Class;
       Part : in     String_Type) is
   begin
      Self.Unfinished := Self.Unfinished & Part;
   end Write_Some;

   ----------------------------------------------------------------------------
   overriding procedure Write_Some
      (Self : in out Buffer_Writer_Class;
       Part : in     String) is
   begin
      Self.Write_Some(To_String_Type(Part));
   end Write_Some;

   ----------------------------------------------------------------------------
   overriding procedure New_Line
      (Self  : in out Buffer_Writer_Class;
       Count : in     Positive := 1) is
   begin
      for I in 1 .. Count loop
         Self.Write_Line("");
      end loop;
   end New_Line;

   ----------------------------------------------------------------------------
   overriding function Line_Count
      (Self : Buffer_Writer_Class) return Natural is
   begin
      return Natural(Self.Lines.Length);
   end Line_Count;

   ----------------------------------------------------------------------------
   overriding function Get_Line
      (Self : Buffer_Writer_Class;
       Line : Positive) return String_Type is
   begin
      return Self.Lines.Element(Line);
   end Get_Line;

end kv.apg.writer.buffer;
