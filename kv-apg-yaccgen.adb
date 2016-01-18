
package body kv.apg.yaccgen is


   ----------------------------------------------------------------------------
   procedure Initialize
      (Self         : in out Generator_Class;
       Parser       : in     kv.apg.parse.Parser_Pointer_Type;
       Package_Name : in     String_Type) is
   begin
   null;
   end Initialize;

   ----------------------------------------------------------------------------
   overriding function Convert
      (Self      : in out Generator_Class;
       Prefix    : in     String_Type;
       Postfix   : in     String_Type;
       Template  : in     String_Type) return kv.apg.writer.buffer.Buffer_Class'CLASS is
      Transformed : kv.apg.writer.buffer.Buffer_Writer_Class;
   begin
      Transformed.Write_Some(Prefix);
--      if Template = +"package_name" then
--         Self.Insert_Package_Name(Transformed);
--      elsif Template = +"token_type" then
--         Self.Insert_Token_Type(Transformed);
--      elsif Template = +"state_list" then
--         Self.Insert_State_List(Transformed);
--      else
--         Transformed.Write_Some("ERROR");
--      end if;
      Transformed.Write_Line(Postfix);
      return kv.apg.writer.buffer.Buffer_Class'CLASS(Transformed);
   end Convert;


end kv.apg.yaccgen;
