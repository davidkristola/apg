with kv.apg.directives;

package body kv.apg.config is

   use String_Maps;

   --==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--
   package Set_Util is
      type Visitor_Class is new kv.apg.directives.Directive_Visitor_Class with private;

      not overriding procedure Initialize
         (Self : in out Visitor_Class;
          Map  : access String_Maps.Map);

      overriding procedure Process_Set
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Set_Class'CLASS);

   private
      type Visitor_Class is new kv.apg.directives.Directive_Visitor_Class with
         record
            Map : access String_Maps.Map;
         end record;
   end Set_Util;

   --==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--
   package body Set_Util is

      -------------------------------------------------------------------------
      not overriding procedure Initialize
         (Self : in out Visitor_Class;
          Map  : access String_Maps.Map) is
      begin
         Self.Map := Map;
      end Initialize;

      -------------------------------------------------------------------------
      overriding procedure Process_Set
         (Self      : in out Visitor_Class;
          Directive : in out kv.apg.directives.Set_Class'CLASS) is
      begin
         Self.Map.Include(To_UTF(+Directive.Get_Name), To_UTF(+Directive.Get_Value));
      end Process_Set;

   end Set_Util;


   ----------------------------------------------------------------------------
   procedure Initialize
      (Self   : in out Key_Value_Map_Class;
       Parser : in     kv.apg.parse.Parser_Pointer_Type) is

      Visitor : Set_Util.Visitor_Class;

   begin
      Visitor.Initialize(Self.Store'UNCHECKED_ACCESS);
      Parser.Process_Directives(Visitor);
   end Initialize;

   ----------------------------------------------------------------------------
   function Has_Key(Self : Key_Value_Map_Class; Key : String) return Boolean is
   begin
      return Self.Store.Find(Key) /= No_Element;
   end Has_Key;

   ----------------------------------------------------------------------------
   function Get_Value(Self : Key_Value_Map_Class; Key : String) return String is
   begin
      if Self.Has_Key(Key) then
         return Self.Store.Element(Key);
      end if;
      return "";
   end Get_Value;

end kv.apg.config;
