with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Wide_Wide_Unbounded;

with Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;


package body kv.apg.regex is

   use Ada.Strings.Wide_Wide_Unbounded; -- &
   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Characters.Conversions;

   Quotation : constant Wide_Wide_Character := To_Wide_Wide_Character(Ada.Characters.Latin_1.Quotation);

   -------------------------------------------------------------------------
   procedure Process_Tree(Self : in out Node_Class) is
   begin
      if Self.Left /= null then
         Self.Left.Process_Tree;
      end if;
      Node_Class'CLASS(Self).Process_This;
      if Self.Right /= null then
         Self.Right.Process_Tree;
      end if;
   end Process_Tree;

   -------------------------------------------------------------------------
   procedure Set_Left(Self : in out Node_Class; Node : in Node_Pointer_Type) is
   begin
      Self.Left := Node;
   end Set_Left;

   -------------------------------------------------------------------------
   procedure Set_Right(Self : in out Node_Class; Node : in Node_Pointer_Type) is
   begin
      Self.Right := Node;
   end Set_Right;


   -------------------------------------------------------------------------
   function Image_Tree(Self : in out Node_Class) return String_Type is
      Answer : String_Type := To_String_Type("");
   begin
      if Self.Left /= null then
         Answer := Self.Left.Image_Tree & To_String_Type(" ");
      end if;
      Answer := Answer & Node_Class'CLASS(Self).Image_This;
      if Self.Right /= null then
         Answer := Answer & To_String_Type(" ") & Self.Right.Image_Tree;
      end if;
      return Answer;
   end Image_Tree;


   -------------------------------------------------------------------------
   not overriding procedure Initialize(Self : in out Match_Node_Class; Value : in     String_Type) is
   begin
      Self.Value := Value;
   end Initialize;

   -------------------------------------------------------------------------
   procedure Process_This(Self : in out Match_Node_Class) is
   begin
      Put_Line("Match_Node_Class.Process_This");
   end Process_This;

   -------------------------------------------------------------------------
   overriding function Image_This(Self : in out Match_Node_Class) return String_Type is
   begin
      return Quotation & Self.Value & Quotation;
   end Image_This;



   -------------------------------------------------------------------------
   not overriding procedure Initialize(Self : in out Match_Any_Node_Class) is
   begin
      null;
   end Initialize;

   -------------------------------------------------------------------------
   procedure Process_This(Self : in out Match_Any_Node_Class) is
   begin
      Put_Line("Match_Any_Node_Class.Process_This");
   end Process_This;

   -------------------------------------------------------------------------
   overriding function Image_This(Self : in out Match_Any_Node_Class) return String_Type is
   begin
      return To_String_Type(".");
   end Image_This;

end kv.apg.regex;
