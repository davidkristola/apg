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

package body kv.apg.lalr.tables is

   use Ada.Strings.UTF_Encoding;
   use Ada.Strings.UTF_Encoding.Strings;

   use kv.apg.incidents; -- Severity_Type

   function Img(Arg : Production_Index_Type) return String renames Production_Index_Type'IMAGE;


   ----------------------------------------------------------------------------
   function Image(Action_Entry : Action_Entry_Type) return String is
   begin
      case Action_Entry.What is
         when Shift =>
            declare
               Answer : String := State_Index_Type'IMAGE(Action_Entry.Where);
            begin
               Answer(1) := 's';
               return Answer;
            end;
         when Reduce =>
            declare
               Answer : String := Img(Action_Entry.Production);
            begin
               Answer(1) := 'r';
               return Answer;
            end;
         when Accept_Input =>
            return "acc";
         when Error =>
            return "err";
      end case;
   end Image;



   Default_Action_Entry : constant Action_Entry_Type := (What => Error, Precedence => 0, Associativity => kv.apg.enum.Neither);


   ----------------------------------------------------------------------------
   procedure Initialize
      (Self        : in out Action_Table_Class;
       States      : in     State_Index_Type;
       Terminal_Lo : in     Terminal_Index_Type;
       Terminal_Hi : in     Terminal_Index_Type) is
   begin
      Self.Table := new Action_Table_Matrix_Type(0..States, Terminal_Lo..Terminal_Hi);
      Self.Table.all := (others => (others => Default_Action_Entry));
      Self.Errors := 0;
   end;

   ----------------------------------------------------------------------------
   procedure Set_Action
      (Self     : in out Action_Table_Class;
       Action   : in     Action_Entry_Type;
       State    : in     State_Index_Type;
       Terminal : in     Terminal_Index_Type;
       Logger   : in     kv.apg.logger.Safe_Logger_Pointer) is
      Current : constant Action_Entry_Type := Self.Table(State, Terminal);
      use kv.apg.enum;
   begin
      if Current = Default_Action_Entry then
         Self.Table(State, Terminal) := Action;
      elsif Action.Precedence > Current.Precedence then
         Logger.Note_By_Severity(Information, Image(Current) & " replaced by " & Image(Action) & " in state " & Img(State) & " for terminal " & Img(Terminal));
         Self.Table(State, Terminal) := Action;
         Self.Replacements := Self.Replacements + 1;
      elsif Current.What = Shift and Action.What = Reduce then
         Logger.Note_By_Severity(Debug, "Keeping " & Image(Current) & ", dropping " & Image(Action) & " in state " & Img(State) & " for terminal " & Img(Terminal));
         Self.Keeps := Self.Keeps + 1;
      else
         Logger.Note_By_Severity(Error, Image(Current) & "/" & Image(Action) & " conflict in state " & Img(State) & " for terminal " & Img(Terminal));
         Self.Errors := Self.Errors + 1;
      end if;
   end Set_Action;

   ----------------------------------------------------------------------------
   procedure Replace_Action
      (Self     : in out Action_Table_Class;
       Action   : in     Action_Entry_Type;
       State    : in     State_Index_Type;
       Terminal : in     Terminal_Index_Type;
       Logger   : in     kv.apg.logger.Safe_Logger_Pointer) is
   begin
      Logger.Note_By_Severity(Debug, Image(Self.Table(State, Terminal)) & " replaced by " & Image(Action) & " in state " & Img(State) & " for terminal " & Img(Terminal));
      Self.Table(State, Terminal) := Action;
   end Replace_Action;

   ----------------------------------------------------------------------------
   function Get_Action
      (Self     : Action_Table_Class;
       State    : State_Index_Type;
       Terminal : Terminal_Index_Type) return Action_Entry_Type is
   begin
      return Self.Table(State, Terminal);
   end Get_Action;

   ----------------------------------------------------------------------------
   function Error_Count(Self : Action_Table_Class) return Natural is
   begin
      return Self.Errors;
   end Error_Count;





   ----------------------------------------------------------------------------
   procedure Initialize
      (Self      : in out Goto_Table_Class;
       States    : in     State_Index_Type;
       Symbol_Lo : in     Non_Terminal_Index_Type;
       Symbol_Hi : in     Non_Terminal_Index_Type) is
   begin
      Self.Table := new Goto_Table_Matrix_Type(0..States, Symbol_Lo..Symbol_Hi);
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Set_Goto
      (Self        : in out Goto_Table_Class;
       Destination : in     State_Index_Type;
       State       : in     State_Index_Type;
       Symbol      : in     Non_Terminal_Index_Type) is
   begin
      Self.Table(State, Symbol) := Destination;
   end Set_Goto;

   ----------------------------------------------------------------------------
   function Get_Goto
      (Self   : Goto_Table_Class;
       State  : State_Index_Type;
       Symbol : Non_Terminal_Index_Type) return State_Index_Type is
   begin
      return Self.Table(State, Symbol);
   end Get_Goto;

end kv.apg.lalr.tables;
