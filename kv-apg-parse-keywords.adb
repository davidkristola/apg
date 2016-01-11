with Ada.Strings.Wide_Wide_Unbounded;

with kv.core.wwstr;

with kv.apg.tokens;
with kv.apg.logger;
with kv.apg.regex;

package body kv.apg.parse.keywords is

   use Ada.Strings.Wide_Wide_Unbounded;
   use kv.core.wwstr;

   use kv.apg.tokens;
   use kv.apg.logger;

   -------------------------------------------------------------------------
   procedure Process_Colon_Or_Equal
      (Self  : in out Keywords_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Symbol then
         if Token.Get_Data_As_String = ":" then
            Self.Expect := Pre_Or_Star;
            return;
         elsif Token.Get_Data_As_String = "=" then
            Self.Expect := Keyword;
            return;
         end if;
      end if;
      Self.Handle_Error(Token, "Expected ':' or '='.");
   end Process_Colon_Or_Equal;

   -------------------------------------------------------------------------
   procedure Process_Pre_Or_Star
      (Self  : in out Keywords_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Word then
         Self.Pre_Pattern := Token;
         Self.Expect := Star;
      elsif Token.Get_Kind = A_Symbol and then Token.Get_Data_As_String = "*" then
         Self.Expect := Post_Or_Equal;
      else
         Self.Handle_Error(Token, "Expected a pre-* pattern or '*'.");
      end if;
   end Process_Pre_Or_Star;

   -------------------------------------------------------------------------
   procedure Process_Star
      (Self  : in out Keywords_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Symbol and then Token.Get_Data_As_String = "*" then
         Self.Expect := Post_Or_Equal;
      else
         Self.Handle_Error(Token, "Expected '*'.");
      end if;
   end Process_Star;

   -------------------------------------------------------------------------
   procedure Process_Post_Or_Equal
      (Self  : in out Keywords_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Word then
         Self.Post_Pattern := Token;
         Self.Expect := Equal;
      elsif Token.Get_Kind = A_Symbol and then Token.Get_Data_As_String = "=" then
         Self.Expect := Keyword;
      else
         Self.Handle_Error(Token, "Expected a post-* pattern or '='.");
      end if;
   end Process_Post_Or_Equal;

   -------------------------------------------------------------------------
   procedure Process_Equal
      (Self  : in out Keywords_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Symbol and then Token.Get_Data_As_String = "=" then
         Self.Expect := Keyword;
      else
         Self.Handle_Error(Token, "Expected '='.");
      end if;
   end Process_Equal;

   -------------------------------------------------------------------------
   procedure Accept_Keyword
      (Self  : in out Keywords_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      Self.Keywords.Append(Token);
      Self.Expect := Keyword_Or_Eos;
   end Accept_Keyword;

   -------------------------------------------------------------------------
   procedure Process_Keyword
      (Self  : in out Keywords_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Get_Kind = A_Word then
         Accept_Keyword(Self, Token);
      else
         Self.Handle_Error(Token, "Expected a keyword.");
      end if;
   end Process_Keyword;

   -------------------------------------------------------------------------
   procedure Process_Eos
      (Self  : in out Keywords_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      Self.Status := Done_Good;
   end Process_Eos;

   -------------------------------------------------------------------------
   procedure Process_Keyword_Or_Eos
      (Self  : in out Keywords_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      if Token.Is_Eos then
         Process_Eos(Self, Token);
      else
         Process_Keyword(Self, Token);
      end if;
   end Process_Keyword_Or_Eos;


   -------------------------------------------------------------------------
   overriding procedure Ingest_Token
      (Self  : in out Keywords_State_Class;
       Token : in     kv.apg.tokens.Token_Class) is
   begin
      case Self.Expect is
         when Colon_Or_Equal => Process_Colon_Or_Equal(Self, Token);
         when Pre_Or_Star    => Process_Pre_Or_Star(Self, Token);
         when Star           => Process_Star(Self, Token);
         when Post_Or_Equal  => Process_Post_Or_Equal(Self, Token);
         when Equal          => Process_Equal(Self, Token);
         when Keyword        => Process_Keyword(Self, Token);
         when Keyword_Or_Eos => Process_Keyword_Or_Eos(Self, Token);
      end case;
   end Ingest_Token;

   -------------------------------------------------------------------------
   overriding function Get_Directive(Self : in out Keywords_State_Class) return kv.apg.directives.Directive_Pointer_Type is
      Directive : access kv.apg.directives.Token_Class;
      Tree      : kv.apg.regex.Regular_Expression_Tree_Type;
      Keyword   : kv.apg.tokens.Token_Class;
      Working   : kv.apg.tokens.Token_Class;
      Name      : kv.apg.tokens.Token_Class;
   begin
      if Self.Status /= Done_Good then
         return null;
      end if;

      Keyword := Self.Keywords.First_Element;
      Self.Keywords.Delete_First;
      if Self.Keywords.Is_Empty then
         Self.Status := Done_Done;
      end if;

      Working.Initialize(Kind => A_String, Where => Keyword.Get_Location, Data => Keyword.Get_Data);
      Tree.Graft_To_Tree(Working);

      Name.Initialize(Kind => A_Word, Where => Keyword.Get_Location, Data => Self.Pre_Pattern.Get_Data & Keyword.Get_Data & Self.Post_Pattern.Get_Data);

      Directive := new kv.apg.directives.Token_Class;
      Directive.Initialize(Name => Name, Tree => Tree, Kind => kv.apg.directives.Accepting);
      return kv.apg.directives.Directive_Pointer_Type(Directive);
   end Get_Directive;

end kv.apg.parse.keywords;
