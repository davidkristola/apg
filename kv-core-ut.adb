-- This version of this file is hereby given to the Public Domain without restrictions or assurances.

with Ada.Text_IO;

package body kv.core.ut is

   use Ada.Text_IO;
   function "+"(S : String) return String_Type renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+"(U : String_Type) return String renames Ada.Strings.Unbounded.To_String;

   ----------------------------------------------------------------------------
   procedure Log(T : in out Test_Class; message : in String) is
   begin
      Put_Line(message);
   end Log;

   ----------------------------------------------------------------------------
   procedure Assert(T : in out Test_Class; condition : in Boolean; fail_message : in String) is
   begin
      T.Asserts := T.Asserts + 1;
      T.Suite.Increment_Asserts;
      if not condition then
         T.Fails := T.Fails + 1;
         T.Suite.Increment_Fail;
         Put_Line((+T.Name) & " *** " & fail_message);
      end if;
   end Assert;

   ----------------------------------------------------------------------------
   procedure Remember
      (T     : in out Test_Class;
       Suite : in     Suite_Pointer_Type;
       Name  : in     String) is
   begin
      T.Suite := Suite;
      T.Name := +Name;
   end Remember;

   ----------------------------------------------------------------------------
   procedure run_all(self : in out Test_Suite_Class) is
   begin
      for B : Bundle_Type of self.tests loop
         --Put_Line("Running " & (+B.Name));
         B.Test.Set_Up;
         begin
            B.Test.Run;
         exception
            when others =>
               self.Fails := self.Fails + 1; --TODO: make better
               Put_Line((+B.Name) & " *** EXCEPTION!");
         end;
         B.Test.Tear_Down;
      end loop;
   end run_all;

   ----------------------------------------------------------------------------
   procedure register(self : in out Test_Suite_Class; test : in Test_Pointer_Type; name : in String) is
      bundle : Bundle_Type;
   begin
      --Put_Line("Registering " & name);
      bundle.Name := +name;
      bundle.Test := test;
      self.tests.Append(bundle);
      test.remember(self'UNCHECKED_ACCESS, Name);
   end register;

   ----------------------------------------------------------------------------
   procedure Increment_Fail(self : in out Test_Suite_Class) is
   begin
      self.Fails := self.Fails + 1;
   end Increment_Fail;

   ----------------------------------------------------------------------------
   procedure Increment_Asserts(self : in out Test_Suite_Class) is
   begin
      self.Asserts := self.Asserts + 1;
   end Increment_Asserts;

   ----------------------------------------------------------------------------
   procedure Final_Report(self : in out Test_Suite_Class) is
   begin
      New_Line(2);
      Put_Line("Tests run       = " & Ada.Containers.Count_Type'IMAGE(self.Tests.Length));
      Put_Line("Asserts checked = " & Natural'IMAGE(self.Asserts));
      Put_Line("Test failures   = " & Natural'IMAGE(self.Fails));
   end Final_Report;

   ----------------------------------------------------------------------------
   procedure instantiate_and_run_this_main is
      suite : aliased Test_Suite_Class;
   begin
      New_Line(2);
      register_all_tests(suite'UNCHECKED_ACCESS);
      suite.run_all;
      suite.Final_Report;
   end instantiate_and_run_this_main;

end kv.core.ut;
