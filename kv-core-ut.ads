-- This version of this file is hereby given to the Public Domain without restrictions or assurances.

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

package kv.core.ut is

   subtype String_Type is Ada.Strings.Unbounded.Unbounded_String;

   type Test_Class is abstract tagged private;
   procedure Log(T : in out Test_Class; message : in String);
   procedure Assert(T : in out Test_Class; condition : in Boolean; fail_message : in String);
   procedure Set_Up(T : in out Test_Class) is null;
   procedure Tear_Down(T : in out Test_Class) is null;
   procedure Run(T : in out Test_Class) is abstract;

   type Test_Pointer_Type is access Test_Class'CLASS;

   type Test_Suite_Class is tagged private;
   type Suite_Pointer_Type is access all Test_Suite_Class'CLASS;

   procedure register(self : in out Test_Suite_Class; test : in Test_Pointer_Type; name : in String);

   generic
      with procedure register_all_tests(suite : in Suite_Pointer_Type);
   procedure instantiate_and_run_this_main;

private

   procedure Remember
      (T     : in out Test_Class;
       Suite : in     Suite_Pointer_Type;
       Name  : in     String);

   type Test_Class is abstract tagged
      record
         Suite   : Suite_Pointer_Type;
         Name    : String_Type;
         Asserts : Natural := 0;
         Fails   : Natural := 0;
      end record;

   type Bundle_Type is
      record
         Name : String_Type;
         Test : Test_Pointer_Type;
      end record;

   package Test_List is new Ada.Containers.Doubly_Linked_Lists(Bundle_Type);

   type Test_Suite_Class is tagged
      record
         Tests   : Test_List.List;
         Asserts : Natural := 0;
         Fails   : Natural := 0;
      end record;

   procedure run_all(self : in out Test_Suite_Class);
   procedure Increment_Fail(self : in out Test_Suite_Class);
   procedure Increment_Asserts(self : in out Test_Suite_Class);
   procedure Final_Report(self : in out Test_Suite_Class);

end kv.core.ut;
