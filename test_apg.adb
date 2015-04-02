with kv.core.ut;
with kv.apg.tests;

procedure test_apg is
   procedure real_main is new kv.core.ut.instantiate_and_run_this_main(register_all_tests => kv.apg.tests.register);
begin
   real_main;
end test_apg;
