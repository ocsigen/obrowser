caml_closure_table = [] ;

function caml_run_from_table (vm, id) {
    if (caml_closure_table [id] == null) {
	vm.failwith ("unbound closure");
    }
    vm.thread_new (caml_closure_table [id]);
    vm.run ();
}

RT.caml_register_closure = function (id, clos) {
    caml_closure_table[id] = clos;
}
