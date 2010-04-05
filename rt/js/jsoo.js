// Caml name:  new_obj
// Caml type:  unit -> obj
function jsoo_new (o) {
    return [];
}
    
// Caml name:  eval
// Caml type:  string -> obj
function jsoo_eval (s) {
    try {
	var code = string_from_value (s) ;
	return eval (code);
    } catch (e) {
	caml_catch(e);
	this.failwith("jsoo_call: " + e.message);
    }
}

// Caml name:  get
// Caml type:  string -> obj -> obj
function jsoo_get (f, o) {
    return o[string_from_value (f)];
}

// Caml name:  set
// Caml type:  string -> obj -> obj -> unit
 function jsoo_set (f, v, o) {
     o[string_from_value (f)] = v;
     return UNIT;
}

// Caml name:  extract
// Caml type:  obj -> value
function jsoo_extract (o) {
    //   | Obj of obj        0
    //   | Num of float      1
    //   | String of string  2
    //   | Block of Obj.t    3
    //   | Nil
    if (o == null)
	return 0;
    if (o instanceof Block) {
	var b = mk_block (1, 3);
	b.set (0, o);
	return b;
    }
    if (typeof o == 'string') {
	var b = mk_block (1, 2);
	b.set (0, value_from_string (o));
	return b;
    }
    if (typeof o == 'number') {
	var b = mk_block (1, 1);
	b.set (0, box_float (o));
	return b;
    }
    var b = mk_block (1, 0);
    b.set (0, o);
    return b;
}

// Caml name:  extract_bool
// Caml type:  obj -> bool
function jsoo_extract_bool (o) {
    if (o)
	return 1;
    return 0;
}

// Caml name:  inject
// Caml type:  value -> obj
function jsoo_inject (o) {
    if (!is_block(o))
	return null;
    if (o.tag == 2)
	return string_from_value (o.get(0));
    if (o.tag == 1)
	return unbox_float (o.get(0));
    return o.get (0);
}

// Caml name:  call
// Caml type:  obj -> obj array -> obj -> obj
function jsoo_call (d, args, o) {
    try {
	return o.apply (d, args.content) ;
    } catch (e) {
	caml_catch(e);
	this.failwith("jsoo_call: " + e.message);
    }
}

// Caml name:  wrap_event
// Caml type:  (unit -> unit) > obj
function jsoo_wrap_event (clos, res) {
    var vm = this;
    return function (evt) {
	var pid = vm.thread_new (clos);
	var p = vm.ctx;
	do {
	    if (p.pid == pid) {
		p.event_args = evt;
		break;
	    }
	    p = p.n_ctx;
	} while (p != vm.ctx);
	vm.run ();
	return null;
    }
}

// Caml name:  get_event_args
// Caml type:  unit -> obj array
function jsoo_get_event_args (unit) {
    return this.ctx.event_args;
}

// Type: unit -> obj
function current_vm () {
    return this;
}
