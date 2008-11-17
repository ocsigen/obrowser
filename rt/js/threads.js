/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

// Caml name: thread_initialize
// Type:      unit -> unit
RT.thread_initialize = function () {
    return UNIT;
}

// Caml name: thread_initialize_preemption
// Type:      unit -> unit
RT.thread_initialize_preemption = function () {
    return UNIT;
}

// Caml name: thread_new
// Type:      (unit -> unit) -> t
RT.thread_new = function (clos) {
    // type t = pid
    return this.thread_new (clos);
}

// Caml name: thread_self
// Type:      unit -> t
RT.thread_self = function (unit) {
    return this.ctx.pid;
}

// Caml name: thread_kill
// Type:      t -> unit
RT.thread_kill = function (pid) {
    this.thread_kill (pid);
    return UNIT;
}

// Caml name: thread_yield
// Type:      unit -> unit
RT.thread_request_reschedule = 
RT.thread_yield = function () {
    this.thread_yield ();
    return UNIT;
}

// Caml name: id
// Type:      t -> int
RT.thread_id = function (pid) {
    throw pid;
}

// Caml name: thread_sleep
// Type:      unit -> unit
RT.thread_sleep = function () {
    this.ctx.status = SLEEP;
    return UNIT;
}

// Caml name: thread_wakeup
// Type:      t -> unit
RT.thread_wakeup = function (pid) {
    this.thread_wakeup (pid);
    return UNIT;
}

// Caml name: thread_wait_pid, thread_join
// Type:      t -> unit
RT.thread_wait_pid =
RT.thread_join = function (pid) {
    this.ctx.status = WAIT;
    this.ctx.waiting_for = pid;
}

// Caml name: thread_delay
// Type:      float -> unit
RT.thread_delay = function (s) {
    this.thread_delay (s.get (0) * 1000);
    return UNIT;
}

// Caml name: thread_uncaught_exception
// Type:      exn -> unit
RT.thread_uncaught_exception = function (e) {
    debug ("Fatal error: " +
	   string_from_value (e.get(0).get(0))
	   + (this.ctx.accu.size == 2
	      ?(" " + repr (e.get (1), 1000))
	      :""));
    this.thread_kill (this.ctx.pid);
}

// Caml name: create
// Type:      unit -> t
RT.caml_js_mutex_create = function (u) {
    var mutex = { locked: false, owner:0 };
    return box_abstract (mutex);
}

// Caml name : lock
// Type:       t -> unit
RT.caml_js_mutex_lock = function (m) {
    var mutex = unbox_abstract (m);
    if (mutex.locked) {
	var vm = this ;
	this.thread_wait (mutex, function () {
		RT.caml_js_mutex_lock.call (vm, m);
	    });
    } else {
	mutex.locked = true;
	mutex.owner = this.ctx.pid;
	return UNIT ;
    }
}

// Caml name : try_lock
// Type:       t -> bool
RT.caml_js_mutex_try_lock = function (m) {
    var mutex = unbox_abstract (m);
    if (mutex.locked) {
	return mk_bool (false) ;
    } else {
	mutex.locked = true;
	mutex.owner = this.ctx.pid;
	return mk_bool (true) ;
    }
}

// Caml name: unlock
// Type:      t -> unit
RT.caml_js_mutex_unlock = function (m) {
    var mutex = unbox_abstract (m);
    if (mutex.locked && mutex.owner == this.ctx.pid) {
	mutex.locked = false;
	this.thread_notify_one (mutex);
    }
    return UNIT;
}
