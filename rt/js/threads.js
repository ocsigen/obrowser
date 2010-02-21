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
function thread_initialize () {
    return UNIT;
}

// Caml name: thread_initialize_preemption
// Type:      unit -> unit
function thread_initialize_preemption () {
    return UNIT;
}

// Caml name: thread_new
// Type:      (unit -> unit) -> t
function thread_new (clos) {
    // type t = pid
    return this.thread_new (clos);
}

// Caml name: thread_self
// Type:      unit -> t
function thread_self (unit) {
    return this.ctx.pid;
}

// Caml name: thread_kill
// Type:      t -> unit
function thread_kill (pid) {
    this.thread_kill (pid);
    return UNIT;
}

// Caml name: thread_yield
// Type:      unit -> unit
thread_request_reschedule = 
thread_yield = function () {
    this.thread_yield ();
    return UNIT;
}

// Caml name: id
// Type:      t -> int
function thread_id (pid) {
    throw pid;
}

// Caml name: thread_sleep
// Type:      unit -> unit
function thread_sleep () {
    this.ctx.status = SLEEP;
    return UNIT;
}

// Caml name: thread_wakeup
// Type:      t -> unit
function thread_wakeup (pid) {
    this.thread_wakeup (pid);
    return UNIT;
}

// Caml name: thread_wait_pid, thread_join
// Type:      t -> unit
thread_wait_pid =
thread_join = function (pid) {
    this.ctx.status = WAIT;
    this.ctx.waiting_for = pid;
}

// Caml name: thread_delay
// Type:      float -> unit
function thread_delay (s) {
    this.thread_delay (s.get (0) * 1000);
    return UNIT;
}

// Caml name: thread_uncaught_exception
// Type:      exn -> unit
function thread_uncaught_exception (e) {
    debug ("Fatal error: " +
	   string_from_value (e.get(0).get(0))
	   + (this.ctx.accu.size == 2
	      ?(" " + repr (e.get (1), 1000))
	      :""));
    this.thread_kill (this.ctx.pid);
}

// Caml name: create
// Type:      unit -> t
function caml_js_mutex_create (u) {
    var mutex = { locked: false, owner:0 };
    return box_abstract (mutex);
}

// Caml name : lock
// Type:       t -> unit
function caml_js_mutex_lock (m) {
    var mutex = unbox_abstract (m);
    if (mutex.locked) {
	var vm = this ;
	this.thread_wait (mutex, function () {
		caml_js_mutex_lock.call (vm, m);
	    });
    } else {
	mutex.locked = true;
	mutex.owner = this.ctx.pid;
	return UNIT ;
    }
}

// Caml name : try_lock
// Type:       t -> bool
function caml_js_mutex_try_lock (m) {
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
function caml_js_mutex_unlock (m) {
    var mutex = unbox_abstract (m);
    if (mutex.locked && mutex.owner == this.ctx.pid) {
	mutex.locked = false;
	this.thread_notify_one (mutex);
    }
    return UNIT;
}
