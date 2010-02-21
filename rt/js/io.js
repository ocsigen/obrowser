/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

var channels = new Array ();
var nchannels = 0;
var CHANNEL_OUT = 1;
var CHANNEL_IN = 2;

function InputChannel (name) {
    this.name = name;
    this.buffer = uudecode (http_get ("uuencode/" + name, caml_raise));
    this.pos = 0;
}

InputChannel.prototype.getc = function () {
    if (this.pos == this.buffer.length)
	throw new Error ("eof");
    return this.buffer[this.pos++];
}

InputChannel.prototype.hilight = function () {
}

function open_for_input (name, fd) {
    if (name == "stdin")
	return new InputConsole ("FILE [" + name + "] (IN)", fd);
    else
	return new InputChannel (name);
}

function open_chan (name, mode) {
    channels[nchannels] = {
	name : name,
	fd : nchannels,
	mode : mode,
	buffer : [],
	con : ((mode == CHANNEL_OUT)
	       ? (new Console ("FILE [" + name + "] (OUT)"))
	       : (open_for_input (name, nchannels)))
    };
    return channels[nchannels++];
}

function open_out (name) {
    return open_chan (name, CHANNEL_OUT);
}

function open_in (name) {
    return open_chan (name, CHANNEL_IN);
}

/*
// Caml name: seek_out_blocking
// Type:      out_channel -> int -> unit
function caml_ml_seek_out () {
  throw new Error ("caml_ml_seek_out" + " not implemented");
}
// Caml name: pos_out
// Type:      out_channel -> int
function caml_ml_pos_out () {
  throw new Error ("caml_ml_pos_out" + " not implemented");
}
// Caml name: out_channel_length
// Type:      out_channel -> int
function caml_ml_channel_size () {
  throw new Error ("caml_ml_channel_size" + " not implemented");
}
// Caml name: close_out_channel
// Type:      out_channel -> unit
function caml_ml_close_channel () {
  throw new Error ("caml_ml_close_channel" + " not implemented");
}
// Caml name: set_binary_mode_out
// Type:      out_channel -> bool -> unit
function caml_ml_set_binary_mode () {
  throw new Error ("caml_ml_set_binary_mode" + " not implemented");
}
// Caml name: seek_in
// Type:      in_channel -> int -> unit
function caml_ml_seek_in () {
  throw new Error ("caml_ml_seek_in" + " not implemented");
}
// Caml name: pos_in
// Type:      in_channel -> int
function caml_ml_pos_in () {
  throw new Error ("caml_ml_pos_in" + " not implemented");
}
// Caml name: in_channel_length
// Type:      in_channel -> int
function caml_ml_channel_size () {
  throw new Error ("caml_ml_channel_size" + " not implemented");
}
// Caml name: set_binary_mode_in
// Type:      in_channel -> bool -> unit
function caml_ml_set_binary_mode () {
  throw new Error ("caml_ml_set_binary_mode" + " not implemented");}
// Caml name: seek_out
// Type:      out_channel -> int64 -> unit
function caml_ml_seek_out_64 () {
  throw new Error ("caml_ml_seek_out_64" + " not implemented");
}
// Caml name: pos_out
// Type:      out_channel -> int64
function caml_ml_pos_out_64 () {
  throw new Error ("caml_ml_pos_out_64" + " not implemented");
}
// Caml name: out_channel_length
// Type:      out_channel -> int64
function caml_ml_channel_size_64 () {
  throw new Error ("caml_ml_channel_size_64" + " not implemented");
}
// Caml name: seek_in
// Type:      in_channel -> int64 -> unit
function caml_ml_seek_in_64 () {
  throw new Error ("caml_ml_seek_in_64" + " not implemented");
}
// Caml name: pos_in
// Type:      in_channel -> int64
function caml_ml_pos_in_64 () {
  throw new Error ("caml_ml_pos_in_64" + " not implemented");
}
// Caml name: in_channel_length
// Type:      in_channel -> int64
function caml_ml_channel_size_64 () {
  throw new Error ("caml_ml_channel_size_64" + " not implemented");
}
*/

// Caml name: close_in
// Type:      in_channel -> unit
function caml_ml_close_channel (b) {
    b.set (0, null);
    return UNIT;
}

// Caml name: open_descriptor_out
// Type:      int -> out_channel
function caml_ml_open_descriptor_out (fd) {
    if (channels [fd].mode == CHANNEL_OUT) {
	var b = mk_block (1, ABSTRACT_TAG);
	b.set (0, channels [fd])
	return b;
    } else {
	caml_failwith (0, "trying to open descriptor " + fd + " in OUT mode");
    }
}

// Caml name: open_descriptor_in
// Type:      int -> in_channel
function caml_ml_open_descriptor_in (fd) {
    if (channels [fd].mode == CHANNEL_IN) {
	var b = mk_block (1, ABSTRACT_TAG);
	b.set (0, channels [fd])
	return b;
    } else {
	caml_failwith (0, "trying to open descriptor " + fd + " in IN mode");
    }
}

// Caml name: descr_inchan
// Type:      in_channel -> Unix.file_descr
function caml_channel_descriptor (chan) {
    return chan.get (0).fd;
}
// Caml name: descr_outchan
// Type:      out_channel -> Unix.file_descr
function caml_channel_descriptor (chan) {
    return chan.get (0).fd;
}

// Caml name: open_desc
// Type:      string -> open_flag list -> int -> int
function caml_sys_open (f, flags, perms) {
    var mode = 0;
    var open_flags = new Array (
	CHANNEL_IN, CHANNEL_OUT, CHANNEL_OUT, 0, 0, 0, 0, 0, 0
    );
    while (flags != 0) {
	mode |= open_flags[flags.get (0)];
	flags = flags.get (1);
    }
    if (flags == (CHANNEL_OUT | CHANNEL_IN))
	caml_failwith ("IN+OUT open not yet handled");
    return open_chan (string_from_value (f), mode).fd;
}

// Caml name: out_channels_list
// Type:      unit -> out_channel list
function caml_ml_out_channels_list (fd) {
    var b = 0;
    var r = 0;
    for (var i = 0;i < nchannels;i++) {
	if (channels[i].mode = CHANNEL_OUT) {
	    if (is_long (r)) {
		r = mk_block (2,0);
		r.set (1, 0);
		b = r;
	    } else {
		r.set (1, mk_block (2,0));
		r = r.get (1);
		r.set (1, 0);
	    }
	    var d = mk_block (1, 0);
	    d.set (0, channels [i]);
	    r.set (0, d);
	}
    }
    return b;
}

// Caml name: output_char_blocking
// Type:      out_channel -> char -> unit
function caml_ml_output_char (chan, c) {
    chan.get(0).con.puts (String.fromCharCode (c));
    return UNIT;
}

// Caml name: unsafe_output_partial
// Type:      out_channel -> string -> int -> int -> int
function caml_ml_output_partial (chan, s, st, len) {
    var so = "";
    for (var v = 0;v < len;v++) {
	so += String.fromCharCode (s.get (st + v))[0];
    }
    chan.get (0).con.puts (so);
    return len;
}

// Caml name: unsafe_output
// Type:      out_channel -> string -> int -> int -> unit
function caml_ml_output (chan, s, st, len) {
    var so = "";
    for (var v = 0;v < len;v++) {
	so += String.fromCharCode (s.get (st + v))[0];
    }
    chan.get (0).con.puts (so);
    return UNIT;
}

// Caml name: flush_partial
// Type:      out_channel -> bool
function caml_ml_flush_partial (s) {
    return TRUE;
}

// Caml name: flush
// Type:      out_channel -> bool
function caml_ml_flush (s) {
    return UNIT;
}

function caml_ml_input_scan_line (chan) {
    try {
	do {
	    var c = chan.con.getc ();
	    chan.buffer.push (c);
	} while (c != "\n".charCodeAt (0));
	prg.ctx.ioresult = chan.buffer.length;
	chan.con.hilight (false);
	return true;
    } catch (e) {
	switch (e.message) {
	case "wait":
	    return false;
	case "eof":
	    chan.con.hilight (false);
	    if (chan.buffer.length == 0)
		caml_raise_end_of_file ();
	    prg.ctx.ioresult = chan.buffer.length;
	    return true;
	default:
	    throw e;
	}
    }
}

// Caml name: unsafe_output
// Type:      in_channel -> int
function caml_ml_input_scan_line (chan) {
    vm_suspend_before_io (chan.get (0).fd,caml_ml_input_scan_line,chan.get(0));
}

function caml_ml_input_char (chan) {
    try {
	if (chan.buffer.length > 0) {
	    prg.ctx.ioresult = chan.buffer[0];
	    chan.buffer = chan.buffer.slice (1, chan.buffer.length);
	} else {
	    prg.ctx.ioresult = chan.con.getc ();
	}
	chan.con.hilight (false);
	return true;
    } catch (e) {
	switch (e.message) {
	case "wait":
	    return false;
	case "eof":
	    chan.con.hilight (false);
	    caml_raise_end_of_file ();
	    return true;
	default:
	    throw e;
	}
    }
}

// Caml name: input_char_blocking
// Type:      in_channel -> char
function caml_ml_input_char (chan) {
    vm_suspend_before_io (chan.get (0).fd, caml_ml_input_char, chan.get (0));
}

function caml_ml_input (args) {
    var chan = args[0],
	buff = args[1],
	st = args[2],
	len = args[3];
    try {
	while (chan.buffer.length < len)
	    chan.buffer.push (chan.con.getc ());
	var r = chan.buffer.slice (0, len);
	chan.buffer = chan.buffer.slice (len, chan.buffer.length);
	for (var i = 0;i < r.length;i++)
	    buff.set (i + st, r[i]);
	prg.ctx.ioresult = r.size;
	chan.con.hilight (false);
	return true;
    } catch (e) {
	switch (e.message) {
	case "wait":
	    return false;
	case "eof":
	    chan.con.hilight (false);
	    if (len > 0 && chan.buffer.length == 0)
		caml_raise_end_of_file ();
	    var r = mk_array_from_js (chan.buffer);
	    chan.buffer = [];
	    for (var i = 0;i < r.size;i++)
		buff.set (i + st, r.get (i));
	    prg.ctx.ioresult = r.size;
	    return true;
	default:
	    throw e;
	}
    }
}

// Caml name: unsafe_input_blocking
// Type:      in_channel -> string -> int -> int -> int
function caml_ml_input (chan, buff, st, len) {
    var args = [chan.get (0), buff, st, len];
    vm_suspend_before_io (chan.get (0).fd, caml_ml_input, args);
}

function caml_input_value (env) {
    try {
	switch (env.stage) {
	case 0 :
	    while (env.chan.buffer.length < 20)
		env.chan.buffer.push (env.chan.con.getc ());
	    env.bsize = input_size (env.chan.buffer, caml_failwith);
	    env.stage = 1;
	case 1 :
	    while (env.chan.buffer.length < env.bsize + 20)
		env.chan.buffer.push (env.chan.con.getc ());
	    prg.ctx.ioresult = input_val (env.chan.buffer, caml_failwith);
	    env.chan.con.hilight (false);
	    env.chan.buffer =
		env.chan.buffer.slice (env.bsize + 20, env.chan.buffer.length);
	    return true;
	}
    } catch (e) {
	switch (e.message) {
	case "wait":
	    return false;
	case "eof":
	    chan.con.hilight (false);
	    caml_raise_end_of_file ();
	    return true;
	default:
	    throw e;
	}
    }
}

// Caml name: input_value
// Type:      in_channel -> 'a
function caml_input_value (chan) {
    vm_suspend_before_io (chan.get (0).fd, caml_input_value,
			  {chan : chan.get (0), stage : 0} );
}
