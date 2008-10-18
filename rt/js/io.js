///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  Caml Virtual Machine in JavaScript                                       //
//  (C) 2007 Benjamin Canou (Benjamin.Canou@gmail.com)                       //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  This program is free software: you can redistribute it and/or modify     //
//  it under the terms of the GNU General Public License as published by     //
//  the Free Software Foundation, either version 3 of the License, or        //
//  (at your option) any later version.                                      //
//                                                                           //
//  This program is distributed in the hope that it will be useful,          //
//  but WITHOUT ANY WARRANTY; without even the implied warranty of           //
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            //
//  GNU General Public License for more details.                             //
//                                                                           //
//  You should have received a copy of the GNU General Public License        //
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.    //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  INPUT/OUTPUT                                                             //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

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
RT["caml_ml_seek_out"] = function () {
  throw new Error ("caml_ml_seek_out" + " not implemented");
}
// Caml name: pos_out
// Type:      out_channel -> int
RT["caml_ml_pos_out"] = function () {
  throw new Error ("caml_ml_pos_out" + " not implemented");
}
// Caml name: out_channel_length
// Type:      out_channel -> int
RT["caml_ml_channel_size"] = function () {
  throw new Error ("caml_ml_channel_size" + " not implemented");
}
// Caml name: close_out_channel
// Type:      out_channel -> unit
RT["caml_ml_close_channel"] = function () {
  throw new Error ("caml_ml_close_channel" + " not implemented");
}
// Caml name: set_binary_mode_out
// Type:      out_channel -> bool -> unit
RT["caml_ml_set_binary_mode"] = function () {
  throw new Error ("caml_ml_set_binary_mode" + " not implemented");
}
// Caml name: seek_in
// Type:      in_channel -> int -> unit
RT["caml_ml_seek_in"] = function () {
  throw new Error ("caml_ml_seek_in" + " not implemented");
}
// Caml name: pos_in
// Type:      in_channel -> int
RT["caml_ml_pos_in"] = function () {
  throw new Error ("caml_ml_pos_in" + " not implemented");
}
// Caml name: in_channel_length
// Type:      in_channel -> int
RT["caml_ml_channel_size"] = function () {
  throw new Error ("caml_ml_channel_size" + " not implemented");
}
// Caml name: set_binary_mode_in
// Type:      in_channel -> bool -> unit
RT["caml_ml_set_binary_mode"] = function () {
  throw new Error ("caml_ml_set_binary_mode" + " not implemented");}
// Caml name: seek_out
// Type:      out_channel -> int64 -> unit
RT["caml_ml_seek_out_64"] = function () {
  throw new Error ("caml_ml_seek_out_64" + " not implemented");
}
// Caml name: pos_out
// Type:      out_channel -> int64
RT["caml_ml_pos_out_64"] = function () {
  throw new Error ("caml_ml_pos_out_64" + " not implemented");
}
// Caml name: out_channel_length
// Type:      out_channel -> int64
RT["caml_ml_channel_size_64"] = function () {
  throw new Error ("caml_ml_channel_size_64" + " not implemented");
}
// Caml name: seek_in
// Type:      in_channel -> int64 -> unit
RT["caml_ml_seek_in_64"] = function () {
  throw new Error ("caml_ml_seek_in_64" + " not implemented");
}
// Caml name: pos_in
// Type:      in_channel -> int64
RT["caml_ml_pos_in_64"] = function () {
  throw new Error ("caml_ml_pos_in_64" + " not implemented");
}
// Caml name: in_channel_length
// Type:      in_channel -> int64
RT["caml_ml_channel_size_64"] = function () {
  throw new Error ("caml_ml_channel_size_64" + " not implemented");
}
*/

// Caml name: close_in
// Type:      in_channel -> unit
RT["caml_ml_close_channel"] = function (b) {
    b.set (0, null);
    return UNIT;
}

// Caml name: open_descriptor_out
// Type:      int -> out_channel
RT["caml_ml_open_descriptor_out"] = function (fd) {
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
RT["caml_ml_open_descriptor_in"] = function (fd) {
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
RT["caml_channel_descriptor"] = function (chan) {
    return chan.get (0).fd;
}
// Caml name: descr_outchan
// Type:      out_channel -> Unix.file_descr
RT["caml_channel_descriptor"] = function (chan) {
    return chan.get (0).fd;
}

// Caml name: open_desc
// Type:      string -> open_flag list -> int -> int
RT["caml_sys_open"] = function (f, flags, perms) {
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
RT["caml_ml_out_channels_list"] = function (fd) {
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
RT["caml_ml_output_char"] = function (chan, c) {
    chan.get(0).con.puts (String.fromCharCode (c));
    return UNIT;
}

// Caml name: unsafe_output_partial
// Type:      out_channel -> string -> int -> int -> int
RT["caml_ml_output_partial"] = function (chan, s, st, len) {
    var so = "";
    for (var v = 0;v < len;v++) {
	so += String.fromCharCode (s.get (st + v))[0];
    }
    chan.get (0).con.puts (so);
    return len;
}

// Caml name: unsafe_output
// Type:      out_channel -> string -> int -> int -> unit
RT["caml_ml_output"] = function (chan, s, st, len) {
    var so = "";
    for (var v = 0;v < len;v++) {
	so += String.fromCharCode (s.get (st + v))[0];
    }
    chan.get (0).con.puts (so);
    return UNIT;
}

// Caml name: flush_partial
// Type:      out_channel -> bool
RT["caml_ml_flush_partial"] = function (s) {
    return TRUE;
}

// Caml name: flush
// Type:      out_channel -> bool
RT["caml_ml_flush"] = function (s) {
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
RT["caml_ml_input_scan_line"] = function (chan) {
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
RT["caml_ml_input_char"] = function (chan) {
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
RT["caml_ml_input"] = function (chan, buff, st, len) {
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
RT["caml_input_value"] = function (chan) {
    vm_suspend_before_io (chan.get (0).fd, caml_input_value,
			  {chan : chan.get (0), stage : 0} );
}
