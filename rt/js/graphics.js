/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

function GraphicsWin (vm, width, height) {
    this._color = 0;
    this._w = width;
    this._h = height;
    this._line_width = 1;
    this.vm = vm;
    this._canvas = document.createElement ("canvas");
    this._canvas.setAttribute ("width", width);
    this._canvas.setAttribute ("height", height);
    this._canvas.setAttribute ("style", "background-color:white; margin:5px; border:1px black solid; margin:2px;");
    
    this.st = { button: false, x: 0, y: 0,keypressed: false, key: 0 };
    var win = this;
    this._canvas.onmouseover = function (e) { win.grab_input (e) } ;
    this._canvas.onmouseout = function (e) { win.release_input (e) } ;
    this._canvas.onmousedown = function (e) { win.h_canvas_mousedown (e) } ;
    this._canvas.onmousemove = function (e) { win.h_canvas_mousemove (e) } ;
    this._canvas.onmouseup = function (e) { win.h_canvas_mouseup (e) } ;
}

#define MASK_BUTTON_DOWN   1
#define MASK_BUTTON_UP     2
#define MASK_KEY_PRESSED   4
#define MASK_MOUSE_MOTION  8
#define MASK_POLL         16

GraphicsWin.prototype.grab_input = function () {
    var win = this;
    this._canvas.style.border = "3px red solid";
    this._canvas.style.margin = "0px";

    this._canvas.onkeydown = function () { win.h_canvas_keydown () } ;
    this._canvas.onkeyup = function () { win.h_canvas_keyup () } ;
}

GraphicsWin.prototype.release_input = function () {
    var win = this;
    this._canvas.style.border = "1px black solid";
    this._canvas.style.margin = "2px";

    this._canvas.onkeydown = null ;
    this._canvas.onkeyup = null ;
}

GraphicsWin.prototype.set_mask = function (mask) {
    this.mask = mask;
}

GraphicsWin.prototype.h_canvas_mouseup = function (e) {
    var can = this._canvas;
    this.st.x =	e.layerX - can.offsetLeft;
    this.st.y =	this._h - (e.layerY - can.offsetTop);
    this.st.button = false;

    if (this.mask & MASK_BUTTON_UP) {
	this.last_answer = {
	    x : this.st.x,
	    y : this.st.y,
	    button : true,
	    keypressed : this.st.keypressed,
	    key : this.st.key
	};
	this.mask = 0;
	this.vm.thread_notify_all (this);
    }
}

GraphicsWin.prototype.h_canvas_mousedown = function (e) {
    var can = this._canvas;
    this.st.x =	e.layerX - can.offsetLeft;
    this.st.y =	this._h - (e.layerY - can.offsetTop);
    this.st.button = true;

    if (this.mask & MASK_BUTTON_DOWN) {
	this.last_answer = {
	    x : this.st.x,
	    y : this.st.y,
	    button : true,
	    keypressed : this.st.keypressed,
	    key : this.st.key
	};
	this.mask = 0;
	this.vm.thread_notify_all (this);
    }
}

GraphicsWin.prototype.h_canvas_keydown = function (e) {
    var can = this._canvas;
    this.st.keypressed = true;
    this.st.key = char_from_event (e);
}

GraphicsWin.prototype.h_canvas_keyup = function (e) {
    var can = this._canvas;
    this.st.keypressed = false;

    if (this.mask & MASK_KEY_PRESSED) {
	this.last_answer = {
	    x : this.st.x,
	    y : this.st.y,
	    button : this.st.button,
	    keypressed : true,
	    key : this.st.key
	};
	this.mask = 0;
	this.vm.thread_notify_all (this);
    }
}

GraphicsWin.prototype.h_canvas_mousemove = function (e) {
    var can = this._canvas;
    this.st.x =	e.layerX - can.offsetLeft;
    this.st.y =	this._h - (e.layerY - can.offsetTop);

    if (this.mask & MASK_MOUSE_MOTION) {
	this.last_answer = {
	    x : this.st.x,
	    y : this.st.y,
	    button : this.st.button,
	    keypressed : this.st.keypressed,
	    key : this.st.key
	};
	this.mask = 0;
	this.vm.thread_notify_all (this);
    }
}

GraphicsWin.prototype.resize = function (w, h) {
    this._w = w;
    this._h = h;
    this._canvas.width = w;
    this._canvas.height = h;
}

GraphicsWin.prototype.get_context = function () {
    if (this.ctx == null) {
	var canvas = this._canvas;
	var ctx = canvas.getContext ("2d");
 	this.font_size = 12;
 	this.font = "Sans";
	ctx.font = this.font_size + "px " + this.font;
	ctx.strokeStyle = ctx.fillStyle = "rgb(" +
	    ((this._color >> 16) & 0xFF) + "," +
	    ((this._color >> 8) & 0xFF) + "," +
	    (this._color & 0xFF) + ")";
	ctx.lineWidth = 1;
	ctx.lineCap = "round";
	ctx.lineJoin = "round";
	ctx.save ();
	ctx.translate (0, this._h);
	ctx.scale (1, -1);
	ctx.translate (0.5, 0.5);
	this.ctx = ctx;
    }
    return this.ctx;
}

GraphicsWin.prototype.get_canvas = function () {
    return this._canvas;
}

// Caml name: raw_open_graph
// Type:      int -> int -> Js.Node.t
function caml_gr_open_graph (width, height) {
    this.graphics_win = new GraphicsWin (this, width, height);
    return this.graphics_win.get_canvas ();
}

// Caml name: raw_close_graph
// Type:      unit -> unit
function caml_gr_close_graph (unit) {
    if (this.graphics_win)
	this.graphics_win.close ();
    return UNIT;
}

// Caml name: set_window_title
// Type:      string -> unit
function caml_gr_set_window_title (t) {
    if (this.graphics_win)
	this.graphics_win.set_title (s);
    return UNIT;
}

// Caml name: resize_window
// Type:      int -> int -> unit
function caml_gr_resize_window (w, h) {
    if (this.graphics_win)
	this.graphics_win.resize (w, h);
    return UNIT;
}

// Caml name: clear_graph
// Type:      unit -> unit
function caml_gr_clear_graph (unit) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	ctx.fillRect (0, 0, this.graphics_win._w, this.graphics_win._h)
    }
    return UNIT;
}

// Caml name: size_x
// Type:      unit -> int
function caml_gr_size_x (unit) {
    if (this.graphics_win)
	return this.graphics_win._w;
    else
	return 0;
}

// Caml name: size_y
// Type:      unit -> int
function caml_gr_size_y (unit) {
    if (this.graphics_win)
	return this.graphics_win._h;
    else
	return 0;
}

// Caml name: display_mode
// Type:      bool -> unit
function caml_gr_display_mode (bool) {
    return UNIT;
}

// Caml name: remember_mode
// Type:      bool -> unit
function caml_gr_remember_mode (bool) {
    return UNIT;
}

// Caml name: synchronize
// Type:      unit -> unit
function caml_gr_synchronize (unit) {
    return UNIT;
}

// Caml name: set_color
// Type:      color -> unit
function caml_gr_set_color (color) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	this.graphics_win._color = color;
	ctx.strokeStyle = ctx.fillStyle = "rgb(" +
	    ((color >> 16) & 0xFF) + "," +
	    ((color >> 8) & 0xFF) + "," +
	    (color & 0xFF) + ")";
    }
    return UNIT;
}

// Caml name: plot
// Type:      int -> int -> unit
function caml_gr_plot (x, y) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	ctx.fillRect (x, y, 1, 1);
	this.graphics_win._x = x;
	this.graphics_win._y = y;
    }
    return UNIT;
}

// Caml name: point_color
// Type:      int -> int -> color
function caml_gr_point_color (x, y) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	var tmp = ctx.getImageData (x,this.graphics_win._h - y - 1,1,1);
	return (tmp.data[0] << 16) | (tmp.data[1] << 8) | tmp.data[2];
    }
    return 0;
}

// Caml name: moveto
// Type:      int -> int -> unit
function caml_gr_moveto (x, y) {
    if (this.graphics_win) {
	this.graphics_win._x = x;
	this.graphics_win._y = y;
    }
    return UNIT;
}

// Caml name: current_x
// Type:      unit -> int
function caml_gr_current_x (unit) {
    if (this.graphics_win)
	return this.graphics_win._x;
    else
	return 0;
}

// Caml name: current_y
// Type:      unit -> int
function caml_gr_current_y (unit) {
    if (this.graphics_win)
	return this.graphics_win._y;
    else
	return 0;
}

// Caml name: lineto
// Type:      int -> int -> unit
function caml_gr_lineto (x, y) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	ctx.beginPath ();
	ctx.moveTo (this.graphics_win._x, this.graphics_win._y);
	ctx.lineTo (x, y);
	ctx.stroke ();
	this.graphics_win._x = x;
	this.graphics_win._y = y;
    }
    return UNIT;
}

// Caml name: raw_draw_rect
// Type:      int -> int -> int -> int -> unit
function caml_gr_draw_rect (x, y, w, h) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	ctx.beginPath ();
	ctx.moveTo (x,y);
	ctx.lineTo (x + w,y);
	ctx.lineTo (x + w,y + h);
	ctx.lineTo (x,y + h);
	ctx.lineTo (x,y);
	ctx.stroke ();
    }
    return UNIT;
}

// Caml name: raw_draw_arc
// Type:      int -> int -> int -> int -> int -> int -> unit
function caml_gr_draw_arc (a) {
    return caml_gr_draw_arc_nat.call (this,a[0],a[1],a[2],a[3],a[4],a[5]);
}
function caml_gr_draw_arc_nat (x,y,rx,ry,a1,a2) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	ctx.beginPath ();
	ctx.arc (x, y, rx, a1 * Math.PI / 180, a2 * Math.PI / 180, false);
	ctx.stroke ();
    }
    return UNIT;
}

// Caml name: raw_set_line_width
// Type:      int -> unit
function caml_gr_set_line_width (lw) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	this.graphics_win._line_width = lw;
	ctx.lineWidth = lw;
    }
    return UNIT;
}

// Caml name: raw_fill_rect
// Type:      int -> int -> int -> int -> unit
function caml_gr_fill_rect (x, y, w, h) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	ctx.fillRect (x, y, w, h);
    }
    return UNIT;
}

// Caml name: fill_poly
// Type:      (int * int) array -> unit
function caml_gr_fill_poly (p) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	ctx.beginPath ();
	if (p.size > 0)
	    ctx.moveTo (p.get (p.size - 1).get (0),
			p.get (p.size - 1).get (1));
	for (var i = 0;i < p.size;i++) {
	    ctx.lineTo (p.get (i).get (0),
			p.get (i).get (1));
	}
	ctx.fill ();
    }
    return UNIT;
}

// Caml name: raw_fill_arc
// Type:      int -> int -> int -> int -> int -> int -> unit
function caml_gr_fill_arc (a) {
    return caml_gr_fill_arc_nat.call (this,a[0],a[1],a[2],a[3],a[4],a[5]);
}
function caml_gr_fill_arc_nat (x,y,rx,ry,a1,a2) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	ctx.beginPath ();
	ctx.arc (x, y, rx, a1 * Math.PI / 180, a2 * Math.PI / 180, false);
	ctx.fill ();
	ctx.stroke ();
    }
    return UNIT;
}

// Caml name: draw_char
// Type:      char -> unit
function caml_gr_draw_char (c) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	ctx.save ();
	ctx.scale (1, -1);
	ctx.translate (0, -this.graphics_win._h);
	ctx.textBaseline = "bottom";
	var m = ctx.measureText (c);
	ctx.fillText (c, this.graphics_win._x, this.graphics_win._y);
	this.graphics_win._x += Math.round (m.width);
	this.graphics_win._y += Math.round (this.graphics_win.font_size);
	ctx.restore ();
    }
    return UNIT;
}

// Caml name: draw_string
// Type:      string -> unit
function caml_gr_draw_string (s) {
    s = string_from_value (s);
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	ctx.save ();
	ctx.scale (1, -1);
	ctx.translate (0, -this.graphics_win._h);
	ctx.textBaseline = "bottom";
	var m = ctx.measureText (s);
	ctx.fillText (s, this.graphics_win._x, this.graphics_win._h - this.graphics_win._y);
	this.graphics_win._x += Math.round (m.width);
	this.graphics_win._y += Math.round (this.graphics_win.font_size);
	ctx.restore ();
    }
    return UNIT;
}

// Caml name: set_font
// Type:      string -> unit
function caml_gr_set_font (s) {
    if (this.graphics_win) {
	this.graphics_win.font = string_from_value (s);
	var ctx = this.graphics_win.get_context ();
	ctx.font = this.graphics_win.font_size + "px " + this.graphics_win.font;
    }
    return UNIT;
}

// Caml name: set_text_size
// Type:      int -> unit
function caml_gr_set_text_size (sz) {
    if (this.graphics_win) {
 	this.graphics_win.font_size = sz;
	var ctx = this.graphics_win.get_context ();
	ctx.font = this.graphics_win.font_size + "px " + this.graphics_win.font;
    }
    return UNIT;
}

// Caml name: text_size
// Type:      string -> int * int
function caml_gr_text_size (s) {
    s = string_from_value (s);
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	var m = ctx.measureText (s);
	var b = mk_block (2, 0);
	b.set (0, Math.round (m.width));
	b.set (1, Math.round (this.graphics_win.font_size));
	return b;
    }
    this.failwith ("caml_gr_text_size");
}

// Caml name: make_image
// Type:      color array array -> image
function caml_gr_make_image (caa) {
    /* warning: no checks */
    var imgdata = new Object ();
    imgdata.height = caa.size;
    imgdata.width = (imgdata.height > 0) ? caa.get (0).size : 0;
    for (var y = 0;y < caa.size;y++) {
	for (var x = 0;x < caa.get (y).size;x++) {
	    imgdata.data[(y * imgdata.width + x) * 4 + 0] =
		(caa.get (y).get (x) >> 16) & 0xFF;
	    imgdata.data[(y * imgdata.width + x) * 4 + 0] =
		(caa.get (y).get (x) >> 8) & 0xFF;
	    imgdata.data[(y * imgdata.width + x) * 4 + 0] =
		caa.get (y).get (x) & 0xFF;
	    imgdata.data[(y * imgdata.width + x) * 4 + 0] = 0;
	}
    }
    var img = mk_block (1, 0);
    img.set (0, imgdata);
    return img;
}

// Caml name: dump_image
// Type:      image -> color array array
function caml_gr_dump_image (img) {
    var caa = mk_block (img.height, 0);
    for (var y = 0;y < img.height;y++) {
	caa.set (y, mk_block (img.width, 0));
	for (var x = 0;x < img.width;x++) {
	    caa.get (y).get (x) =
		(img.data[(y * imgdata.width + x) * 4 + 0] << 16)
		| (img.data[(y * imgdata.width + x) * 4 + 1] << 8)
		| img.data[(y * imgdata.width + x) * 4 + 1];
	}
    }
    return caa;
}

// Caml name: draw_image
// Type:      image -> int -> int -> unit
function caml_gr_draw_image (bimg, x, y) {
    var ctx = this.graphics_win.get_context ();
    var img = bimg.get (0);
    ctx.putImageData (img, x, this.graphics_win._h - y - img.height);
    return UNIT;
}

// Caml name: create_image
// Type:      int -> int -> image
function caml_gr_create_image (w, h) {
    var img = new Object ();
    img.width = w;
    img.height = h;
    img.data = new Array ();
    for (var y = 0;y < h;y++)
	for (var x = 0;x < w;x++) {
	    img.data[(y * w + x) * 4 + 0] = 0;
	    img.data[(y * w + x) * 4 + 1] = 0;
	    img.data[(y * w + x) * 4 + 2] = 0;
	    img.data[(y * w + x) * 4 + 3] = 255;
	}
    var bimg = mk_block (1, CUSTOM_TAG);
    bimg.set (0, img);
    return bimg;
}

// Caml name: blit_image
// Type:      image -> int -> int -> unit
function caml_gr_blit_image (bimg, x, y) {
    var ctx = this.graphics_win.get_context ();
    var img = bimg.get (0);
    img.data = ctx.getImageData (x, this.graphics_win._h - y - img.height,
				 img.width, img.height).data;
    return UNIT;
}

// Caml name: wait_next_event
// Type:      event list -> status
function caml_gr_wait_event (evl) {
    var mask = 0;
    while (is_block (evl)) {
	mask |= 1 << evl.get (0);
	evl = evl.get (1);
    }

    if (mask & MASK_POLL) {
	var st = mk_block (5, 0);
	st.set(0,this.graphics_win.st.x);
	st.set(1,this.graphics_win.st.y);
	st.set(2,this.graphics_win.st.button);
	st.set(3,this.graphics_win.st.keypressed);
	st.set(4, this.graphics_win.st.key);
	return st;
    } else {
	var vm = this;
	function gr_wait_cont () {
	    if (vm.graphics_win.last_answer == null)
		vm.thread_wait (vm.graphics_win, gr_wait_cont);
	    var st = mk_block (5, 0);
	    st.set(0,vm.graphics_win.last_answer.x);
	    st.set(1,vm.graphics_win.last_answer.y);
	    st.set(2,vm.graphics_win.last_answer.button);
	    st.set(3,vm.graphics_win.last_answer.keypressed);
	    st.set(4,vm.graphics_win.last_answer.key);
	    vm.graphics_win.last_answer = null;
	    return st;
	}
	this.graphics_win.set_mask (mask);
	this.thread_wait (this.graphics_win, gr_wait_cont);
    }
}

// Caml name: sound
// Type:      int -> int -> unit
function caml_gr_sound (f, d) {
    return UNIT;
}
