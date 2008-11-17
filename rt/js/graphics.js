/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

#include <font.js>

// Caml name: raw_open_graph
// Type:      int -> int -> Js.Node.t
RT.caml_gr_open_graph = function (width, height) {
    var div = document.createElement ("div");
    this.graphics_win = new GraphicsWin (this, div, width, height);
    return box_abstract (div);
}

// Caml name: raw_close_graph
// Type:      unit -> unit
RT.caml_gr_close_graph = function (unit) {
    if (this.graphics_win)
	this.graphics_win.close ();
    return UNIT;
}

// Caml name: set_window_title
// Type:      string -> unit
RT.caml_gr_set_window_title = function (t) {
    if (this.graphics_win)
	this.graphics_win.set_title (s);
    return UNIT;
}

// Caml name: resize_window
// Type:      int -> int -> unit
RT.caml_gr_resize_window = function (w, h) {
    if (this.graphics_win)
	this.graphics_win.resize (w, h);
    return UNIT;
}

// Caml name: clear_graph
// Type:      unit -> unit
RT.caml_gr_clear_graph = function (unit) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	ctx.fillRect (0, 0, this.graphics_win._w, this.graphics_win._h)
    }
    return UNIT;
}

// Caml name: size_x
// Type:      unit -> int
RT.caml_gr_size_x = function (unit) {
    if (this.graphics_win)
	return this.graphics_win._w;
    else
	return 0;
}

// Caml name: size_y
// Type:      unit -> int
RT.caml_gr_size_y = function (unit) {
    if (this.graphics_win)
	return this.graphics_win._h;
    else
	return 0;
}

// Caml name: display_mode
// Type:      bool -> unit
RT.caml_gr_display_mode = function (bool) {
    return UNIT;
}

// Caml name: remember_mode
// Type:      bool -> unit
RT.caml_gr_remember_mode = function (bool) {
    return UNIT;
}

// Caml name: synchronize
// Type:      unit -> unit
RT.caml_gr_synchronize = function (unit) {
    return UNIT;
}

// Caml name: set_color
// Type:      color -> unit
RT.caml_gr_set_color = function (color) {
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
RT.caml_gr_plot = function (x, y) {
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
RT.caml_gr_point_color = function (x, y) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	var tmp = ctx.getImageData (x,this.graphics_win._h - y - 1,1,1);
	return (tmp.data[0] << 16) | (tmp.data[1] << 8) | tmp.data[2];
    }
    return 0;
}

// Caml name: moveto
// Type:      int -> int -> unit
RT.caml_gr_moveto = function (x, y) {
    if (this.graphics_win) {
	this.graphics_win._x = x;
	this.graphics_win._y = y;
    }
    return UNIT;
}

// Caml name: current_x
// Type:      unit -> int
RT.caml_gr_current_x = function (unit) {
    if (this.graphics_win)
	return this.graphics_win._x;
    else
	return 0;
}

// Caml name: current_y
// Type:      unit -> int
RT.caml_gr_current_y = function (unit) {
    if (this.graphics_win)
	return this.graphics_win._y;
    else
	return 0;
}

// Caml name: lineto
// Type:      int -> int -> unit
RT.caml_gr_lineto = function (x, y) {
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
RT.caml_gr_draw_rect = function (x, y, w, h) {
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
RT.caml_gr_draw_arc = function (a) {
    return RT.caml_gr_draw_arc_nat.call (this,a[0],a[1],a[2],a[3],a[4],a[5]);
}
RT.caml_gr_draw_arc_nat = function (x,y,rx,ry,a1,a2) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	ctx.beginPath ();
	ctx.arc (x, y, rx, a1, a2, false);
	ctx.stroke ();
    }
    return UNIT;
}

// Caml name: raw_set_line_width
// Type:      int -> unit
RT.caml_gr_set_line_width = function (lw) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	this.graphics_win._line_width = lw;
	ctx.lineWidth = lw;
    }
    return UNIT;
}

// Caml name: raw_fill_rect
// Type:      int -> int -> int -> int -> unit
RT.caml_gr_fill_rect = function (x, y, w, h) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	ctx.fillRect (x, y, w, h);
    }
    return UNIT;
}

// Caml name: fill_poly
// Type:      (int * int) array -> unit
RT.caml_gr_fill_poly = function (p) {
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
RT.caml_gr_fill_arc = function (a) {
    return RT.caml_gr_fill_arc_nat (a[0],a[1],a[2],a[3],a[4],a[5]);
}
RT.caml_gr_fill_arc_nat = function (x,y,rx,ry,a1,a2) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	ctx.beginPath ();
	ctx.arc (x, y, rx, a1, a2, false);
	ctx.fill ();
    }
    return UNIT;
}

// Caml name: draw_char
// Type:      char -> unit
RT.caml_gr_draw_char = function (c) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	ctx.drawImage (graphics_font.mapped[c],
		       this.graphics_win._x,
		       this.graphics_win._y);
	this.graphics_win._x += 8;
	this.graphics_win._y += graphics_font.height;
    }
    return UNIT;
}

// Caml name: draw_string
// Type:      string -> unit
RT.caml_gr_draw_string = function (s) {
    if (this.graphics_win) {
	var ctx = this.graphics_win.get_context ();
	for (var i = 0;i < s.size - 1;i++) {
	    var img = graphics_font.mapped[s.get (i)];
	    if (img) {
		for (var y = 0;y < graphics_font.height;y++)
		    for (var x = 0;x < 8;x++)
			if (img[graphics_font.height - 1 - y][x])
			    ctx.fillRect (this.graphics_win._x + x - .5,
					  this.graphics_win._y + y - .5,
					  1, 1);
		this.graphics_win._x += 8;
	    }
	}
	this.graphics_win._y += graphics_font.height;
    }
    return UNIT;
}

// Caml name: set_font
// Type:      string -> unit
RT.caml_gr_set_font = function (s) {
    return UNIT;
}

// Caml name: set_text_size
// Type:      int -> unit
RT.caml_gr_set_text_size = function (sz) {
    return UNIT;
}

// Caml name: text_size
// Type:      string -> int * int
RT.caml_gr_text_size = function (s) {
    var b = mk_block (2, 0);
    b.set (0, 8 * (s.size - 1));
    b.set (1, graphics_font.height);
    return b;
}

// Caml name: make_image
// Type:      color array array -> image
RT.caml_gr_make_image = function (caa) {
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
RT.caml_gr_dump_image = function (img) {
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
RT.caml_gr_draw_image = function (bimg, x, y) {
    var ctx = this.graphics_win.get_context ();
    var img = bimg.get (0);
    ctx.putImageData (img, x, this.graphics_win._h - y - img.height);
    return UNIT;
}

// Caml name: create_image
// Type:      int -> int -> image
RT.caml_gr_create_image = function (w, h) {
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
RT.caml_gr_blit_image = function (bimg, x, y) {
    var ctx = this.graphics_win.get_context ();
    var img = bimg.get (0);
    img.data = ctx.getImageData (x, this.graphics_win._h - y - img.height,
				 img.width, img.height).data;
    return UNIT;
}

// Caml name: wait_next_event
// Type:      event list -> status
RT.caml_gr_wait_event = function (evl) {
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
RT.caml_gr_sound = function (f, d) {
    return UNIT;
}
