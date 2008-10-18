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
//  WINDOWS                                                                  //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

function GraphicsWin (vm, div, width, height) {
    this._div = div;
    this._color = 0;
    this._w = width;
    this._h = height;
    this._line_width = 1;
    this.vm = vm;
    this._canvas = document.createElement ("canvas");
    this._canvas.setAttribute ("width", width);
    this._canvas.setAttribute ("height", height);
    this._canvas.setAttribute ("style", "background-color:white; margin:5px; border:1px black solid; margin:2px;");
    div.appendChild (this._canvas);
    
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

METHODS(GraphicsWin).grab_input = function () {
    var win = this;
    this._canvas.style.border = "3px red solid";
    this._canvas.style.margin = "0px";

    this._canvas.onkeydown = function () { win.h_canvas_keydown () } ;
    this._canvas.onkeyup = function () { win.h_canvas_keyup () } ;
}

METHODS(GraphicsWin).release_input = function () {
    var win = this;
    this._canvas.style.border = "1px black solid";
    this._canvas.style.margin = "2px";

    this._canvas.onkeydown = null ;
    this._canvas.onkeyup = null ;
}

METHODS(GraphicsWin).set_mask = function (mask) {
    this.mask = mask;
}

METHODS(GraphicsWin).h_canvas_mouseup = function (e) {
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

METHODS(GraphicsWin).h_canvas_mousedown = function (e) {
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

METHODS(GraphicsWin).h_canvas_keydown = function (e) {
    var can = this._canvas;
    this.st.keypressed = true;
    this.st.key = char_from_event (e);
}

METHODS(GraphicsWin).h_canvas_keyup = function (e) {
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

METHODS(GraphicsWin).h_canvas_mousemove = function (e) {
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

METHODS(GraphicsWin).resize = function (w, h) {
    this._w = w;
    this._h = h;
    this._canvas.width = w;
    this._canvas.height = h;
}

METHODS(GraphicsWin).get_context = function () {
    if (this.ctx == null) {
	var canvas = this._canvas;
	var ctx = canvas.getContext ("2d");
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

METHODS(GraphicsWin).get_canvas = function () {
    return this._canvas;
}
