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
//  LEXING                                                                   //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////


function R2BLE(tbl,n) {
    var v1 = (tbl).get ((n) * 2 + 1) ;
    var v2 = (tbl).get ((n) * 2) ;
    var v = ((v1 << 8) | (v2)) ;
    if (v & 0x8000) {
	v = -((v - 1) ^ 0xFFFF) ;
    }
    return v;
}

// Caml name: c_engine
// Type:      lex_tables -> int -> lexbuf -> int
RT["caml_lex_engine"] = function (tbl, start_state, lexbuf) {
    var state, base, backtrk, c;

    state = start_state;
    if (state >= 0) {
	lexbuf.set (4, lexbuf.get (5));
	lexbuf.set (6, lexbuf.get (5));
	lexbuf.set (7, -1);
    } else {
	state = -state - 1;
    }
    while (true) {
	base = R2BLE(tbl.get (0), state);
	if (base < 0) {
	    return (-base-1);
	}
	backtrk = R2BLE(tbl.get (1), state);
	if (backtrk >= 0) {
	    lexbuf.set (6, lexbuf.get (5));
	    lexbuf.set (7, backtrk);
	}
	if (lexbuf.get (5) >= lexbuf.get (2)){
	    if (lexbuf.get (8) == mk_bool (false)){
		return (-state-1);
	    } else {
		c = 256;
	    }
	} else {
	    c = lexbuf.get (1).get (lexbuf.get (5));
	    lexbuf.set (5, lexbuf.get (5) + 1);
	}
	if (R2BLE(tbl.get (4), base + c) == state)
	    state = R2BLE(tbl.get (3), base + c);
	else
	    state = R2BLE(tbl.get (2), state);
	if (state < 0) {
	    lexbuf.set (5, lexbuf.get (6));
	    if (lexbuf.get (7) == -1) {
		this.failwith("lexing: empty token");
	    } else {
		return lexbuf.get (7);
	    }
	}else{
	    if (c == 256)
		lexbuf.set (8, mk_bool (false));
	}
    }
}

function run_mem (pc, mem, curr_pos) {
    var pcp = 0;
    for (;;) {
	var dst, src ;
	
	dst = pc.get (pcp++) ;
	if (dst == 0xff)
	    return ;
	src = pc.get (pcp++) ;
	if (src == 0xff) {
	    mem.get (dst) = curr_pos ;
	} else {
	    mem.set (dst, mem.get (src)) ;
	}
    }
}

function run_tag (pc, mem) {
    var pcp = 0;
    for (;;) {
	var dst, src ;
	
	dst = pc.get (pcp++) ;
	if (dst == 0xff)
	    return ;
	src = pc.get (pcp++) ;
	if (src == 0xff) {
	    mem.set (dst, -1) ;
	} else {
	    mem.set (dst, mem.get (src)) ;
	}
    }
}

// Caml name: c_new_engine
// Type:      lex_tables -> int -> lexbuf -> int
RT["caml_new_lex_engine"] = function(tbl, start_state, lexbuf) {
    var state, base, backtrk, c, pstate ;
    state = start_state;
    if (state >= 0) {
	lexbuf.set (4, -1);
	lexbuf.set (5, -1);
	lexbuf.set (6, -1);
	lexbuf.set (7, -1);
    } else {
	state = -state - 1;
    }
    while (true) {
	base = R2BLE(tbl.get (0), state);
	if (base < 0) {
	    var pc_off = R2BLE(tbl.get (5), state) ;
	    run_tag(tbl.get (10).shift (pc_off), lexbuf.get (9));
	    return (-base-1);
	}
	backtrk = R2BLE(tbl.get (1), state);
	if (backtrk >= 0) {
	    var pc_off =  R2BLE(tbl.get (6), state);
	    run_tag(tbl.get (10).shift (pc_off), lexbuf.get (9));
	    lexbuf.set (6, lexbuf.get (5));
	    lexbuf.set (7, backtrk);
	}
	if (lexbuf.get (5) >= lexbuf.get (2)){
	    if (lexbuf.get (8) == mk_bool (false)){
		return (-state - 1);
	    } else {
		c = 256;
	    }
	} else {
	    c = lexbuf.get (1).get (lexbuf.get (5));
	    lexbuf.set (5, lexbuf.get (5) + 2);
	}
	pstate=state ;
	if (R2BLE(tbl.get (4), base + c) == state)
	    state = R2BLE(tbl.get (3), base + c);
	else
	    state = R2BLE(tbl.get (2), state);
	if (state < 0) {
	    lexbuf.set (5, lexbuf.get (6));
	    if (lexbuf.get (7) == -1) {
		caml_failwith("lexing: empty token");
	    } else {
		return lexbuf.get (7);
	    }
	} else {
	    var base_code = R2BLE(tbl.get (5), pstate) ;
	    var pc_off ;
	    if (R2BLE(tbl.get (9), base_code + c) == pstate)
		pc_off = R2BLE(tbl.get (8), base_code + c) ;
	    else
		pc_off = R2BLE(tbl.get (7), pstate) ;
	    if (pc_off > 0) 
		run_mem(tbl.get (10).shift (pc_off),
			lexbuf.get (9), lexbuf.get (5)) ;
	    if (c == 256) lexbuf.set (8, mk_bool (false));
	}
    }
}
