/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

#define REFILL_BUFF      0
#define LEX_BUFFER       1
#define LEX_BUFFER_LEN   2
#define LEX_ABS_POS      3
#define LEX_START_POS    4
#define LEX_CURR_POS     5
#define LEX_LAST_POS     6
#define LEX_LAST_ACTION  7
#define LEX_EOF_REACHED  8
#define LEX_MEM          9
#define LEX_START_P      10
#define LEX_CURR_P       11

#define LEX_BASE         0
#define LEX_BACKTRK      1
#define LEX_DEFAULT      2
#define LEX_TRANS        3
#define LEX_CHECK        4
#define LEX_BASE_CODE    5
#define LEX_BACKTRK_CODE 6
#define LEX_DEFAULT_CODE 7
#define LEX_TRANS_CODE   8
#define LEX_CHECK_CODE   9
#define LEX_CODE         10

function R2BLE(tbl,n) {
    var v1 = tbl.get ((n) * 2 + 1) & 0xFF ;
    var v2 = tbl.get ((n) * 2) & 0xFF ;
    var v = ((v1 << 8) | (v2)) ;
    if (v & 0x8000) {
	v = v - 0xFFFF - 1 ;
    }
    return v;
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
	    mem.set (dst, curr_pos) ;
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

// Caml name: c_engine
// Type:      lex_tables -> int -> lexbuf -> int
RT["caml_lex_engine"] = function(tbl, start_state, lexbuf) {
    var state, base, backtrk, c ;
    state = start_state;
    if (state >= 0) {
	lexbuf.set (LEX_LAST_POS, (lexbuf.get (LEX_CURR_POS)));
	lexbuf.set (LEX_START_POS, (lexbuf.get (LEX_CURR_POS)));
	lexbuf.set (LEX_LAST_ACTION, -1);
    } else {
	state = -state - 1;
    }
    while (true) {
	base = R2BLE(tbl.get (LEX_BASE), state);
	if (base < 0) {
	    return (-base-1);
	}
	backtrk = R2BLE(tbl.get (LEX_BACKTRK), state);
	
	if (backtrk >= 0) {
	    var pc_off =  R2BLE(tbl.get (LEX_BACKTRK_CODE), state);
	    run_tag(tbl.get (LEX_CODE).shift (pc_off), lexbuf.get (LEX_MEM));
	    lexbuf.set (LEX_LAST_POS, lexbuf.get (LEX_CURR_POS));
	    lexbuf.set (LEX_LAST_ACTION, backtrk);
	}
	if (lexbuf.get (LEX_CURR_POS) >= lexbuf.get (LEX_BUFFER_LEN)){
	    if (lexbuf.get (LEX_EOF_REACHED) == mk_bool (false)){
		return (-state - 1);
	    } else {
		c = 256;
	    }
	} else {
	    c = lexbuf.get (LEX_BUFFER).get (lexbuf.get (LEX_CURR_POS));
	    lexbuf.set (LEX_CURR_POS, lexbuf.get (LEX_CURR_POS) + 1);
	}
	if (R2BLE(tbl.get (LEX_CHECK), base + c) == state)
	    state = R2BLE(tbl.get (LEX_TRANS), base + c);
	else
	    state = R2BLE(tbl.get (LEX_DEFAULT), state);
	if (state < 0) {
	    lexbuf.set (LEX_CURR_POS, lexbuf.get (LEX_LAST_POS));
	    if (lexbuf.get (LEX_LAST_ACTION) == -1) {
		this.failwith("lexing: empty token");
	    } else {
		return lexbuf.get (LEX_LAST_ACTION);
	    }
	} else {
	    if (c == 256) lexbuf.set (LEX_EOF_REACHED, mk_bool (false));
	}
    }
}


// Caml name: c_new_engine
// Type:      lex_tables -> int -> lexbuf -> int
RT["caml_new_lex_engine"] = function(tbl, start_state, lexbuf) {
    var state, base, backtrk, c, pstate ;
    state = start_state;
    if (state >= 0) {
	lexbuf.set (LEX_LAST_POS, (lexbuf.get (LEX_CURR_POS)));
	lexbuf.set (LEX_START_POS, (lexbuf.get (LEX_CURR_POS)));
	lexbuf.set (LEX_LAST_ACTION, -1);
    } else {
	state = -state - 1;
    }
    while (true) {
	base = R2BLE(tbl.get (LEX_BASE), state);
	if (base < 0) {
	    var pc_off = R2BLE(tbl.get (LEX_BASE_CODE), state) ;
	    run_tag(tbl.get (LEX_CODE).shift (pc_off), lexbuf.get (LEX_MEM));
	    return (-base-1);
	}
	backtrk = R2BLE(tbl.get (LEX_BACKTRK), state);
	if (backtrk >= 0) {
	    var pc_off =  R2BLE(tbl.get (LEX_BACKTRK_CODE), state);
	    run_tag(tbl.get (LEX_CODE).shift (pc_off), lexbuf.get (LEX_MEM));
	    lexbuf.set (LEX_LAST_POS, lexbuf.get (LEX_CURR_POS));
	    lexbuf.set (LEX_LAST_ACTION, backtrk);
	}
	if (lexbuf.get (LEX_CURR_POS) >= lexbuf.get (LEX_BUFFER_LEN)){
	    if (lexbuf.get (LEX_EOF_REACHED) == mk_bool (false)){
		return (-state - 1);
	    } else {
		c = 256;
	    }
	} else {
	    c = lexbuf.get (LEX_BUFFER).get (lexbuf.get (LEX_CURR_POS));
	    lexbuf.set (LEX_CURR_POS, lexbuf.get (LEX_CURR_POS) + 1);
	}
	pstate=state ;
	if (R2BLE(tbl.get (LEX_CHECK), base + c) == state)
	    state = R2BLE(tbl.get (LEX_TRANS), base + c);
	else
	    state = R2BLE(tbl.get (LEX_DEFAULT), state);
	if (state < 0) {
	    lexbuf.set (LEX_CURR_POS, lexbuf.get (LEX_LAST_POS));
	    if (lexbuf.get (LEX_LAST_ACTION) == -1) {
		this.failwith("lexing: empty token");
	    } else {
		return lexbuf.get (LEX_LAST_ACTION);
	    }
	} else {
	    var base_code = R2BLE(tbl.get (LEX_BASE_CODE), pstate) ;
	    var pc_off ;
	    if (R2BLE(tbl.get (LEX_CHECK_CODE), base_code + c) == pstate)
		pc_off = R2BLE(tbl.get (LEX_TRANS_CODE), base_code + c) ;
	    else
		pc_off = R2BLE(tbl.get (LEX_DEFAULT_CODE), pstate) ;
	    if (pc_off > 0) 
		run_mem(tbl.get (LEX_CODE).shift (pc_off),
			lexbuf.get (LEX_MEM), lexbuf.get (LEX_CURR_POS)) ;
	    if (c == 256) lexbuf.set (LEX_EOF_REACHED, mk_bool (false));
	}
    }
}

