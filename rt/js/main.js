/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

#include <compatibility.js>

#define INT32(x) ((x) & (-1))

#include <instructions.js>

#define GOT_RES 43
#define RUN    44
#define WAIT_RES 45
#define SLEEP  46
#define WAIT   47

#define VM_PAUSED  666
#define VM_RUNNING 667
#define VM_WAITING 668

#define RESCHED_INTERVAL 800000
#define TIMEOUT_INTERVAL 50
#define TIMEOUT_STOP     2000

#define MAGIC_CAML_EX   0xEE1664EE
#define MAGIC_CAML_CONT 0xEE1515EE

#define caml_catch(e) if (((e) == MAGIC_CAML_CONT) || ((e) == MAGIC_CAML_EX)) throw (e)

#include <utils.js>
#include <mlvalues.js>
#include <custom.js>
#include <marshall.js>
#include <pervasives.js>
#include <sys.js>
#include <md5.js>

#include <threads.js>
#include <rtjs.js>
#include <jsoo.js>
#include <regexp.js>
#include <camlinternalOO.js>
#include <graphics.js>
#include <lexing.js>

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//  VIRTUAL MACHINE                                                         //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

#include <loader.js>

function VM(url, argv) {
    this.program_name = value_from_string (url);
    this.argv = mk_array_from_js (Array.map (argv, value_from_string));
    this.argv = this.argv.shift(-1);
    this.argv.set (0,this.program_name);
    var program = load_program (url);
    this.syms = program.symbols;
    this.prims = [];
    function undefined_primitive (name){
	return function () {
	    throw (new Error("undefined primitive " + name))
	}
    }
    for (n in this.syms) {
	try {
	    this.prims[n] = eval (this.syms[n]);
	} catch (e) {
	    this.prims[n] = undefined_primitive (this.syms[n]);
	}
    }
    this.data = program.data ;

    /* init state */
    this.status = VM_WAITING;

    /* init scheduler */
    this.max_pid = 1;
    this.ctx = {
	cur_code : program.code,
	pc : 0,
	sp : 0,
	caml_trap_sp : -1,
	accu : UNIT,
	stack : [],
	env : ATOM,
	extra_args : 0,
	status : RUN,
	pid : this.max_pid++
    };
    this.ctx.n_ctx = this.ctx ;
    this.ctx.p_ctx = this.ctx ;
}

#define unbox_code(v) (v.tag == CLOSURE_TAG ? v.get (0):v)

var i_tbl = {
    IACC0: function (vm, c) {
	c.accu = c.stack[c.sp];
	return true;
    },
    IACC1: function (vm, c) {
	c.accu = c.stack[c.sp + 1];
	return true;
    },
    IACC2: function (vm, c) {
	c.accu = c.stack[c.sp + 2];
	return true;
    },
    IACC3: function (vm, c) {
	c.accu = c.stack[c.sp + 3];
	return true;
    },
    IACC4: function (vm, c) {
	c.accu = c.stack[c.sp + 4];
	return true;
    },
    IACC5: function (vm, c) {
	c.accu = c.stack[c.sp + 5];
	return true;
    },
    IACC6: function (vm, c) {
	c.accu = c.stack[c.sp + 6];
	return true;
    },
    IACC7: function (vm, c) {
	c.accu = c.stack[c.sp + 7];
	return true;
    },
    IACC: function (vm, c) {
	c.accu = c.stack[c.sp + c.cur_code.get(c.pc++)];
	return true;
    },
    IPUSH: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	return true;
    },
    IPUSHACC0: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	return true;
    },
    IPUSHACC1: function (vm, c) {
	c.stack[--c.sp] = c.accu;  c.accu = c.stack[c.sp + 1];
	return true;
    },
    IPUSHACC2: function (vm, c) {
	c.stack[--c.sp] = c.accu;  c.accu = c.stack[c.sp + 2];
	return true;
    },
    IPUSHACC3: function (vm, c) {
	c.stack[--c.sp] = c.accu;  c.accu = c.stack[c.sp + 3];
	return true;
    },
    IPUSHACC4: function (vm, c) {
	c.stack[--c.sp] = c.accu;  c.accu = c.stack[c.sp + 4];
	return true;
    },
    IPUSHACC5: function (vm, c) {
	c.stack[--c.sp] = c.accu;  c.accu = c.stack[c.sp + 5];
	return true;
    },
    IPUSHACC6: function (vm, c) {
	c.stack[--c.sp] = c.accu;  c.accu = c.stack[c.sp + 6];
	return true;
    },
    IPUSHACC7: function (vm, c) {
	c.stack[--c.sp] = c.accu;  c.accu = c.stack[c.sp + 7];
	return true;
    },
    IPUSHACC: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	c.accu = c.stack[c.sp + c.cur_code.get(c.pc++)];
	return true;
    },
    IPOP: function (vm, c) {
	c.sp += c.cur_code.get(c.pc++);
	return true;
    },
    IASSIGN: function (vm, c) {
	c.stack[c.sp + c.cur_code.get(c.pc++)] = c.accu;
	c.accu = UNIT;
	return true;
    },
    IENVACC1: function (vm, c) {
	c.accu = c.env.get(1);
	return true;
    },
    IENVACC2: function (vm, c) {
	c.accu = c.env.get(2);
	return true;
    },
    IENVACC3: function (vm, c) {
	c.accu = c.env.get(3);
	return true;
    },
    IENVACC4: function (vm, c) {
	c.accu = c.env.get(4);
	return true;
    },
    IENVACC: function (vm, c) {
	c.accu = c.env.get(c.cur_code.get(c.pc++));
	return true;
    },
    IPUSHENVACC1: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	c.accu = c.env.get(1);
	return true;
    },
    IPUSHENVACC2: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	c.accu = c.env.get(2);
	return true;
    },
    IPUSHENVACC3: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	c.accu = c.env.get(3);
	return true;
    },
    IPUSHENVACC4: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	c.accu = c.env.get(4);
	return true;
    },
    IPUSHENVACC: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	c.accu = c.env.get(c.cur_code.get(c.pc++));
	return true;
    },
    IPUSH_RETADDR: function (vm, c) {
	c.sp -= 3;
	c.stack[c.sp] = c.cur_code.shift (c.pc + c.cur_code.get(c.pc));
	c.stack[c.sp + 1] = c.env;
	c.stack[c.sp + 2] = c.extra_args;
	c.pc++;
	return true;
    },
    IAPPLY: function (vm, c) {
	c.extra_args = c.cur_code.get(c.pc) - 1;
	c.cur_code = unbox_code (c.accu);
	c.pc = 0;
	c.env = c.accu;
	return true;
    },
    IAPPLY1: function (vm, c) {
	var arg1 = c.stack[c.sp];
	c.sp -= 3;
	c.stack[c.sp] = arg1;
	c.stack[c.sp + 1] = c.cur_code.shift (c.pc);
	c.stack[c.sp + 2] = c.env;
	c.stack[c.sp + 3] = c.extra_args;
	c.cur_code = unbox_code (c.accu);
	c.pc = 0;
	c.env = c.accu;
	c.extra_args = 0;
	return true;
    },
    IAPPLY2: function (vm, c) {
	var arg1 = c.stack[c.sp];
	var arg2 = c.stack[c.sp + 1];
	c.sp -= 3;
	c.stack[c.sp] = arg1;
	c.stack[c.sp + 1] = arg2;
	c.stack[c.sp + 2] = c.cur_code.shift (c.pc);
	c.stack[c.sp + 3] = c.env;
	c.stack[c.sp + 4] = c.extra_args;
	c.cur_code = unbox_code (c.accu);
	c.pc = 0;
	c.env = c.accu;
	c.extra_args = 1;
	return true;
    },
    IAPPLY3: function (vm, c) {
	var arg1 = c.stack[c.sp];
	var arg2 = c.stack[c.sp + 1];
	var arg3 = c.stack[c.sp + 2];
	c.sp -= 3;
	c.stack[c.sp] = arg1;
	c.stack[c.sp + 1] = arg2;
	c.stack[c.sp + 2] = arg3;
	c.stack[c.sp + 3] = c.cur_code.shift (c.pc);
	c.stack[c.sp + 4] = c.env;
	c.stack[c.sp + 5] = c.extra_args;
	c.cur_code = unbox_code (c.accu);
	c.pc = 0;
	c.env = c.accu;
	c.extra_args = 2;
	return true;
    },
    IAPPTERM: function (vm, c) {
	var nargs = c.cur_code.get(c.pc++);
	var slotsize = c.cur_code.get(c.pc);
	var newsp = c.sp + slotsize - nargs;
	for (i = nargs - 1; i >= 0; i--)
	    c.stack[newsp + i] = c.stack[c.sp + i];
	c.sp = newsp;
	c.cur_code = unbox_code (c.accu);
	c.pc = 0;
	c.env = c.accu;
	c.extra_args += nargs - 1;
	return true;
    },
    IAPPTERM1: function (vm, c) {
	var arg1 = c.stack[c.sp];
	c.sp += c.cur_code.get (c.pc) - 1;
	c.stack[c.sp] = arg1;
	c.cur_code = unbox_code (c.accu);
	c.pc = 0;
	c.env = c.accu;      
	return true;
    },
    IAPPTERM2: function (vm, c) {
	var arg1 = c.stack[c.sp];
	var arg2 = c.stack[c.sp + 1];
	c.sp += c.cur_code.get (c.pc) - 2;
	c.stack[c.sp] = arg1;
	c.stack[c.sp + 1] = arg2;
	c.cur_code = unbox_code (c.accu);
	c.pc = 0;
	c.env = c.accu;
	c.extra_args += 1;
	return true;
    },
    IAPPTERM3: function (vm, c) {
	var arg1 = c.stack[c.sp];
	var arg2 = c.stack[c.sp + 1];
	var arg3 = c.stack[c.sp + 2];
	c.sp += c.cur_code.get (c.pc) - 3;
	c.stack[c.sp] = arg1;
	c.stack[c.sp + 1] = arg2;
	c.stack[c.sp + 2] = arg3;
	c.cur_code = unbox_code (c.accu);
	c.pc = 0;
	c.env = c.accu;
	c.extra_args += 2;
	return true;
    },
    IRETURN: function (vm, c) {
	c.sp += c.cur_code.get (c.pc++);
	if (c.extra_args > 0) {
	    c.extra_args--;
	    c.cur_code = unbox_code (c.accu);
	    c.pc = 0;
	    c.env = c.accu;
	} else {
	    c.cur_code = unbox_code (c.stack[c.sp]);
	    c.pc = 0;
	    c.env = c.stack[c.sp + 1];
	    c.extra_args = c.stack[c.sp + 2];
	    c.sp += 3;
	}
	return true;
    },
    IRESTART: function (vm, c) {
	var num_args = c.env.size - 2;
	c.sp -= num_args;
	for (var i = 0; i < num_args; i++)
	    c.stack[c.sp + i] = c.env.get(i + 2);
	c.env = c.env.get (1);
	c.extra_args += num_args;
	return true;
    },
    IGRAB: function (vm, c) {
	var required = c.cur_code.get (c.pc++);
	if (c.extra_args >= required) {
	    c.extra_args -= required;
	} else {
	    var num_args;
	    num_args = 1 + c.extra_args;
	    c.accu = mk_block (num_args + 2, CLOSURE_TAG);
	    c.accu.set (0, c.cur_code.shift (c.pc - 3));
	    c.accu.set (1, c.env);
	    for (var i = 0; i < num_args; i++)
		c.accu.set(i + 2, c.stack[c.sp + i]);
	    c.sp += num_args;
	    c.cur_code = unbox_code (c.stack[c.sp]);
	    c.pc = 0;
	    c.env = c.stack[c.sp + 1];
	    c.extra_args = c.stack[c.sp + 2];
	    c.sp += 3;
	}
	return true;
    },
    ICLOSURE: function (vm, c) {
	var nvars = c.cur_code.get (c.pc++);
	if (nvars > 0)
	    c.stack[--c.sp] = c.accu;
	c.accu = mk_block (nvars + 1, CLOSURE_TAG);
	c.accu.set(0, c.cur_code.shift (c.pc + c.cur_code.get(c.pc)));
	c.pc++;
	for (var i = 0; i < nvars; i++)
	    c.accu.set(i + 1,c.stack[c.sp + i]);
	c.sp += nvars;
	return true;
    },
    ICLOSUREREC: function (vm, c) {
	var nfuncs = c.cur_code.get (c.pc++);
	var nvars = c.cur_code.get (c.pc++);
	if (nvars > 0) c.stack[--c.sp] = c.accu;
	c.accu = mk_block (nfuncs * 2 - 1 + nvars, CLOSURE_TAG);
	for (var i = 0;i < nvars;i++)
	    c.accu.set (nfuncs * 2 - 1 + i, c.stack[c.sp++]);
	c.accu.set(0,c.cur_code.shift (c.pc + c.cur_code.get (c.pc)));
	c.stack[--c.sp] = c.accu;
	for (var i = 1; i < nfuncs; i++) {
	    var ofs = c.pc + c.cur_code.get (c.pc + i);
	    c.accu.set (i * 2 - 1, ((i * 2) << 10) | INFIX_TAG);
	    c.accu.set (i * 2, c.cur_code.shift (ofs));
	}
	/* precompute shifts for pointer equality tests */
	var cl = c.accu;
	for (var i = 0;i < nfuncs-1;i++) {
	    cl.next = cl.shift (2);
	    cl.next.prev = cl;
	    c.stack[--c.sp] = cl.next;
	    cl = cl.next;
	}
	c.pc += nfuncs;
	return true;
    },
    IOFFSETCLOSUREM2: function (vm, c) {
	c.accu = c.env.prev;
	return true;
    },
    IOFFSETCLOSURE0: function (vm, c) {
	c.accu = c.env;
	return true;
    },
    IOFFSETCLOSURE2: function (vm, c) {
	c.accu = c.env.next;
	return true;
    },
    IOFFSETCLOSURE: function (vm, c) {
	var ofs = c.cur_code.get (c.pc++);
	c.accu = c.env;
	while (ofs > 0) {
	    c.accu = c.accu.next;
	    ofs -= 2;
	}
	while (ofs < 0) {
	    c.accu = c.accu.prev;
	    ofs += 2;
	}
	return true;
    },
    IPUSHOFFSETCLOSUREM2: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	c.accu = c.env.prev;
	return true;
    },
    IPUSHOFFSETCLOSURE0: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	c.accu = c.env;
	return true;
    },
    IPUSHOFFSETCLOSURE2: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	c.accu = c.env.next;
	return true;
    },
    IPUSHOFFSETCLOSURE: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	var ofs = c.cur_code.get (c.pc++);
	c.accu = c.env;
	while (ofs > 0) {
	    c.accu = c.accu.next;
	    ofs -= 2;
	}
	while (ofs < 0) {
	    c.accu = c.accu.prev;
	    ofs += 2;
	}
	return true;
    },
    IGETGLOBAL: function (vm, c) {
	c.accu = vm.data.get(c.cur_code.get (c.pc++));
	return true;
    },
    IPUSHGETGLOBAL: function (vm, c) {
	c.stack[--c.sp] = c.accu;      
	c.accu = vm.data.get(c.cur_code.get (c.pc++));
	return true;
    },
    IGETGLOBALFIELD: function (vm, c) {
	c.accu = vm.data.get(c.cur_code.get (c.pc++));
	c.accu = c.accu.get(c.cur_code.get (c.pc++));
	return true;
    },
    IPUSHGETGLOBALFIELD: function (vm, c) {
	c.stack[--c.sp] = c.accu;      
	c.accu = vm.data.get(c.cur_code.get (c.pc++));
	c.accu = c.accu.get(c.cur_code.get (c.pc++));
	return true;
    },
    ISETGLOBAL: function (vm, c) {
	vm.data.set(c.cur_code.get (c.pc++), c.accu);
	c.accu = UNIT;
	return true;
    },
    IATOM0: function (vm, c) {
	c.accu = ATOM;
	return true;
    },
    IATOM: function (vm, c) {
	c.accu = mk_block (0,c.cur_code.get (c.pc++));
	return true;
    },
    IPUSHATOM0: function (vm, c) {
	c.stack[--c.sp] = c.accu;      
	c.accu = ATOM;
	return true;
    },
    IPUSHATOM: function (vm, c) {
	c.stack[--c.sp] = c.accu;      
	c.accu = mk_block (0,c.cur_code.get (c.pc++));
	return true;
    },
    IMAKEBLOCK: function (vm, c) {
	var size = c.cur_code.get (c.pc++);
	var tag = c.cur_code.get (c.pc++);
	var block = mk_block (size, tag);
	block.set(0, c.accu);
	for (var i = 1;i < size;i++) {
	    block.set(i, c.stack[c.sp++]);
	}
	c.accu = block;
	return true;
    },
    IMAKEBLOCK1: function (vm, c) {
	var tag = c.cur_code.get (c.pc++);
	var block = mk_block (1, tag);
	block.set(0,c.accu);
	c.accu = block;
	return true;
    },
    IMAKEBLOCK2: function (vm, c) {
	var tag = c.cur_code.get (c.pc++);
	var block = mk_block (2, tag);
	block.set(0,c.accu);
	block.set(1,c.stack[c.sp]);
	c.sp += 1;
	c.accu = block;
	return true;
    },
    IMAKEBLOCK3: function (vm, c) {
	var tag = c.cur_code.get (c.pc++);
	var block = mk_block (3, tag);
	block.set(0,c.accu);
	block.set(1,c.stack[c.sp]);
	block.set(2,c.stack[c.sp + 1]);
	c.sp += 2;
	c.accu = block;
	return true;
    },
    IMAKEFLOATBLOCK: function (vm, c) {
	var size = c.cur_code.get (c.pc++);
	var block = mk_block (size, DOUBLE_ARRAY_TAG);
	block.set(0, unbox_float(c.accu));
	for (var i = 1;i < size;i++)
	    block.set(i, unbox_float(c.stack[c.sp++]));
	c.accu = block;
	return true;
    },
    IGETFIELD0: function (vm, c) {
	c.accu = c.accu.get(0);
	return true;
    },
    IGETFIELD1: function (vm, c) {
	c.accu = c.accu.get(1);
	return true;
    },
    IGETFIELD2: function (vm, c) {
	c.accu = c.accu.get(2);
	return true;
    },
    IGETFIELD3: function (vm, c) {
	c.accu = c.accu.get(3);
	return true;
    },
    IGETFIELD: function (vm, c) {
	c.accu = c.accu.get(c.cur_code.get (c.pc++));
	return true;
    },
    IGETFLOATFIELD: function (vm, c) {
	c.accu = box_float(c.accu.get(c.cur_code.get (c.pc++)));
	return true;
    },
    ISETFIELD0: function (vm, c) {
	c.accu.set(0,c.stack[c.sp++]);  c.accu = UNIT;
	return true;
    },
    ISETFIELD1: function (vm, c) {
	c.accu.set(1,c.stack[c.sp++]);  c.accu = UNIT;
	return true;
    },
    ISETFIELD2: function (vm, c) {
	c.accu.set(2,c.stack[c.sp++]);  c.accu = UNIT;
	return true;
    },
    ISETFIELD3: function (vm, c) {
	c.accu.set(3,c.stack[c.sp++]);  c.accu = UNIT;
	return true;
    },
    ISETFIELD: function (vm, c) {
	c.accu.set(c.cur_code.get (c.pc++),c.stack[c.sp++]);
	c.accu = UNIT;
	return true;
    },
    ISETFLOATFIELD: function (vm, c) {
	c.accu.set(c.cur_code.get (c.pc++),unbox_float(c.stack[c.sp++]));
	c.accu = UNIT;
	return true;
    },
    IVECTLENGTH: function (vm, c) {
	c.accu = c.accu.size;
	return true;
    },
    IGETVECTITEM: function (vm, c) {
	c.accu = c.accu.get(c.stack[c.sp++]);
	return true;
    },
    ISETVECTITEM: function (vm, c) {
	c.accu.set(c.stack[c.sp], c.stack[c.sp + 1]);
	c.sp += 2;
	return true;
    },
    IGETSTRINGCHAR: function (vm, c) {
	c.accu = c.accu.get(c.stack[c.sp++]);
	return true;
    },
    ISETSTRINGCHAR: function (vm, c) {
	c.accu.set(c.stack[c.sp], c.stack[c.sp + 1]);
	c.sp += 2;
	return true;
    },
    IBRANCH: function (vm, c) {
	c.pc += c.cur_code.get (c.pc);
	return true;
    },
    IBRANCHIF: function (vm, c) {
	if (c.accu != 0)
	    c.pc += c.cur_code.get (c.pc);
	else
	    c.pc++;
	return true;
    },
    IBRANCHIFNOT: function (vm, c) {
	if (c.accu == 0)
	    c.pc += c.cur_code.get (c.pc);
	else
	    c.pc++;
	return true;
    },
    ISWITCH: function (vm, c) {
	var sizes = c.cur_code.get (c.pc++);
	if (is_block(c.accu)) {
	    var index = c.accu.tag;
	    c.pc += c.cur_code.get (c.pc + (sizes & 0xFFFF) + index);
	} else {
	    var index = c.accu;
	    c.pc += c.cur_code.get (c.pc + index);
	}
	return true;
    },
    IBOOLNOT: function (vm, c) {
	c.accu = mk_bool (c.accu == 0);
	return true;
    },
    IPUSHTRAP: function (vm, c) {
	c.sp -= 4;
	c.stack[c.sp] =	c.cur_code.shift (c.pc + c.cur_code.get (c.pc));
	c.stack[c.sp + 1] = c.caml_trap_sp;
	c.stack[c.sp + 2] = c.env;
	c.stack[c.sp + 3] = c.extra_args;
	c.caml_trap_sp = c.sp;
	c.pc++;
	return true;
    },
    IPOPTRAP: function (vm, c) {
	c.caml_trap_sp = c.stack[c.sp + 1];
	c.sp += 4;
	return true;
    },
    IRAISE: function (vm, c) {
	if (c.caml_trap_sp == -1) {
	    basic_io_write ('Fatal error: ' +
		      string_from_value (c.accu.get (0).get (0))
		      + (c.accu.size == 2
			 ?(' ' + repr (c.accu.get (1), 1000))
			 :''));
	    vm.thread_kill (c.pid);
	    return false;
	} else {
	    c.sp = c.caml_trap_sp;
	    c.cur_code = unbox_code (c.stack[c.sp]);
	    c.pc = 0;
	    c.caml_trap_sp = c.stack[c.sp + 1];
	    c.env = c.stack[c.sp + 2];
	    c.extra_args = c.stack[c.sp + 3];
	    c.sp += 4;
	    return true;
	}
    },
    ICHECK_SIGNALS: function (vm, c) {
	/* do nothing (yet) */
	return true;
    },
    IJS_CALL1: function (vm, c) {
	try {
	    var r = vm.prims [c.cur_code.get (c.pc)].call (vm, c.accu);
	    c.accu = r;
	    c.pc++;
	} catch (e) {
	    if (e == MAGIC_CAML_CONT) { c.pc++ ; return false; }
	    if (e != MAGIC_CAML_EX) throw e;
	}
	return true;
    },
    IJS_CALL2: function (vm, c) {
	try {
	    var a0 = c.accu;
	    var a1 = c.stack[c.sp];
	    var r = vm.prims [c.cur_code.get (c.pc)].call (vm, a0, a1);
	    c.accu = r;
	    c.sp += 1;
	    c.pc++;
	} catch (e) {
	    if (e == MAGIC_CAML_CONT) { c.pc++ ; c.sp += 1 ; return false; }
	    if (e != MAGIC_CAML_EX) throw e;
	}
	return true;
    },
    IJS_CALL3: function (vm, c) {
	try {
	    var a0 = c.accu;
	    var a1 = c.stack[c.sp];
	    var a2 = c.stack[c.sp + 1];
	    var r =
	    vm.prims [c.cur_code.get (c.pc)].call (vm, a0, a1, a2);
	    c.accu = r;
	    c.sp += 2;
	    c.pc++;
	} catch (e) {
	    if (e == MAGIC_CAML_CONT) { c.pc++ ; c.sp += 2 ; return false; }
	    if (e != MAGIC_CAML_EX) throw e;
	}
	return true;
    },
    IJS_CALL4: function (vm, c) {
	try {
	    var a0 = c.accu;
	    var a1 = c.stack[c.sp];
	    var a2 = c.stack[c.sp + 1];
	    var a3 = c.stack[c.sp + 2];
	    var r =
	    vm.prims [c.cur_code.get (c.pc)].call (vm, a0, a1, a2, a3);
	    c.accu = r;
	    c.sp += 3;
	    c.pc++;
	} catch (e) {
	    if (e == MAGIC_CAML_CONT) { c.pc++ ; c.sp += 3 ; return false; }
	    if (e != MAGIC_CAML_EX) throw e;
	}
	return true;
    },
    IJS_CALL5: function (vm, c) {
	try {
	    var a0 = c.accu;
	    var a1 = c.stack[c.sp];
	    var a2 = c.stack[c.sp + 1];
	    var a3 = c.stack[c.sp + 2];
	    var a4 = c.stack[c.sp + 3];
	    var r =
	    vm.prims [c.cur_code.get (c.pc)].call (vm,a0,a1,a2,a3,a4);
	    c.accu = r;
	    c.sp += 4;
	    c.pc++;
	} catch (e) {
	    if (e == MAGIC_CAML_CONT) { c.pc++ ; c.sp += 4 ; return false; }
	    if (e != MAGIC_CAML_EX) throw e;
	}
	return true;
    },
    IJS_CALLN: function (vm, c) {
	try {
	    var nargs = c.cur_code.get (c.pc++);
	    c.stack[--c.sp] = c.accu;
	    var args = new Array ();
	    for (var i = 0;i < nargs;i++) {
		args[i] = c.stack[c.sp + i];
	    }
	    var r =
	    vm.prims [c.cur_code.get (c.pc)].call (vm, args, nargs);
	    c.accu = r;
	    c.sp += nargs;
	    c.pc++;
	} catch (e) {
	    if (e == MAGIC_CAML_CONT) { c.pc++ ; c.sp += nargs ; return false; }
	    if (e != MAGIC_CAML_EX) throw e;
	}
	return true;
    },
    ICONST0: function (vm, c) {
	c.accu = 0;
	return true;
    },
    ICONST1: function (vm, c) {
	c.accu = 1;
	return true;
    },
    ICONST2: function (vm, c) {
	c.accu = 2;
	return true;
    },
    ICONST3: function (vm, c) {
	c.accu = 3;
	return true;
    },
    ICONSTINT: function (vm, c) {
	c.accu = c.cur_code.get (c.pc);
	c.pc++;
	return true;
    },
    IPUSHCONST0: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	c.accu = 0;
	return true;
    },
    IPUSHCONST1: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	c.accu = 1;
	return true;
    },
    IPUSHCONST2: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	c.accu = 2;
	return true;
    },
    IPUSHCONST3: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	c.accu = 3;
	return true;
    },
    IPUSHCONSTINT: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	c.accu = c.cur_code.get (c.pc);
        c.pc++;
	return true;
    },
    INEGINT: function (vm, c) {
	c.accu = INT32(-c.accu);
	return true;
    },
    IADDINT: function (vm, c) {
	c.accu = INT32 (c.accu + c.stack[c.sp++]);
	return true;
    },
    ISUBINT: function (vm, c) {
	c.accu = INT32 (c.accu - c.stack[c.sp++]);
	return true;
    },
    IMULINT: function (vm, c) {
	c.accu = INT32 (c.accu * c.stack[c.sp++]);
	return true;
    },
    IDIVINT: function (vm, c) {
	c.accu = INT32 (c.accu / c.stack[c.sp++]);
	return true;
    },
    IMODINT: function (vm, c) {
	c.accu = INT32 (c.accu % c.stack[c.sp++]);
	return true;
    },
    IANDINT: function (vm, c) {
	c.accu = c.accu & c.stack[c.sp++];
	return true;
    },
    IORINT: function (vm, c) {
	c.accu = c.accu | c.stack[c.sp++];
	return true;
    },
    IXORINT: function (vm, c) {
	c.accu = c.accu ^ c.stack[c.sp++];
	return true;
    },
    ILSLINT: function (vm, c) {
	c.accu = INT32 (c.accu << c.stack[c.sp++]);
	return true;
    },
    ILSRINT: function (vm, c) {
	c.accu = INT32 (c.accu >>> c.stack[c.sp++]);
	return true;
    },
    IASRINT: function (vm, c) {
	c.accu = INT32 (c.accu >> c.stack[c.sp++]);
	return true;
    },
    IEQ: function (vm, c) {
	c.accu = mk_bool (c.accu == c.stack[c.sp++]);
	return true;
    },
    INEQ: function (vm, c) {
	c.accu = mk_bool (c.accu != c.stack[c.sp++]);
	return true;
    },
    ILTINT: function (vm, c) {
	c.accu = mk_bool (c.accu < c.stack[c.sp++]);
	return true;
    },
    ILEINT: function (vm, c) {
	c.accu = mk_bool (c.accu <= c.stack[c.sp++]);
	return true;
    },
    IGTINT: function (vm, c) {
	c.accu = mk_bool (c.accu > c.stack[c.sp++]);
	return true;
    },
    IGEINT: function (vm, c) {
	c.accu = mk_bool (c.accu >= c.stack[c.sp++]);
	return true;
    },
    IOFFSETINT: function (vm, c) {
	c.accu = INT32(c.accu + c.cur_code.get (c.pc));
	c.pc++;
	return true;
    },
    IOFFSETREF: function (vm, c) {
	c.accu.set (0, INT32(c.accu.get (0) + c.cur_code.get (c.pc)));
	c.accu = UNIT;
	c.pc++;
	return true;
    },
    IISINT: function (vm, c) {
	c.accu = is_long (c.accu);
	return true;
    },
    IGETMETHOD: function (vm, c) {
	c.accu = c.stack[c.sp].get (0).get (c.accu);
	return true;
    },
    IBEQ: function (vm, c) {
	if (c.cur_code.get (c.pc++) == c.accu) {
	    c.pc += c.cur_code.get (c.pc);
	} else {
	    c.pc++;
	}
	return true;
    },
    IBNEQ: function (vm, c) {
	if (c.cur_code.get (c.pc++) != c.accu) {
	    c.pc += c.cur_code.get (c.pc);
	} else {
	    c.pc++;
	}
	return true;
    },
    IBLTINT: function (vm, c) {
	if (c.cur_code.get (c.pc++) < c.accu) {
	    c.pc += c.cur_code.get (c.pc);
	} else {
	    c.pc++;
	}
	return true;
    },
    IBLEINT: function (vm, c) {
	if (c.cur_code.get (c.pc++) <= c.accu) {
	    c.pc += c.cur_code.get (c.pc);
	} else {
	    c.pc++;
	}
	return true;
    },
    IBGTINT: function (vm, c) {
	if (c.cur_code.get (c.pc++) > c.accu) {
	    c.pc += c.cur_code.get (c.pc);
	} else {
	    c.pc++;
	}
	return true;
    },
    IBGEINT: function (vm, c) {
	if (c.cur_code.get (c.pc++) >= c.accu) {
	    c.pc += c.cur_code.get (c.pc);
	} else {
	    c.pc++;
	}
	return true;
    },
    IULTINT: function (vm, c) {
	c.accu = mk_bool (ult (c.accu, c.stack[c.sp++]));
	return true;
    },
    IUGEINT: function (vm, c) {
	c.accu = mk_bool (!ult (c.accu, c.stack[c.sp++]));
	return true;
    },
    IBULTINT: function (vm, c) {
	if (ult(c.cur_code.get (c.pc++), c.accu)) {
	    c.pc += c.cur_code.get (c.pc);
	} else {
	    c.pc++;
	}
	return true;
    },
    IBUGEINT: function (vm, c) {
	if (!ult(c.cur_code.get (c.pc++), c.accu)) {
	    c.pc += c.cur_code.get (c.pc);
	} else {
	    c.pc++;
	}
	return true;
    },
    IGETPUBMET: function (vm, c) {
	c.stack[--c.sp] = c.accu;
	c.accu = c.cur_code.get (c.pc);
	c.pc += 2;
	var meths = c.stack[c.sp].get (0);
	var li = 3;
	var hi = meths.get (0) * 2 + 1;
	while (li < hi) {
	    var mi = ((li + hi) >> 1) | 1;
	    if (c.accu < meths.get (mi))
	 	hi = mi - 2;
	    else
	 	li = mi;
	} 
	c.accu = meths.get (li - 1);
	return true;
    },
    IGETDYNMET: function (vm, c) {
	var meths = c.stack[c.sp].get (0);
	var li = 3;
	var hi = meths.get (0) * 2 + 1;
	while (li < hi) {
	    var mi = ((li + hi) >> 1) | 1;
	    if (c.accu < meths.get (mi))
	 	hi = mi - 2;
	    else
	 	li = mi;
	} 
	c.accu = meths.get (li - 1);
	return true;
    },
    ISTOP: function (vm, c) {
	vm.thread_kill (c.pid);
	return false;
    },
    IEVENT: function (vm, c) {
	/* unsupported instruction */
	vm.thread_kill (c.pid);
	return false;
    },
    IBREAK: function (vm, c) {
	/* unsupported instruction */
	vm.thread_kill (c.pid);
	return false;
    }
};

VM.prototype.raise = function (e) {
    with (this.ctx) {
	accu = e;
	if (caml_trap_sp == -1) {
	    basic_io_write ("Fatal error: " +
			     string_from_value (accu.get(0).get(0))
			     + (accu.size == 2
				?(" " + repr (accu.get (1), 1000))
				:""));
	    throw new Error ("Fatal");
	    this.thread_kill (pid);
	} else {
	    sp = caml_trap_sp;
	    cur_code = unbox_code (stack[sp]);
	    pc = 0;
	    caml_trap_sp = stack[sp + 1];
	    env = stack[sp + 2];
	    extra_args = stack[sp + 3];
	    sp += 4;
	    throw MAGIC_CAML_EX;
	}
    }
}

VM.prototype.run = function () {
    running_vm = this;
    if (this.status != VM_RUNNING) {
	this.status = VM_RUNNING;
	var vm = this;
	function sched_run () {
	    var t1 = (new Date ()).getTime ();
	    for (var i = 0;i < TIMEOUT_INTERVAL;i++) {
		for (var j = 0;j < RESCHED_INTERVAL;j++) {
		    if (vm.ctx == null) break;						
		    var c = vm.ctx;							
		    if (c.status == GOT_RES) {						
			try {								
			    c.accu = c.iocontinuation ();				
			    c.status = RUN;						
			    continue;							
			} catch (e) {							
			    if (e == MAGIC_CAML_CONT) break;				
			    if (e != MAGIC_CAML_EX) throw e;				
			    break;							
			}								
		    } else {								
			if (c.status != RUN)						
			    /* SLEEP, WAIT & WAIT_RES */				
			    break;							
		    }									
		    if (! i_tbl [c.cur_code.get (c.pc++)] (vm, c)) break;               
		}
		var t2 = (new Date ()).getTime ();
		if (!vm.thread_yield ()) {
		    vm.status = VM_WAITING;
		    break;
		}
		if (t2 - t1 > TIMEOUT_STOP) {
		    t1 = t2;
		    break;
		}
	    }
	    if (vm.status != VM_WAITING)
		window.setTimeout (sched_run, 0);
	}
	sched_run ();
    }
    running_vm = null;
}

VM.prototype.thread_notify_all = function (res) {
    var p = this.ctx;
    do {
	if (p.status == WAIT_RES && p.waiting_for == res) {
	    p.status = GOT_RES;
	    break;
	}
	p = p.n_ctx;
    } while (p != this.ctx);
    if (this.status == VM_WAITING)
	this.run ();
}

VM.prototype.thread_notify_one = function (res) {
    var p = this.ctx;
    do {
	if (p.status == WAIT_RES && p.waiting_for == res) {
	    p.status = GOT_RES;
	    break;
	}
	p = p.n_ctx;
    } while (p != this.ctx);
    if (this.status == VM_WAITING)
	this.run ();
}

VM.prototype.thread_wait = function (res, cont) {
    this.ctx.waiting_for = res;
    this.ctx.status = WAIT_RES;
    this.ctx.iocontinuation = cont;
    //    this.thread_yield ();
    throw MAGIC_CAML_CONT;
}

VM.prototype.thread_yield = function () {
    if (this.ctx == null)
	return false;
    var p = this.ctx.n_ctx;
    do {
	if (p.status <= RUN) {
	    this.ctx = p;
	    return true;
	}
	p = p.n_ctx;
    } while (p != this.ctx);
    return false;
}

VM.prototype.thread_new = function (clos, arg1) {
    var t = {
	cur_code : clos.get (0),
	pc : 0,
	sp : 0,
	caml_trap_sp : -1,
	accu : ((arg1 == null) ? UNIT : arg1),
	stack : new Array (),
 	env : clos,
	extra_args : 0,
	status : RUN,
	pid : this.max_pid
    };
    if (this.ctx == null) {
	t.n_ctx = t;
	t.p_ctx = t;
	this.ctx = t;
    } else {
	t.n_ctx = this.ctx.n_ctx;
	t.p_ctx = this.ctx;
	this.ctx.n_ctx.p_ctx = t;
	this.ctx.n_ctx = t;
    }
    return this.max_pid++;
}

VM.prototype.thread_kill = function (pid) {
    var p = this.ctx;
    do {
	if (p.pid == pid) {
	    p.p_ctx.n_ctx = p.n_ctx;
	    p.n_ctx.p_ctx = p.p_ctx;
	    if (this.ctx == p) {
		if (p == p.n_ctx) {
		    this.ctx = null;
		} else {
		    this.ctx = p.n_ctx;
		}
	    }
	    return;
	}
	p = p.n_ctx;
    } while (p != this.ctx);
}

VM.prototype.thread_delay = function (millis) {
    function resume (vm, t) {
	return function () {
	    t.status = RUN;
	    if (vm.status == VM_WAITING)
		vm.run ();
	}
    }
    this.ctx.status = SLEEP;
    window.setTimeout (resume (this, this.ctx), millis);
}

VM.prototype.thread_wakeup = function (pid) {
    var p = this.ctx;
    do {
	if (p.pid == pid) {
	    p.status = RUN;
	    if (this.status == VM_WAITING)
		this.run ();
	}
	p = p.n_ctx;
    } while (p != this.ctx);
}

#include <exceptions.js>
#include <callback.js>

function exec_caml (url) {
    var argv = [];
    for (var i = 0;i < arguments.length - 1;i++)
	argv[i] = arguments[i+1].toString();
    last_vm = new VM(url, argv) ;
    last_vm.run ();
    return last_vm ;
}
