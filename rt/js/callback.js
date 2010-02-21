#define MAGIC_CAML_CALLBACK 987654321

var i_tbl_cb = []
for (i = 0;i <= 255; i++) {
    i_tbl_cb[i] = i_tbl[i];
}
i_tbl_cb[IRAISE] = function (vm, c) {
    if (c.caml_trap_sp == -1) {
	/* reraise */
	throw ([ MAGIC_CAML_CALLBACK , c.accu ]);
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
}

VM.prototype.callback = function (clos, args) {
    var code = mk_block (7, 0);
    var ctx = {
	cur_code : code,
	pc : 0,
	sp : 0,
	caml_trap_sp : -1,
	accu : UNIT,
	stack : new Array (),
 	env : mk_block (0, 0),
	extra_args : 0,
	status : RUN,
	pid : 0
    } ;
    var narg = args.length;
    var octx = this.ctx ;
    this.ctx = ctx;

    ctx.sp -= narg + 4;
    for (i = 0; i < narg; i++)
	ctx.stack[ctx.sp + i] = args[i];
    
    ctx.stack[ctx.sp + narg] = code.shift (4);
    ctx.stack[ctx.sp + narg + 1] = UNIT;
    ctx.stack[ctx.sp + narg + 2] = 0;
    ctx.stack[ctx.sp + narg + 3] = clos;
    code.set (0, IACC);
    code.set (1, narg + 3);
    code.set (2, IAPPLY);
    code.set (3, narg);
    code.set (4, IPOP);
    code.set (5, 1);
    code.set (6, ISTOP);

    try {
	while (ctx.cur_code.get (ctx.pc) != ISTOP) {
	    if (! i_tbl_cb [ctx.cur_code.get (ctx.pc++)] (this, ctx)) {
		this.ctx = octx;
		this.failwith ("blocking functions in callbacks not supported");
	    }
	}
    } catch (e) {
	this.ctx = octx;
	if (e[0] == MAGIC_CAML_CALLBACK) {
	    this.raise (e[1]);
	} else {
	    throw e;
	}
    }
    
    var r = ctx.accu;
    this.ctx = octx;
    return r;
}
