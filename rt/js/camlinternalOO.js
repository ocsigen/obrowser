var label_cache = {} ;

function jsstr(s) {
    if (s.jsstr != null)
	return s.jsstr;
    else {
	s.jsstr = string_from_value (s);
	return s.jsstr; 
    }
}

function plabel_jsstr (s) {
    if (label_cache[s] != null)
	return label_cache[s] ;
    var accu = 0;
    for (var i = 0;i < s.length;i++) {
	accu = (223 * accu) | 0;
	accu = (accu + s.charCodeAt(i)) | 0;
    }
    accu &= 0x7FFFFFFF;
    if (accu > 0x3FFFFFFF)
	accu = -0x7FFFFFFF + accu - 1;

    label_cache[s] = accu ;
    return accu ;
}

function plabel (s) {
    s = jsstr (s); 
    return plabel_jsstr (s);
}

var oo_table_count = 0;

function fit_size (n) {
    if (n <= 2) return n; else return (fit_size ((n + 1) / 2) * 2);
}

oo_new_table = function (pm) {
    oo_table_count++;
    var methods = mk_block (pm.size * 2 + 2, 0);
    methods.set (0, pm.size);
    methods.set (1, fit_size (pm.size) * 32 / 8 - 1);
    for (var i = 2;i < methods.size;i++) {
	methods.set (i, mk_block (0, 0));
    }
    for (var i = 0;i < pm.size;i++) {
	var plab = plabel (pm.get(i));
	methods.set (i * 2 + 3, plab);
    }
    var refnil = mk_block (1, 0);
    refnil.set (0, 0);
    return {
	methods : methods,
	by_name : [],
	by_label : [],
	previous_states : null,
	hidden_meths : [],
	vars : [],
	initializers : refnil,
	size : 2
    };
}

oo_create_table = function (pm) {
    var table = oo_new_table (pm) ;
    for (var i = 0;i < pm.size;i++) {
	var lab = i * 2 + 2;
	table.by_label[lab] = true;
	table.by_name[jsstr(pm.get(i))] = lab;
    }
    return table;
}

function oo_resize (table, size) {
    if (table.methods.size < size)
	table.methods.size = size;    
}

oo_put = function (table, label, element) {
    oo_resize (table, label + 1);
    table.methods.set (label, element);
    return UNIT;
}

var method_count = 0;
var inst_var_count = 0;

oo_new_method = function (table) {
    var index = table.methods.size;
    oo_resize (table, index + 1);
    return index;
}

oo_get_method_label = function (table, name) {
    name = jsstr (name);
    var m = table.by_name[name];
    if (m == null) {
	m = oo_new_method (table);
	table.by_name[name] = m;
	table.by_label[m] = true;
    }
    return m;
}

oo_set_method = function (table, label, element) {
    method_count++;
    if (table.by_label[label]) {
	oo_put (table, label, element);
    } else {
	table.hidden_meths[label] = element;
    }
}

oo_get_method = function (table, label) {
    var m = table.hidden_meths[label];
    if (m)
	return m;
    else
	return table.methods.get (label);
}

function bmap (b, f) {
    var bb = mk_block (b.size, b.tag);
    for (var i = 0;i < b.size;i++)
	bb.set (i, f (b.get (i)));
    return bb;
}

function dup (b) {
    var bb = []
    for (var i in b)
	bb[i] = b[i];
    return bb;
}

oo_narrow = function (table, vars, virt_meths, concr_meths) {
    var virt_meths_labs = bmap (virt_meths, function (m) {return oo_get_method_label (table, m)});
    var concr_meths_labs = bmap (concr_meths, function (m) {return oo_get_method_label (table, m)});
    table.previous_states = {
	by_name : dup (table.by_name),
	by_label : dup (table.by_label),
	hidden_meths : dup (table.hidden_meths),
	tvars : dup (table.vars),
	virt_meths_labs : virt_meths_labs,
	vars : vars,
	next : table.previous_states
    }
    var nvars = [];
    if (vars) {
	for (var i = 0;i < vars.size;i++) {
	    var v = jsstr (vars.get (i));
	    if (table.vars[v])
		nvars[v] = vars[v];
	}
    }
    table.vars = nvars;
    var by_label = [];
    var by_name = [];
    for (var i = 0;i < concr_meths.size;i++) {
	var met = jsstr (concr_meths.get (i));
	var label = concr_meths_labs.get (i);
	by_name[met] = label;
	by_label[label] = (table.by_label[label] != false);
    }
    for (var i = 0 ;i < virt_meths.size;i++) {
	var met = jsstr (virt_meths.get (i));
	var label = virt_meths_labs.get (i);
	by_name[met] = label;
	by_label[label] = false;
    }
    table.by_name = by_name;
    table.by_label = by_label;
    var hidden_meths = [];
    for (var i = 0;i < virt_meths_labs.size;i++) {
	var lab = virt_meths_labs.get (i);
	var m = table.hidden_meths[lab];
	if (m)
	    hidden_meths[lab] = m;
    }
    table.hidden_meths = hidden_meths;
}

oo_widen = function (table) {
    var prev = table.previous_states ;
    table.previous_states = prev.next;
    var vars = dup (prev.tvars);
    for (var i = 0;i < prev.vars.size;i++) {
	var v = jsstr (prev.vars.get (i));
	vars[v] = table.vars[v];
    }
    table.vars = vars;
    table.by_name = prev.by_name;
    table.by_label = prev.by_label;
    table.hidden_meths = dup (prev.hidden_meths);
    for (var i = 0;i < prev.virt_meths_labs.size;i++) {
	var lab = prev.virt_meths_labs[i];
	var m = table.hidden_meths[lab];
	if (m)
	    hidden_meths[lab] = m;
    }
}

oo_new_slot = function (table) {
    return table.size++;
}


oo_new_variable = function (table, name) {
    name = jsstr (name);
    var v = table.vars[name];
    if (v)
	return v;
    else {
	var index = oo_new_slot (table);
	if (name.length != 0)
	    table.vars[name] = index;
	return index;
    }
}

oo_get_variable = function (table, name) {
    return table.vars[jsstr (name)];
}

oo_initializers = function (table) {
    return table.initializers;
}

oo_methods = function (table) {
    return table.methods;
}

oo_size = function (table) {
    return table.size;
}

oo_init_class_raw = function (table) {
    inst_var_count += table.size - 1;
    oo_resize (table, 3 + table.methods.get (1) * 16 / 32);
}
