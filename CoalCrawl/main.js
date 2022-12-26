(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$document = _Browser_document;
var $author$project$Main$Restart = function (a) {
	return {$: 'Restart', a: a};
};
var $author$project$Main$SetWidthOverHeight = function (a) {
	return {$: 'SetWidthOverHeight', a: a};
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$random$Random$Generate = function (a) {
	return {$: 'Generate', a: a};
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0.a;
		return $elm$random$Random$Generate(
			A2($elm$random$Random$map, func, generator));
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			$elm$random$Random$Generate(
				A2($elm$random$Random$map, tagger, generator)));
	});
var $elm$browser$Browser$Dom$getViewport = _Browser_withWindow(_Browser_getViewport);
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
				var lo = _v0.a;
				var hi = _v0.b;
				var range = (hi - lo) + 1;
				if (!((range - 1) & range)) {
					return _Utils_Tuple2(
						(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
						$elm$random$Random$next(seed0));
				} else {
					var threshhold = (((-range) >>> 0) % range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var x = $elm$random$Random$peel(seed);
							var seedN = $elm$random$Random$next(seed);
							if (_Utils_cmp(x, threshhold) < 0) {
								var $temp$seed = seedN;
								seed = $temp$seed;
								continue accountForBias;
							} else {
								return _Utils_Tuple2((x % range) + lo, seedN);
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var $elm$random$Random$map3 = F4(
	function (func, _v0, _v1, _v2) {
		var genA = _v0.a;
		var genB = _v1.a;
		var genC = _v2.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v3 = genA(seed0);
				var a = _v3.a;
				var seed1 = _v3.b;
				var _v4 = genB(seed1);
				var b = _v4.a;
				var seed2 = _v4.b;
				var _v5 = genC(seed2);
				var c = _v5.a;
				var seed3 = _v5.b;
				return _Utils_Tuple2(
					A3(func, a, b, c),
					seed3);
			});
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $elm$random$Random$independentSeed = $elm$random$Random$Generator(
	function (seed0) {
		var makeIndependentSeed = F3(
			function (state, b, c) {
				return $elm$random$Random$next(
					A2($elm$random$Random$Seed, state, (1 | (b ^ c)) >>> 0));
			});
		var gen = A2($elm$random$Random$int, 0, 4294967295);
		return A2(
			$elm$random$Random$step,
			A4($elm$random$Random$map3, makeIndependentSeed, gen, gen, gen),
			seed0);
	});
var $author$project$View$Tab$DetailTab = {$: 'DetailTab'};
var $author$project$Data$Item$Coal = {$: 'Coal'};
var $author$project$Data$Block$EntityBlock = function (a) {
	return {$: 'EntityBlock', a: a};
};
var $author$project$Data$Block$FloorBlock = function (a) {
	return {$: 'FloorBlock', a: a};
};
var $author$project$Data$Floor$Ground = {$: 'Ground'};
var $author$project$Data$Actor$Helper = function (a) {
	return {$: 'Helper', a: a};
};
var $author$project$Data$Actor$Path = {$: 'Path'};
var $author$project$Data$Floor$RailwayTrack = {$: 'RailwayTrack'};
var $author$project$Data$Actor$Train = function (a) {
	return {$: 'Train', a: a};
};
var $author$project$Data$Entity$Vein = function (a) {
	return {$: 'Vein', a: a};
};
var $author$project$Data$Entity$Wall = {$: 'Wall'};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $author$project$Data$World$empty = {actors: $elm$core$Dict$empty, entities: $elm$core$Dict$empty, floor: $elm$core$Dict$empty, items: $elm$core$Dict$empty, nextId: 0};
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Data$World$removeEntity = F2(
	function (pos, world) {
		return _Utils_update(
			world,
			{
				actors: function () {
					var _v0 = A2($elm$core$Dict$get, pos, world.entities);
					if ((_v0.$ === 'Just') && (_v0.a.$ === 'Actor')) {
						var id = _v0.a.a;
						return A2($elm$core$Dict$remove, id, world.actors);
					} else {
						return world.actors;
					}
				}(),
				entities: A2($elm$core$Dict$remove, pos, world.entities),
				floor: A3(
					$elm$core$Dict$update,
					pos,
					function (maybe) {
						return $elm$core$Maybe$Just(
							A2($elm$core$Maybe$withDefault, $author$project$Data$Floor$Ground, maybe));
					},
					world.floor)
			});
	});
var $author$project$Data$World$insertEntityAt = F3(
	function (pos, entity, world) {
		return function (w) {
			return _Utils_update(
				w,
				{
					entities: A3($elm$core$Dict$insert, pos, entity, world.entities)
				});
		}(
			A2($author$project$Data$World$removeEntity, pos, world));
	});
var $author$project$Data$World$insertFloorAt = F3(
	function (pos, floor, world) {
		return _Utils_update(
			world,
			{
				floor: A3($elm$core$Dict$insert, pos, floor, world.floor)
			});
	});
var $author$project$Data$World$insert = F3(
	function (pos, block, world) {
		if (block.$ === 'FloorBlock') {
			var floor = block.a;
			return A3($author$project$Data$World$insertFloorAt, pos, floor, world);
		} else {
			var entity = block.a;
			return A3($author$project$Data$World$insertEntityAt, pos, entity, world);
		}
	});
var $author$project$Data$World$fromList = A2(
	$elm$core$List$foldl,
	function (_v0) {
		var p = _v0.a;
		var b = _v0.b;
		return A2($author$project$Data$World$insert, p, b);
	},
	$author$project$Data$World$empty);
var $author$project$Data$Player$fromPos = function (pos) {
	return {item: $elm$core$Maybe$Nothing, pos: pos, riding: $elm$core$Maybe$Nothing, targetPos: $elm$core$Maybe$Nothing};
};
var $author$project$Data$Train$fromPos = function (pos) {
	return {
		dir: _Utils_Tuple2(0, -1),
		items: _List_Nil,
		moving: false,
		pos: pos,
		tracks: 0
	};
};
var $author$project$Data$Entity$Actor = function (a) {
	return {$: 'Actor', a: a};
};
var $author$project$Data$World$insertActorAt = F3(
	function (pos, actor, world) {
		return function (w) {
			return _Utils_update(
				w,
				{
					actors: A3(
						$elm$core$Dict$insert,
						world.nextId,
						_Utils_Tuple2(pos, actor),
						world.actors),
					entities: A3(
						$elm$core$Dict$insert,
						pos,
						$author$project$Data$Entity$Actor(world.nextId),
						world.entities),
					nextId: world.nextId + 1
				});
		}(
			A2($author$project$Data$World$removeEntity, pos, world));
	});
var $author$project$Data$World$insertActor = F2(
	function (actor, pos) {
		return A2($author$project$Data$World$insertActorAt, pos, actor);
	});
var $author$project$Data$Game$new = function () {
	var walls = A2(
		$elm$core$List$map,
		function (p) {
			return _Utils_Tuple2(
				p,
				$author$project$Data$Block$EntityBlock($author$project$Data$Entity$Wall));
		},
		A2(
			$elm$core$List$cons,
			_Utils_Tuple2(0, -1),
			A2(
				$elm$core$List$concatMap,
				function (y) {
					return A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(x, y);
						},
						_List_fromArray(
							[-1, 1]));
				},
				A2($elm$core$List$range, 0, 2))));
	var train = _Utils_Tuple2(0, 2);
	var tracks = A2(
		$elm$core$List$map,
		function (i) {
			return _Utils_Tuple2(
				_Utils_Tuple2(0, i),
				$author$project$Data$Block$FloorBlock($author$project$Data$Floor$RailwayTrack));
		},
		A2($elm$core$List$range, 0, 1));
	var player = _Utils_Tuple2(0, 3);
	var coals = _List_fromArray(
		[
			_Utils_Tuple2(0 - 1, 3),
			_Utils_Tuple2(0 + 1, 3)
		]);
	return {
		improvements: _List_Nil,
		player: $author$project$Data$Player$fromPos(player),
		selected: player,
		trainId: 0,
		world: A3(
			$author$project$Data$World$insertActor,
			$author$project$Data$Actor$Helper($author$project$Data$Actor$Path),
			_Utils_Tuple2(0, 4),
			A3(
				$author$project$Data$World$insertActor,
				$author$project$Data$Actor$Train(
					$author$project$Data$Train$fromPos(train)),
				train,
				$author$project$Data$World$fromList(
					_Utils_ap(
						_List_fromArray(
							[
								_Utils_Tuple2(
								train,
								$author$project$Data$Block$FloorBlock($author$project$Data$Floor$RailwayTrack)),
								_Utils_Tuple2(
								player,
								$author$project$Data$Block$FloorBlock($author$project$Data$Floor$Ground))
							]),
						_Utils_ap(
							tracks,
							_Utils_ap(
								walls,
								A2(
									$elm$core$List$map,
									function (pos) {
										return _Utils_Tuple2(
											pos,
											$author$project$Data$Block$EntityBlock(
												$author$project$Data$Entity$Vein($author$project$Data$Item$Coal)));
									},
									coals)))))))
	};
}();
var $author$project$Data$Modal$TitleModal = function (a) {
	return {$: 'TitleModal', a: a};
};
var $author$project$Data$Modal$title = $author$project$Data$Modal$TitleModal(
	{animationFrame: 0});
var $author$project$Main$restart = F2(
	function (widthOverHeight, seed) {
		return function (game) {
			return {
				building: $elm$core$Maybe$Nothing,
				camera: game.player.pos,
				game: game,
				level: 1,
				modal: $elm$core$Maybe$Just($author$project$Data$Modal$title),
				seed: seed,
				sidebarTab: $elm$core$Maybe$Just($author$project$View$Tab$DetailTab),
				tickInterval: 200,
				volume: 25,
				widthOverHeight: widthOverHeight,
				zoomPercent: 25
			};
		}($author$project$Data$Game$new);
	});
var $author$project$Main$init = function (_v0) {
	return _Utils_Tuple2(
		A2(
			$author$project$Main$restart,
			1.4,
			$elm$random$Random$initialSeed(42)),
		$elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					A2($elm$random$Random$generate, $author$project$Main$Restart, $elm$random$Random$independentSeed),
					A2(
					$elm$core$Task$perform,
					$author$project$Main$SetWidthOverHeight,
					A2(
						$elm$core$Task$map,
						function (_v1) {
							var viewport = _v1.viewport;
							return viewport.width / viewport.height;
						},
						$elm$browser$Browser$Dom$getViewport))
				])));
};
var $author$project$Main$TimePassed = {$: 'TimePassed'};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 'Every', a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {processes: processes, taggers: taggers};
	});
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 'Nothing') {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.processes;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(_Utils_Tuple0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.taggers);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $elm$browser$Browser$Events$Window = {$: 'Window'};
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onResize = function (func) {
	return A3(
		$elm$browser$Browser$Events$on,
		$elm$browser$Browser$Events$Window,
		'resize',
		A2(
			$elm$json$Json$Decode$field,
			'target',
			A3(
				$elm$json$Json$Decode$map2,
				func,
				A2($elm$json$Json$Decode$field, 'innerWidth', $elm$json$Json$Decode$int),
				A2($elm$json$Json$Decode$field, 'innerHeight', $elm$json$Json$Decode$int))));
};
var $author$project$Main$subscriptions = function (model) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				A2(
				$elm$time$Time$every,
				model.tickInterval,
				function (_v0) {
					return $author$project$Main$TimePassed;
				}),
				$elm$browser$Browser$Events$onResize(
				F2(
					function (w, h) {
						return $author$project$Main$SetWidthOverHeight(w / h);
					}))
			]));
};
var $author$project$Data$Sound$Error = {$: 'Error'};
var $author$project$Data$Effect$PlaySound = function (a) {
	return {$: 'PlaySound', a: a};
};
var $author$project$Data$Game$addImprovement = F2(
	function (improvement, game) {
		return _Utils_update(
			game,
			{
				improvements: A2($elm$core$List$cons, improvement, game.improvements)
			});
	});
var $author$project$Data$Game$addImprovementTo = F2(
	function (game, improvement) {
		return A2($author$project$Data$Game$addImprovement, improvement, game);
	});
var $author$project$Data$Sound$Build = {$: 'Build'};
var $author$project$Data$Sound$Destruct = {$: 'Destruct'};
var $author$project$Data$Sound$MovingTrain = {$: 'MovingTrain'};
var $author$project$Data$Sound$PickUp = {$: 'PickUp'};
var $author$project$Data$Sound$Unload = {$: 'Unload'};
var $author$project$Data$Sound$asList = _List_fromArray(
	[$author$project$Data$Sound$Build, $author$project$Data$Sound$PickUp, $author$project$Data$Sound$Unload, $author$project$Data$Sound$MovingTrain, $author$project$Data$Sound$Destruct, $author$project$Data$Sound$Error]);
var $author$project$Data$World$getActor = F2(
	function (id, world) {
		return A2($elm$core$Dict$get, id, world.actors);
	});
var $author$project$Data$Game$getTrain = function (game) {
	var _v0 = A2($author$project$Data$World$getActor, game.trainId, game.world);
	if ((_v0.$ === 'Just') && (_v0.a.b.$ === 'Train')) {
		var _v1 = _v0.a;
		var train = _v1.b.a;
		return train;
	} else {
		return $author$project$Data$Train$fromPos(
			_Utils_Tuple2(0, 0));
	}
};
var $author$project$Data$World$getFloor = F2(
	function (pos, world) {
		return A2($elm$core$Dict$get, pos, world.floor);
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Data$World$getBlock = F2(
	function (pos, world) {
		var _v0 = A2($elm$core$Dict$get, pos, world.entities);
		if (_v0.$ === 'Just') {
			var a = _v0.a;
			return $elm$core$Maybe$Just(
				$author$project$Data$Block$EntityBlock(a));
		} else {
			return A2(
				$elm$core$Maybe$map,
				$author$project$Data$Block$FloorBlock,
				A2($author$project$Data$World$getFloor, pos, world));
		}
	});
var $author$project$Data$World$isFloor = F2(
	function (pos, world) {
		var _v0 = A2($author$project$Data$World$getBlock, pos, world);
		if ((_v0.$ === 'Just') && (_v0.a.$ === 'FloorBlock')) {
			return true;
		} else {
			return false;
		}
	});
var $author$project$ListBag$count = F2(
	function (a, list) {
		var rec = function (input) {
			rec:
			while (true) {
				if (input.b) {
					var _v1 = input.a;
					var a1 = _v1.a;
					var n1 = _v1.b;
					var tail = input.b;
					if (_Utils_eq(a1, a)) {
						return n1;
					} else {
						var $temp$input = tail;
						input = $temp$input;
						continue rec;
					}
				} else {
					return 0;
				}
			}
		};
		return rec(list);
	});
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$ListBag$remove = F3(
	function (n, a, list) {
		var rec = F2(
			function (input, output) {
				rec:
				while (true) {
					if (input.b) {
						var _v1 = input.a;
						var a1 = _v1.a;
						var n1 = _v1.b;
						var tail = input.b;
						if (_Utils_eq(a, a1)) {
							return (_Utils_cmp(n1, n) > 0) ? A2(
								$elm$core$List$cons,
								_Utils_Tuple2(a1, n1 - n),
								_Utils_ap(tail, output)) : _Utils_ap(tail, output);
						} else {
							var $temp$input = tail,
								$temp$output = A2(
								$elm$core$List$cons,
								_Utils_Tuple2(a1, n1),
								output);
							input = $temp$input;
							output = $temp$output;
							continue rec;
						}
					} else {
						return output;
					}
				}
			});
		return A2(rec, list, _List_Nil);
	});
var $author$project$Data$Train$removeItem = F3(
	function (n, item, train) {
		return A2(
			$elm$core$Maybe$map,
			function (t) {
				return (_Utils_eq(item, $author$project$Data$Item$Coal) && (!A2($author$project$ListBag$count, $author$project$Data$Item$Coal, t.items))) ? _Utils_update(
					t,
					{moving: false}) : t;
			},
			(_Utils_cmp(
				A2($author$project$ListBag$count, item, train.items),
				n) > -1) ? $elm$core$Maybe$Just(
				_Utils_update(
					train,
					{
						items: A3($author$project$ListBag$remove, n, item, train.items)
					})) : $elm$core$Maybe$Nothing);
	});
var $elm$core$Tuple$mapSecond = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var $author$project$Data$World$updateActor = F3(
	function (id, fun, world) {
		return function (actors) {
			return _Utils_update(
				world,
				{actors: actors});
		}(
			A3(
				$elm$core$Dict$update,
				id,
				$elm$core$Maybe$map(
					$elm$core$Tuple$mapSecond(fun)),
				world.actors));
	});
var $author$project$Data$World$setActor = F2(
	function (id, actor) {
		return A2(
			$author$project$Data$World$updateActor,
			id,
			function (_v0) {
				return actor;
			});
	});
var $author$project$Data$Game$setTrain = F2(
	function (train, game) {
		return _Utils_update(
			game,
			{
				world: A3(
					$author$project$Data$World$setActor,
					game.trainId,
					$author$project$Data$Actor$Train(train),
					game.world)
			});
	});
var $author$project$Data$Game$buildActor = F4(
	function (pos, _v0, actor, game) {
		var item = _v0.a;
		var cost = _v0.b;
		return A2($author$project$Data$World$isFloor, pos, game.world) ? A2(
			$elm$core$Maybe$map,
			function (g) {
				return _Utils_Tuple2(
					g,
					_List_fromArray(
						[
							$author$project$Data$Effect$PlaySound($author$project$Data$Sound$Build)
						]));
			},
			A2(
				$elm$core$Maybe$map,
				function (train) {
					return A2(
						$author$project$Data$Game$setTrain,
						train,
						_Utils_update(
							game,
							{
								world: A3($author$project$Data$World$insertActorAt, pos, actor, game.world)
							}));
				},
				A3(
					$author$project$Data$Train$removeItem,
					cost,
					item,
					$author$project$Data$Game$getTrain(game)))) : $elm$core$Maybe$Nothing;
	});
var $author$project$Data$Game$buildBlock = F4(
	function (pos, _v0, block, game) {
		var item = _v0.a;
		var cost = _v0.b;
		return function () {
			if (block.$ === 'FloorBlock') {
				return _Utils_eq(
					A2($author$project$Data$World$getFloor, pos, game.world),
					$elm$core$Maybe$Just($author$project$Data$Floor$Ground));
			} else {
				return A2($author$project$Data$World$isFloor, pos, game.world);
			}
		}() ? $elm$core$Maybe$Just(
			function (g) {
				return _Utils_Tuple2(
					g,
					_List_fromArray(
						[
							$author$project$Data$Effect$PlaySound($author$project$Data$Sound$Build)
						]));
			}(
				A2(
					$elm$core$Maybe$withDefault,
					game,
					A2(
						$elm$core$Maybe$map,
						function (train) {
							return A2(
								$author$project$Data$Game$setTrain,
								train,
								_Utils_update(
									game,
									{
										world: A3($author$project$Data$World$insert, pos, block, game.world)
									}));
						},
						A3(
							$author$project$Data$Train$removeItem,
							cost,
							item,
							$author$project$Data$Game$getTrain(game)))))) : $elm$core$Maybe$Nothing;
	});
var $elm$random$Random$constant = function (value) {
	return $elm$random$Random$Generator(
		function (seed) {
			return _Utils_Tuple2(value, seed);
		});
};
var $author$project$Data$Item$Iron = {$: 'Iron'};
var $author$project$Data$Train$coalNeeded = function (train) {
	var _v0 = train.pos;
	var y = _v0.b;
	return y * 2;
};
var $author$project$ListBag$insert = F3(
	function (n, a, list) {
		var rec = F2(
			function (input, output) {
				rec:
				while (true) {
					if (input.b) {
						var _v1 = input.a;
						var a1 = _v1.a;
						var n1 = _v1.b;
						var tail = input.b;
						if (_Utils_eq(a1, a)) {
							return A2(
								$elm$core$List$cons,
								_Utils_Tuple2(a1, n1 + n),
								_Utils_ap(tail, output));
						} else {
							var $temp$input = tail,
								$temp$output = A2(
								$elm$core$List$cons,
								_Utils_Tuple2(a1, n1),
								output);
							input = $temp$input;
							output = $temp$output;
							continue rec;
						}
					} else {
						return A2(
							$elm$core$List$cons,
							_Utils_Tuple2(a, n),
							output);
					}
				}
			});
		return A2(rec, list, _List_Nil);
	});
var $author$project$ListBag$fromList = A2(
	$elm$core$List$foldl,
	function (a) {
		return A2($author$project$ListBag$insert, 1, a);
	},
	_List_Nil);
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$ListBag$union = function (l1) {
	return A2(
		$elm$core$List$foldl,
		function (_v0) {
			var a = _v0.a;
			var n = _v0.b;
			return A2($author$project$ListBag$insert, n, a);
		},
		l1);
};
var $author$project$Data$Train$addAll = F2(
	function (list, train) {
		return function (t) {
			return (A2($elm$core$List$member, $author$project$Data$Item$Coal, list) && ((_Utils_cmp(
				A2($author$project$ListBag$count, $author$project$Data$Item$Coal, t.items),
				$author$project$Data$Train$coalNeeded(t)) > -1) || (t.tracks > 0))) ? _Utils_update(
				t,
				{moving: true}) : t;
		}(
			_Utils_update(
				train,
				{
					items: A2(
						$author$project$ListBag$union,
						$author$project$ListBag$fromList(list),
						train.items)
				}));
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Data$World$getItem = F2(
	function (pos, world) {
		return A2(
			$elm$core$Maybe$andThen,
			$elm$core$List$head,
			A2($elm$core$Dict$get, pos, world.items));
	});
var $author$project$Data$World$get = F2(
	function (pos, world) {
		return A2(
			$elm$core$Maybe$map,
			function (block) {
				return _Utils_Tuple2(
					block,
					A2($author$project$Data$World$getItem, pos, world));
			},
			A2($author$project$Data$World$getBlock, pos, world));
	});
var $author$project$Data$World$insertAllItemsAt = F3(
	function (pos, items, world) {
		return _Utils_update(
			world,
			{
				floor: A3(
					$elm$core$Dict$update,
					pos,
					function (floor) {
						return $elm$core$Maybe$Just(
							A2($elm$core$Maybe$withDefault, $author$project$Data$Floor$Ground, floor));
					},
					world.floor),
				items: A3(
					$elm$core$Dict$update,
					pos,
					function (maybe) {
						return $elm$core$Maybe$Just(
							_Utils_ap(
								items,
								A2($elm$core$Maybe$withDefault, _List_Nil, maybe)));
					},
					world.items)
			});
	});
var $author$project$Data$World$insertAllItems = F2(
	function (items, pos) {
		return A2($author$project$Data$World$insertAllItemsAt, pos, items);
	});
var $author$project$Data$World$removeFloor = F2(
	function (pos, world) {
		return _Utils_update(
			world,
			{
				floor: A3($elm$core$Dict$insert, pos, $author$project$Data$Floor$Ground, world.floor)
			});
	});
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $author$project$Data$Game$setWorld = F2(
	function (world, game) {
		return _Utils_update(
			game,
			{world: world});
	});
var $author$project$Data$Game$setWorldOf = F2(
	function (game, world) {
		return A2($author$project$Data$Game$setWorld, world, game);
	});
var $author$project$Config$wagonCost = 8;
var $author$project$Data$Game$destroyBlock = F2(
	function (pos, game) {
		return A2(
			$elm$core$Maybe$map,
			function (g) {
				return _Utils_Tuple2(
					g,
					_List_fromArray(
						[
							$author$project$Data$Effect$PlaySound($author$project$Data$Sound$Destruct)
						]));
			},
			A2(
				$elm$core$Maybe$andThen,
				function (block) {
					_v0$2:
					while (true) {
						if (block.$ === 'EntityBlock') {
							if (block.a.$ === 'Actor') {
								var id = block.a.a;
								var _v1 = A2($author$project$Data$World$getActor, id, game.world);
								if ((_v1.$ === 'Just') && (_v1.a.b.$ === 'Minecart')) {
									var _v2 = _v1.a;
									var minecart = _v2.b.a;
									return $elm$core$Maybe$Just(
										function (g) {
											return A2(
												$author$project$Data$Game$setWorldOf,
												g,
												A3(
													$author$project$Data$World$insertAllItems,
													minecart.storage.items,
													pos,
													A2($author$project$Data$World$removeEntity, pos, g.world)));
										}(
											function (train) {
												return A2($author$project$Data$Game$setTrain, train, game);
											}(
												A2(
													$author$project$Data$Train$addAll,
													A2($elm$core$List$repeat, $author$project$Config$wagonCost, $author$project$Data$Item$Iron),
													$author$project$Data$Game$getTrain(game)))));
								} else {
									return $elm$core$Maybe$Nothing;
								}
							} else {
								break _v0$2;
							}
						} else {
							if (block.a.$ === 'Track') {
								var _v3 = block.a;
								return $elm$core$Maybe$Just(
									function (world) {
										return _Utils_update(
											game,
											{world: world});
									}(
										A2($author$project$Data$World$removeFloor, pos, game.world)));
							} else {
								break _v0$2;
							}
						}
					}
					return $elm$core$Maybe$Nothing;
				},
				A2(
					$elm$core$Maybe$map,
					$elm$core$Tuple$first,
					A2($author$project$Data$World$get, pos, game.world))));
	});
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Main$loadSound = _Platform_outgoingPort(
	'loadSound',
	function ($) {
		var a = $.a;
		var b = $.b;
		return A2(
			$elm$json$Json$Encode$list,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					$elm$json$Json$Encode$string(a),
					$elm$json$Json$Encode$string(b)
				]));
	});
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Data$Effect$map = function (fun) {
	return $elm$random$Random$map(
		function (_v0) {
			var a = _v0.a;
			var l = _v0.b;
			return A2(
				$elm$core$Tuple$mapSecond,
				$elm$core$Basics$append(l),
				fun(a));
		});
};
var $elm$random$Random$andThen = F2(
	function (callback, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed) {
				var _v1 = genA(seed);
				var result = _v1.a;
				var newSeed = _v1.b;
				var _v2 = callback(result);
				var genB = _v2.a;
				return genB(newSeed);
			});
	});
var $author$project$Config$tracksPerTrip = 8;
var $author$project$Generation$baseProbability = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return _List_fromArray(
		[
			_Utils_Tuple2(
			(_Utils_cmp(y, $author$project$Config$tracksPerTrip) > 0) ? 0.5 : 0,
			_Utils_Tuple2(x, y - 1)),
			_Utils_Tuple2(
			0.5,
			_Utils_Tuple2(x, y + 1)),
			_Utils_Tuple2(
			0.5,
			_Utils_Tuple2(x - 1, y)),
			_Utils_Tuple2(
			0.5,
			_Utils_Tuple2(x + 1, y))
		]);
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$random$Random$float = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var seed1 = $elm$random$Random$next(seed0);
				var range = $elm$core$Basics$abs(b - a);
				var n1 = $elm$random$Random$peel(seed1);
				var n0 = $elm$random$Random$peel(seed0);
				var lo = (134217727 & n1) * 1.0;
				var hi = (67108863 & n0) * 1.0;
				var val = ((hi * 134217728.0) + lo) / 9007199254740992.0;
				var scaled = (val * range) + a;
				return _Utils_Tuple2(
					scaled,
					$elm$random$Random$next(seed1));
			});
	});
var $author$project$Generation$generateAt = F3(
	function (pos, fun, world) {
		return A2(
			$elm$random$Random$map,
			function (updateAt) {
				return function (maybe) {
					return _Utils_eq(maybe, $elm$core$Maybe$Nothing) ? A2(updateAt, pos, world) : world;
				}(
					A2($author$project$Data$World$get, pos, world));
			},
			fun);
	});
var $author$project$Data$World$insertEntity = F2(
	function (entity, pos) {
		return A2($author$project$Data$World$insertEntityAt, pos, entity);
	});
var $author$project$Generation$generateContent = F3(
	function (pos, content, dict) {
		return A3(
			$elm$core$List$foldl,
			function (_v0) {
				var prob = _v0.a;
				var p = _v0.b;
				return $elm$random$Random$andThen(
					function (w) {
						return A2(
							$elm$random$Random$andThen,
							function (_float) {
								return A3(
									$author$project$Generation$generateAt,
									p,
									(_Utils_cmp(_float, prob) < 0) ? content : $elm$random$Random$constant(
										$author$project$Data$World$insertEntity($author$project$Data$Entity$Wall)),
									w);
							},
							A2($elm$random$Random$float, 0, 1));
					});
			},
			$elm$random$Random$constant(dict),
			$author$project$Generation$baseProbability(pos));
	});
var $author$project$Data$World$insertItemAt = F2(
	function (pos, item) {
		return A2(
			$author$project$Data$World$insertAllItemsAt,
			pos,
			_List_fromArray(
				[item]));
	});
var $author$project$Data$Actor$Cave = function (a) {
	return {$: 'Cave', a: a};
};
var $author$project$Data$Actor$CoalCave = {$: 'CoalCave'};
var $author$project$Data$Actor$CollapsedCave = {$: 'CollapsedCave'};
var $author$project$Data$Actor$IronCave = {$: 'IronCave'};
var $author$project$Data$Actor$LavaCave = {$: 'LavaCave'};
var $author$project$Data$Actor$WaterCave = {$: 'WaterCave'};
var $elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						$elm$core$List$cons,
						sep,
						A2($elm$core$List$cons, x, rest));
				});
			var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
			return A2($elm$core$List$cons, hd, spersed);
		}
	});
var $elm$core$Basics$pow = _Basics_pow;
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $elm$random$Random$getByWeight = F3(
	function (_v0, others, countdown) {
		getByWeight:
		while (true) {
			var weight = _v0.a;
			var value = _v0.b;
			if (!others.b) {
				return value;
			} else {
				var second = others.a;
				var otherOthers = others.b;
				if (_Utils_cmp(
					countdown,
					$elm$core$Basics$abs(weight)) < 1) {
					return value;
				} else {
					var $temp$_v0 = second,
						$temp$others = otherOthers,
						$temp$countdown = countdown - $elm$core$Basics$abs(weight);
					_v0 = $temp$_v0;
					others = $temp$others;
					countdown = $temp$countdown;
					continue getByWeight;
				}
			}
		}
	});
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $elm$random$Random$weighted = F2(
	function (first, others) {
		var normalize = function (_v0) {
			var weight = _v0.a;
			return $elm$core$Basics$abs(weight);
		};
		var total = normalize(first) + $elm$core$List$sum(
			A2($elm$core$List$map, normalize, others));
		return A2(
			$elm$random$Random$map,
			A2($elm$random$Random$getByWeight, first, others),
			A2($elm$random$Random$float, 0, total));
	});
var $author$project$Generation$Wall$wallGenerator = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var content = function (i) {
		return $elm$core$List$reverse(
			A2(
				$elm$core$List$take,
				i + 1,
				_Utils_ap(
					_List_fromArray(
						[
							$author$project$Data$World$insertActor(
							$author$project$Data$Actor$Helper(
								$author$project$Data$Actor$Cave($author$project$Data$Actor$CoalCave))),
							$author$project$Data$World$insertEntity(
							$author$project$Data$Entity$Vein($author$project$Data$Item$Coal))
						]),
					A2(
						$elm$core$List$intersperse,
						$author$project$Data$World$insertEntity($author$project$Data$Entity$Wall),
						_List_fromArray(
							[
								$author$project$Data$World$insertActor(
								$author$project$Data$Actor$Helper(
									$author$project$Data$Actor$Cave($author$project$Data$Actor$IronCave))),
								$author$project$Data$World$insertActor(
								$author$project$Data$Actor$Helper(
									$author$project$Data$Actor$Cave($author$project$Data$Actor$WaterCave))),
								$author$project$Data$World$insertActor(
								$author$project$Data$Actor$Helper(
									$author$project$Data$Actor$Cave($author$project$Data$Actor$CollapsedCave))),
								$author$project$Data$World$insertActor(
								$author$project$Data$Actor$Helper(
									$author$project$Data$Actor$Cave($author$project$Data$Actor$LavaCave)))
							])))));
	};
	return function (list) {
		if (!list.b) {
			return $elm$random$Random$constant(
				$author$project$Data$World$insertEntity($author$project$Data$Entity$Wall));
		} else {
			var head = list.a;
			var tail = list.b;
			return A2(
				$elm$random$Random$weighted,
				_Utils_Tuple2(1, head),
				A2(
					$elm$core$List$indexedMap,
					F2(
						function (i, fun) {
							return _Utils_Tuple2(
								1 / A2($elm$core$Basics$pow, 2, i + 1),
								fun);
						}),
					tail));
		}
	}(
		function (_int) {
			return (_Utils_cmp(y, _int) < 1) ? _List_Nil : content(_int);
		}(((y / $author$project$Config$tracksPerTrip) | 0) + 1));
};
var $author$project$Generation$mine = F2(
	function (_v0, world) {
		var x = _v0.a;
		var y = _v0.b;
		var _v1 = A2(
			$author$project$Data$World$get,
			_Utils_Tuple2(x, y),
			world);
		if ((_v1.$ === 'Just') && (_v1.a.a.$ === 'EntityBlock')) {
			var _v2 = _v1.a;
			var entity = _v2.a.a;
			return A2(
				$elm$core$Maybe$withDefault,
				$elm$random$Random$constant(world),
				A2(
					$elm$core$Maybe$map,
					function (items) {
						return A3(
							$author$project$Generation$generateContent,
							_Utils_Tuple2(x, y),
							$author$project$Generation$Wall$wallGenerator(
								_Utils_Tuple2(x, y)),
							function () {
								if (!items.b) {
									return $elm$core$Basics$identity;
								} else {
									var item = items.a;
									return A2(
										$author$project$Data$World$insertItemAt,
										_Utils_Tuple2(x, y),
										item);
								}
							}()(
								A2(
									$author$project$Data$World$removeEntity,
									_Utils_Tuple2(x, y),
									world)));
					},
					function () {
						switch (entity.$) {
							case 'Vein':
								var item = entity.a;
								return $elm$core$Maybe$Just(
									_List_fromArray(
										[item]));
							case 'Actor':
								return $elm$core$Maybe$Nothing;
							default:
								return $elm$core$Maybe$Just(_List_Nil);
						}
					}()));
		} else {
			return $elm$random$Random$constant(world);
		}
	});
var $author$project$Data$Behavior$Player$canMoveTo = F2(
	function (game, p) {
		var _v0 = A2($author$project$Data$World$get, p, game.world);
		_v0$4:
		while (true) {
			if (_v0.$ === 'Just') {
				if (_v0.a.a.$ === 'FloorBlock') {
					var _v1 = _v0.a;
					return true;
				} else {
					switch (_v0.a.a.a.$) {
						case 'Actor':
							var _v2 = _v0.a;
							var id = _v2.a.a.a;
							var _v3 = A2($author$project$Data$World$getActor, id, game.world);
							_v3$3:
							while (true) {
								if (_v3.$ === 'Just') {
									switch (_v3.a.b.$) {
										case 'Minecart':
											var _v4 = _v3.a;
											return true;
										case 'MovingWater':
											var _v5 = _v3.a;
											return true;
										case 'Train':
											var _v6 = _v3.a;
											return true;
										default:
											break _v3$3;
									}
								} else {
									break _v3$3;
								}
							}
							return false;
						case 'Water':
							var _v7 = _v0.a;
							var _v8 = _v7.a.a;
							return true;
						case 'Vein':
							var _v9 = _v0.a;
							return true;
						default:
							break _v0$4;
					}
				}
			} else {
				break _v0$4;
			}
		}
		return false;
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$core$List$sortBy = _List_sortBy;
var $krisajenkins$elm_astar$AStar$Generalised$cheapestOpen = F2(
	function (costFn, model) {
		return A2(
			$elm$core$Maybe$map,
			$elm$core$Tuple$first,
			$elm$core$List$head(
				A2(
					$elm$core$List$sortBy,
					$elm$core$Tuple$second,
					A2(
						$elm$core$List$filterMap,
						function (position) {
							var _v0 = A2($elm$core$Dict$get, position, model.costs);
							if (_v0.$ === 'Nothing') {
								return $elm$core$Maybe$Nothing;
							} else {
								var cost = _v0.a;
								return $elm$core$Maybe$Just(
									_Utils_Tuple2(
										position,
										cost + costFn(position)));
							}
						},
						$elm$core$Set$toList(model.openSet)))));
	});
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2($elm$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});
var $elm$core$Set$diff = F2(
	function (_v0, _v1) {
		var dict1 = _v0.a;
		var dict2 = _v1.a;
		return $elm$core$Set$Set_elm_builtin(
			A2($elm$core$Dict$diff, dict1, dict2));
	});
var $elm$core$Set$foldl = F3(
	function (func, initialState, _v0) {
		var dict = _v0.a;
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (key, _v1, state) {
					return A2(func, key, state);
				}),
			initialState,
			dict);
	});
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Elm$JsArray$push = _JsArray_push;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Elm$JsArray$singleton = _JsArray_singleton;
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$insertTailInTree = F4(
	function (shift, index, tail, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		if (_Utils_cmp(
			pos,
			$elm$core$Elm$JsArray$length(tree)) > -1) {
			if (shift === 5) {
				return A2(
					$elm$core$Elm$JsArray$push,
					$elm$core$Array$Leaf(tail),
					tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, $elm$core$Elm$JsArray$empty));
				return A2($elm$core$Elm$JsArray$push, newSub, tree);
			}
		} else {
			var value = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (value.$ === 'SubTree') {
				var subTree = value.a;
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, subTree));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4(
						$elm$core$Array$insertTailInTree,
						shift - $elm$core$Array$shiftStep,
						index,
						tail,
						$elm$core$Elm$JsArray$singleton(value)));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$unsafeReplaceTail = F2(
	function (newTail, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var originalTailLen = $elm$core$Elm$JsArray$length(tail);
		var newTailLen = $elm$core$Elm$JsArray$length(newTail);
		var newArrayLen = len + (newTailLen - originalTailLen);
		if (_Utils_eq(newTailLen, $elm$core$Array$branchFactor)) {
			var overflow = _Utils_cmp(newArrayLen >>> $elm$core$Array$shiftStep, 1 << startShift) > 0;
			if (overflow) {
				var newShift = startShift + $elm$core$Array$shiftStep;
				var newTree = A4(
					$elm$core$Array$insertTailInTree,
					newShift,
					len,
					newTail,
					$elm$core$Elm$JsArray$singleton(
						$elm$core$Array$SubTree(tree)));
				return A4($elm$core$Array$Array_elm_builtin, newArrayLen, newShift, newTree, $elm$core$Elm$JsArray$empty);
			} else {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					newArrayLen,
					startShift,
					A4($elm$core$Array$insertTailInTree, startShift, len, newTail, tree),
					$elm$core$Elm$JsArray$empty);
			}
		} else {
			return A4($elm$core$Array$Array_elm_builtin, newArrayLen, startShift, tree, newTail);
		}
	});
var $elm$core$Array$push = F2(
	function (a, array) {
		var tail = array.d;
		return A2(
			$elm$core$Array$unsafeReplaceTail,
			A2($elm$core$Elm$JsArray$push, a, tail),
			array);
	});
var $krisajenkins$elm_astar$AStar$Generalised$reconstructPath = F2(
	function (cameFrom, goal) {
		var _v0 = A2($elm$core$Dict$get, goal, cameFrom);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Array$empty;
		} else {
			var next = _v0.a;
			return A2(
				$elm$core$Array$push,
				goal,
				A2($krisajenkins$elm_astar$AStar$Generalised$reconstructPath, cameFrom, next));
		}
	});
var $elm$core$Set$remove = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A2($elm$core$Dict$remove, key, dict));
	});
var $elm$core$Set$union = F2(
	function (_v0, _v1) {
		var dict1 = _v0.a;
		var dict2 = _v1.a;
		return $elm$core$Set$Set_elm_builtin(
			A2($elm$core$Dict$union, dict1, dict2));
	});
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $krisajenkins$elm_astar$AStar$Generalised$updateCost = F3(
	function (current, neighbour, model) {
		var newCameFrom = A3($elm$core$Dict$insert, neighbour, current, model.cameFrom);
		var distanceTo = $elm$core$Array$length(
			A2($krisajenkins$elm_astar$AStar$Generalised$reconstructPath, newCameFrom, neighbour));
		var newCosts = A3($elm$core$Dict$insert, neighbour, distanceTo, model.costs);
		var newModel = _Utils_update(
			model,
			{cameFrom: newCameFrom, costs: newCosts});
		var _v0 = A2($elm$core$Dict$get, neighbour, model.costs);
		if (_v0.$ === 'Nothing') {
			return newModel;
		} else {
			var previousDistance = _v0.a;
			return (_Utils_cmp(distanceTo, previousDistance) < 0) ? newModel : model;
		}
	});
var $krisajenkins$elm_astar$AStar$Generalised$astar = F4(
	function (costFn, moveFn, goal, model) {
		astar:
		while (true) {
			var _v0 = A2(
				$krisajenkins$elm_astar$AStar$Generalised$cheapestOpen,
				costFn(goal),
				model);
			if (_v0.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var current = _v0.a;
				if (_Utils_eq(current, goal)) {
					return $elm$core$Maybe$Just(
						A2($krisajenkins$elm_astar$AStar$Generalised$reconstructPath, model.cameFrom, goal));
				} else {
					var neighbours = moveFn(current);
					var modelPopped = _Utils_update(
						model,
						{
							evaluated: A2($elm$core$Set$insert, current, model.evaluated),
							openSet: A2($elm$core$Set$remove, current, model.openSet)
						});
					var newNeighbours = A2($elm$core$Set$diff, neighbours, modelPopped.evaluated);
					var modelWithNeighbours = _Utils_update(
						modelPopped,
						{
							openSet: A2($elm$core$Set$union, modelPopped.openSet, newNeighbours)
						});
					var modelWithCosts = A3(
						$elm$core$Set$foldl,
						$krisajenkins$elm_astar$AStar$Generalised$updateCost(current),
						modelWithNeighbours,
						newNeighbours);
					var $temp$costFn = costFn,
						$temp$moveFn = moveFn,
						$temp$goal = goal,
						$temp$model = modelWithCosts;
					costFn = $temp$costFn;
					moveFn = $temp$moveFn;
					goal = $temp$goal;
					model = $temp$model;
					continue astar;
				}
			}
		}
	});
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $elm$core$Dict$singleton = F2(
	function (key, value) {
		return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
	});
var $elm$core$Set$singleton = function (key) {
	return $elm$core$Set$Set_elm_builtin(
		A2($elm$core$Dict$singleton, key, _Utils_Tuple0));
};
var $krisajenkins$elm_astar$AStar$Generalised$initialModel = function (start) {
	return {
		cameFrom: $elm$core$Dict$empty,
		costs: A2($elm$core$Dict$singleton, start, 0),
		evaluated: $elm$core$Set$empty,
		openSet: $elm$core$Set$singleton(start)
	};
};
var $krisajenkins$elm_astar$AStar$Generalised$findPath = F4(
	function (costFn, moveFn, start, end) {
		return A2(
			$elm$core$Maybe$map,
			$elm$core$Array$toList,
			A4(
				$krisajenkins$elm_astar$AStar$Generalised$astar,
				costFn,
				moveFn,
				end,
				$krisajenkins$elm_astar$AStar$Generalised$initialModel(start)));
	});
var $krisajenkins$elm_astar$AStar$findPath = $krisajenkins$elm_astar$AStar$Generalised$findPath;
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $author$project$Data$Effect$genWithNone = $elm$random$Random$map(
	function (a) {
		return _Utils_Tuple2(a, _List_Nil);
	});
var $author$project$Data$Player$moveTo = F2(
	function (pos, player) {
		return _Utils_update(
			player,
			{pos: pos});
	});
var $author$project$Data$Position$neighbors = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return _List_fromArray(
		[
			_Utils_Tuple2(x, y + 1),
			_Utils_Tuple2(x - 1, y),
			_Utils_Tuple2(x, y - 1),
			_Utils_Tuple2(x + 1, y)
		]);
};
var $author$project$Data$Actor$Minecart = function (a) {
	return {$: 'Minecart', a: a};
};
var $author$project$Data$Actor$MovingWater = function (a) {
	return {$: 'MovingWater', a: a};
};
var $author$project$Data$Momentum$new = function (pos) {
	return {
		momentum: $elm$core$Maybe$Just(pos)
	};
};
var $author$project$Data$Position$plus = F2(
	function (_v0, _v1) {
		var x1 = _v0.a;
		var y1 = _v0.b;
		var x2 = _v1.a;
		var y2 = _v1.b;
		return _Utils_Tuple2(x1 + x2, y1 + y2);
	});
var $author$project$Data$Position$vecTo = F2(
	function (p, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return A2(
			$author$project$Data$Position$plus,
			p,
			_Utils_Tuple2(-x, -y));
	});
var $author$project$Data$Momentum$fromPoints = function (args) {
	return $author$project$Data$Momentum$new(
		A2($author$project$Data$Position$vecTo, args.to, args.from));
};
var $author$project$Data$World$push = F2(
	function (_v0, world) {
		var from = _v0.from;
		var pos = _v0.pos;
		return A2(
			$elm$core$Maybe$andThen,
			function (block) {
				if (block.$ === 'EntityBlock') {
					var entity = block.a;
					switch (entity.$) {
						case 'Actor':
							var id = entity.a;
							var _v3 = A2(
								$elm$core$Maybe$map,
								$elm$core$Tuple$second,
								A2($author$project$Data$World$getActor, id, world));
							_v3$2:
							while (true) {
								if (_v3.$ === 'Just') {
									switch (_v3.a.$) {
										case 'Minecart':
											var minecart = _v3.a.a;
											return $elm$core$Maybe$Just(
												A3(
													$author$project$Data$World$setActor,
													id,
													$author$project$Data$Actor$Minecart(
														_Utils_update(
															minecart,
															{
																movedFrom: $elm$core$Maybe$Just(from)
															})),
													world));
										case 'MovingWater':
											return $elm$core$Maybe$Just(
												A3(
													$author$project$Data$World$setActor,
													id,
													$author$project$Data$Actor$MovingWater(
														$author$project$Data$Momentum$fromPoints(
															{from: from, to: pos})),
													world));
										default:
											break _v3$2;
									}
								} else {
									break _v3$2;
								}
							}
							return $elm$core$Maybe$Nothing;
						case 'Water':
							return $elm$core$Maybe$Just(
								A3(
									$author$project$Data$World$insertActor,
									$author$project$Data$Actor$MovingWater(
										$author$project$Data$Momentum$fromPoints(
											{from: from, to: pos})),
									pos,
									A2($author$project$Data$World$removeEntity, pos, world)));
						default:
							return $elm$core$Maybe$Nothing;
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			A2($author$project$Data$World$getBlock, pos, world));
	});
var $krisajenkins$elm_astar$AStar$straightLineCost = F2(
	function (_v0, _v1) {
		var x1 = _v0.a;
		var y1 = _v0.b;
		var x2 = _v1.a;
		var y2 = _v1.b;
		var dy = $elm$core$Basics$abs(y1 - y2);
		var dx = $elm$core$Basics$abs(x1 - x2);
		return dx + dy;
	});
var $author$project$Data$Effect$withNone = function (a) {
	return $elm$random$Random$constant(
		_Utils_Tuple2(a, _List_Nil));
};
var $author$project$Data$Behavior$Player$moveTowards = F2(
	function (targetPos, game) {
		var _v0 = A2(
			$elm$core$Maybe$andThen,
			$elm$core$List$head,
			A4(
				$krisajenkins$elm_astar$AStar$findPath,
				$krisajenkins$elm_astar$AStar$straightLineCost,
				function (pos) {
					return $elm$core$Set$fromList(
						A2(
							$elm$core$List$filter,
							function (p) {
								return _Utils_eq(p, targetPos) || A2($author$project$Data$Behavior$Player$canMoveTo, game, p);
							},
							$author$project$Data$Position$neighbors(pos)));
				},
				game.player.pos,
				targetPos));
		if (_v0.$ === 'Just') {
			var pos = _v0.a;
			var _v1 = A2($author$project$Data$World$getBlock, pos, game.world);
			_v1$3:
			while (true) {
				if (_v1.$ === 'Just') {
					if (_v1.a.$ === 'FloorBlock') {
						return $author$project$Data$Effect$withNone(
							_Utils_update(
								game,
								{
									player: A2($author$project$Data$Player$moveTo, pos, game.player)
								}));
					} else {
						switch (_v1.a.a.$) {
							case 'Actor':
								var id = _v1.a.a.a;
								var _v2 = A2($author$project$Data$World$getActor, id, game.world);
								if ((_v2.$ === 'Just') && (_v2.a.b.$ === 'Train')) {
									var _v3 = _v2.a;
									return $author$project$Data$Effect$withNone(
										_Utils_update(
											game,
											{
												player: A2($author$project$Data$Player$moveTo, pos, game.player)
											}));
								} else {
									return $author$project$Data$Effect$withNone(
										A2(
											$elm$core$Maybe$withDefault,
											game,
											A2(
												$elm$core$Maybe$map,
												$author$project$Data$Game$setWorldOf(game),
												A2(
													$author$project$Data$World$push,
													{from: game.player.pos, pos: pos},
													game.world))));
								}
							case 'Vein':
								return $author$project$Data$Effect$genWithNone(
									A2(
										$elm$random$Random$map,
										$author$project$Data$Game$setWorldOf(game),
										A2($author$project$Generation$mine, pos, game.world)));
							default:
								break _v1$3;
						}
					}
				} else {
					break _v1$3;
				}
			}
			return $author$project$Data$Effect$withNone(
				A2(
					$elm$core$Maybe$withDefault,
					game,
					A2(
						$elm$core$Maybe$map,
						$author$project$Data$Game$setWorldOf(game),
						A2(
							$author$project$Data$World$push,
							{from: game.player.pos, pos: pos},
							game.world))));
		} else {
			return $elm$random$Random$constant(
				_Utils_Tuple2(game, _List_Nil));
		}
	});
var $author$project$Data$Player$hold = F2(
	function (item, player) {
		var _v0 = player.item;
		if (_v0.$ === 'Just') {
			return $elm$core$Maybe$Nothing;
		} else {
			return $elm$core$Maybe$Just(
				_Utils_update(
					player,
					{
						item: $elm$core$Maybe$Just(item)
					}));
		}
	});
var $author$project$Data$World$removeItem = F2(
	function (pos, world) {
		return _Utils_update(
			world,
			{
				items: A3(
					$elm$core$Dict$update,
					pos,
					$elm$core$Maybe$andThen(
						function (maybe) {
							if (maybe.b) {
								if (!maybe.b.b) {
									return $elm$core$Maybe$Nothing;
								} else {
									var tail = maybe.b;
									return $elm$core$Maybe$Just(tail);
								}
							} else {
								return $elm$core$Maybe$Nothing;
							}
						}),
					world.items)
			});
	});
var $author$project$Data$Behavior$Player$pickUp = F2(
	function (pos, game) {
		var _v0 = A2($author$project$Data$World$get, pos, game.world);
		if ((_v0.$ === 'Just') && (_v0.a.b.$ === 'Just')) {
			var _v1 = _v0.a;
			var item = _v1.b.a;
			return A2(
				$elm$core$Maybe$withDefault,
				_Utils_Tuple2(game, _List_Nil),
				A2(
					$elm$core$Maybe$map,
					function (player) {
						return _Utils_Tuple2(
							_Utils_update(
								game,
								{
									player: player,
									world: A2($author$project$Data$World$removeItem, pos, game.world)
								}),
							_List_fromArray(
								[
									$author$project$Data$Effect$PlaySound($author$project$Data$Sound$PickUp)
								]));
					},
					A2($author$project$Data$Player$hold, item, game.player)));
		} else {
			return _Utils_Tuple2(game, _List_Nil);
		}
	});
var $author$project$Data$Player$dropItem = function (player) {
	return A2(
		$elm$core$Maybe$map,
		function (i) {
			return _Utils_Tuple2(
				_Utils_update(
					player,
					{item: $elm$core$Maybe$Nothing}),
				i);
		},
		player.item);
};
var $author$project$Data$Storage$load = F2(
	function (items, storage) {
		return (_Utils_cmp(
			$elm$core$List$length(storage.items) + $elm$core$List$length(items),
			storage.maxAmount) < 1) ? $elm$core$Maybe$Just(
			_Utils_update(
				storage,
				{
					items: _Utils_ap(storage.items, items)
				})) : $elm$core$Maybe$Nothing;
	});
var $author$project$Data$World$getActorAt = F2(
	function (pos, world) {
		return A2(
			$elm$core$Maybe$andThen,
			function (entity) {
				if (entity.$ === 'Actor') {
					var id = entity.a;
					return A2(
						$elm$core$Maybe$map,
						function (_v1) {
							var actor = _v1.b;
							return _Utils_Tuple2(id, actor);
						},
						A2($elm$core$Dict$get, id, world.actors));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			A2($elm$core$Dict$get, pos, world.entities));
	});
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $author$project$Data$Storage$empty = function (maxAmount) {
	return {items: _List_Nil, maxAmount: maxAmount};
};
var $author$project$Config$wagonMaxItems = 10;
var $author$project$Config$trainLoadSize = $author$project$Config$wagonMaxItems;
var $author$project$Data$Train$updateStorage = F2(
	function (fun, train) {
		return A2(
			$elm$core$Tuple$mapFirst,
			function (storage) {
				return A2($author$project$Data$Train$addAll, storage.items, train);
			},
			fun(
				$author$project$Data$Storage$empty($author$project$Config$trainLoadSize)));
	});
var $author$project$Data$World$updateStorage = F3(
	function (fun, pos, world) {
		return A2(
			$elm$core$Maybe$andThen,
			function (_v0) {
				var id = _v0.a;
				var actor = _v0.b;
				switch (actor.$) {
					case 'Minecart':
						var minecart = actor.a;
						return $elm$core$Maybe$Just(
							A2(
								$elm$core$Tuple$mapFirst,
								function (storage) {
									return A3(
										$author$project$Data$World$setActor,
										id,
										$author$project$Data$Actor$Minecart(
											_Utils_update(
												minecart,
												{storage: storage})),
										world);
								},
								fun(minecart.storage)));
					case 'Train':
						var train = actor.a;
						return $elm$core$Maybe$Just(
							A2(
								$elm$core$Tuple$mapFirst,
								function (t) {
									return A3(
										$author$project$Data$World$setActor,
										id,
										$author$project$Data$Actor$Train(t),
										world);
								},
								A2($author$project$Data$Train$updateStorage, fun, train)));
					default:
						return $elm$core$Maybe$Nothing;
				}
			},
			A2($author$project$Data$World$getActorAt, pos, world));
	});
var $author$project$Data$World$load = F3(
	function (pos, list, world) {
		return A2(
			$elm$core$Maybe$andThen,
			function (_v0) {
				var w = _v0.a;
				var maybeEffect = _v0.b;
				return A2(
					$elm$core$Maybe$map,
					function (l) {
						return _Utils_Tuple2(w, l);
					},
					maybeEffect);
			},
			A3(
				$author$project$Data$World$updateStorage,
				function (storage) {
					return A2(
						$elm$core$Maybe$withDefault,
						_Utils_Tuple2(storage, $elm$core$Maybe$Nothing),
						A2(
							$elm$core$Maybe$map,
							function (s) {
								return _Utils_Tuple2(
									s,
									$elm$core$Maybe$Just(
										_List_fromArray(
											[
												$author$project$Data$Effect$PlaySound($author$project$Data$Sound$Unload)
											])));
							},
							A2($author$project$Data$Storage$load, list, storage)));
				},
				pos,
				world));
	});
var $author$project$Data$Behavior$Player$putInto = F2(
	function (pos, game) {
		return A2(
			$elm$core$Maybe$andThen,
			function (_v0) {
				var player = _v0.a;
				var item = _v0.b;
				return A2(
					$elm$core$Maybe$map,
					$elm$core$Tuple$mapFirst(
						function (w) {
							return _Utils_update(
								game,
								{player: player, world: w});
						}),
					A3(
						$author$project$Data$World$load,
						pos,
						_List_fromArray(
							[item]),
						game.world));
			},
			$author$project$Data$Player$dropItem(game.player));
	});
var $author$project$Data$Behavior$Player$interactWith = F2(
	function (pos, game) {
		return A2(
			$elm$core$Maybe$withDefault,
			$elm$random$Random$constant(
				_Utils_Tuple2(game, _List_Nil)),
			A2(
				$elm$core$Maybe$map,
				$author$project$Data$Effect$map(
					$author$project$Data$Behavior$Player$pickUp(pos)),
				A2(
					$elm$core$Maybe$map,
					function (block) {
						if (block.$ === 'FloorBlock') {
							return A2($author$project$Data$Behavior$Player$moveTowards, pos, game);
						} else {
							var entity = block.a;
							switch (entity.$) {
								case 'Wall':
									return A2(
										$elm$random$Random$map,
										function (g) {
											return _Utils_Tuple2(g, _List_Nil);
										},
										$elm$random$Random$constant(game));
								case 'Actor':
									var id = entity.a;
									return $elm$random$Random$constant(
										A2(
											$elm$core$Maybe$withDefault,
											_Utils_Tuple2(game, _List_Nil),
											A2(
												$elm$core$Maybe$andThen,
												function (_v2) {
													var p = _v2.a;
													return A2($author$project$Data$Behavior$Player$putInto, p, game);
												},
												A2($elm$core$Dict$get, id, game.world.actors))));
								case 'Vein':
									return A2(
										$elm$random$Random$map,
										function (world) {
											return _Utils_Tuple2(
												_Utils_update(
													game,
													{world: world}),
												_List_Nil);
										},
										A2($author$project$Generation$mine, pos, game.world));
								case 'Water':
									return $elm$random$Random$constant(
										_Utils_Tuple2(game, _List_Nil));
								default:
									return $elm$random$Random$constant(
										_Utils_Tuple2(game, _List_Nil));
							}
						}
					},
					A2($author$project$Data$World$getBlock, pos, game.world))));
	});
var $author$project$Data$Player$stopMoving = function (player) {
	return _Utils_update(
		player,
		{targetPos: $elm$core$Maybe$Nothing});
};
var $author$project$Data$Behavior$Player$act = function (game) {
	return A2(
		$elm$core$Maybe$withDefault,
		$elm$random$Random$constant(
			_Utils_Tuple2(game, _List_Nil)),
		A2(
			$elm$core$Maybe$map,
			function (targetPos) {
				return A2(
					$elm$random$Random$map,
					function (_v0) {
						var g = _v0.a;
						var l = _v0.b;
						return A2(
							$elm$core$Tuple$mapSecond,
							$elm$core$Basics$append(l),
							A2($author$project$Data$Behavior$Player$pickUp, g.player.pos, g));
					},
					(A2(
						$elm$core$List$member,
						targetPos,
						$author$project$Data$Position$neighbors(game.player.pos)) || _Utils_eq(targetPos, game.player.pos)) ? A2(
						$elm$random$Random$map,
						$elm$core$Tuple$mapFirst(
							function (g) {
								return _Utils_update(
									g,
									{
										player: $author$project$Data$Player$stopMoving(g.player)
									});
							}),
						A2($author$project$Data$Behavior$Player$interactWith, targetPos, game)) : A2($author$project$Data$Behavior$Player$moveTowards, targetPos, game));
			},
			game.player.targetPos));
};
var $author$project$Data$Actor$Falling = function (a) {
	return {$: 'Falling', a: a};
};
var $author$project$Data$Behavior$Falling$act = F3(
	function (entity, pos, world) {
		return A2(
			$elm$random$Random$map,
			A2($author$project$Data$World$insertEntity, entity, pos),
			A3(
				$elm$core$List$foldl,
				function (p) {
					return $elm$random$Random$andThen(
						function (w) {
							return A2(
								$elm$random$Random$map,
								function (fun) {
									return A2(fun, p, w);
								},
								A2(
									$elm$random$Random$weighted,
									_Utils_Tuple2(
										1,
										$author$project$Data$World$insertEntity(entity)),
									_List_fromArray(
										[
											_Utils_Tuple2(
											1 / 4,
											$author$project$Data$World$insertActor(
												$author$project$Data$Actor$Helper(
													$author$project$Data$Actor$Falling(entity))))
										])));
						});
				},
				$elm$random$Random$constant(world),
				A2(
					$elm$core$List$filter,
					function (p) {
						return A2($author$project$Data$World$isFloor, p, world);
					},
					$author$project$Data$Position$neighbors(pos))));
	});
var $author$project$Data$Improvement$MinecartCanCollect = {$: 'MinecartCanCollect'};
var $author$project$Data$Behavior$Minecart$getMinecart = F2(
	function (id, world) {
		return A2(
			$elm$core$Maybe$andThen,
			function (_v0) {
				var pos = _v0.a;
				var actor = _v0.b;
				if (actor.$ === 'Minecart') {
					var minecart = actor.a;
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(pos, minecart));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			A2($author$project$Data$World$getActor, id, world));
	});
var $author$project$Data$Storage$size = function (storage) {
	return $elm$core$List$length(storage.items);
};
var $author$project$Data$Storage$spaceRemaining = function (storage) {
	return storage.maxAmount - $author$project$Data$Storage$size(storage);
};
var $author$project$Data$Storage$isFull = function (storage) {
	return !$author$project$Data$Storage$spaceRemaining(storage);
};
var $author$project$Data$Storage$insert = F2(
	function (item, storage) {
		return $author$project$Data$Storage$isFull(storage) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
			_Utils_update(
				storage,
				{
					items: A2($elm$core$List$cons, item, storage.items)
				}));
	});
var $author$project$Data$Minecart$setStorageOf = F2(
	function (minecart, storage) {
		return _Utils_update(
			minecart,
			{storage: storage});
	});
var $author$project$Data$Minecart$insert = F2(
	function (item, minecart) {
		return A2(
			$elm$core$Maybe$map,
			function (storage) {
				return _Utils_Tuple2(
					A2($author$project$Data$Minecart$setStorageOf, minecart, storage),
					$author$project$Data$Sound$PickUp);
			},
			A2($author$project$Data$Storage$insert, item, minecart.storage));
	});
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $elm$core$Tuple$mapBoth = F3(
	function (funcA, funcB, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			funcA(x),
			funcB(y));
	});
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $author$project$Data$Behavior$Minecart$pickup = F3(
	function (from, id, world) {
		return A2(
			$elm$core$Maybe$andThen,
			function (_v0) {
				var block = _v0.a;
				var maybeItem = _v0.b;
				if (block.$ === 'FloorBlock') {
					return A2(
						$elm$core$Maybe$map,
						A2(
							$elm$core$Tuple$mapBoth,
							function (m) {
								return A2(
									$author$project$Data$World$removeItem,
									from,
									A3(
										$author$project$Data$World$updateActor,
										id,
										function (_v2) {
											return $author$project$Data$Actor$Minecart(m);
										},
										world));
							},
							function (s) {
								return $elm$core$List$singleton(
									$author$project$Data$Effect$PlaySound(s));
							}),
						A2(
							$elm$core$Maybe$andThen,
							$elm$core$Basics$identity,
							A3(
								$elm$core$Maybe$map2,
								$author$project$Data$Minecart$insert,
								maybeItem,
								A2(
									$elm$core$Maybe$map,
									$elm$core$Tuple$second,
									A2($author$project$Data$Behavior$Minecart$getMinecart, id, world)))));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			A2($author$project$Data$World$get, from, world));
	});
var $author$project$Data$Behavior$Minecart$collect = F4(
	function (pos, id, improvements, w) {
		return A2($elm$core$List$member, $author$project$Data$Improvement$MinecartCanCollect, improvements) ? A3(
			$elm$core$List$foldl,
			F2(
				function (p, _v0) {
					var world = _v0.a;
					var l = _v0.b;
					return A2(
						$elm$core$Maybe$withDefault,
						_Utils_Tuple2(world, l),
						A2(
							$elm$core$Maybe$map,
							$elm$core$Tuple$mapSecond(
								function (e) {
									return _Utils_ap(e, l);
								}),
							A3($author$project$Data$Behavior$Minecart$pickup, p, id, world)));
				}),
			_Utils_Tuple2(w, _List_Nil),
			$author$project$Data$Position$neighbors(pos)) : _Utils_Tuple2(w, _List_Nil);
	});
var $author$project$Data$Floor$Track = {$: 'Track'};
var $author$project$Data$Minecart$moveFrom = F2(
	function (movedFrom, wagon) {
		return _Utils_update(
			wagon,
			{
				movedFrom: $elm$core$Maybe$Just(movedFrom)
			});
	});
var $author$project$Data$Behavior$Minecart$swapWith = F3(
	function (id0, _v0, world) {
		var pos = _v0.a;
		var id1 = _v0.b;
		return A3(
			$elm$core$Maybe$map2,
			F2(
				function (_v1, _v2) {
					var p0 = _v1.a;
					var w0 = _v1.b;
					var w1 = _v2.b;
					return A3(
						$author$project$Data$World$setActor,
						id1,
						$author$project$Data$Actor$Minecart(
							A2($author$project$Data$Minecart$setStorageOf, w1, w0.storage)),
						A3(
							$author$project$Data$World$setActor,
							id0,
							$author$project$Data$Actor$Minecart(
								(_Utils_eq(
									A2($author$project$Data$World$getFloor, p0, world),
									$elm$core$Maybe$Just($author$project$Data$Floor$Track)) ? $author$project$Data$Minecart$moveFrom(pos) : $elm$core$Basics$identity)(
									A2($author$project$Data$Minecart$setStorageOf, w0, w1.storage))),
							world));
				}),
			A2($author$project$Data$Behavior$Minecart$getMinecart, id0, world),
			A2($author$project$Data$Behavior$Minecart$getMinecart, id1, world));
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $author$project$Data$World$remainingSpace = F2(
	function (pos, world) {
		return A2(
			$elm$core$Maybe$map,
			$elm$core$Tuple$second,
			A3(
				$author$project$Data$World$updateStorage,
				function (s) {
					return _Utils_Tuple2(
						s,
						$author$project$Data$Storage$spaceRemaining(s));
				},
				pos,
				world));
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $author$project$Data$Storage$take = F2(
	function (amount, storage) {
		return _Utils_Tuple2(
			_Utils_update(
				storage,
				{
					items: A2($elm$core$List$drop, amount, storage.items)
				}),
			A2($elm$core$List$take, amount, storage.items));
	});
var $author$project$Data$Storage$unload = function (storage) {
	return _Utils_Tuple2(
		_Utils_update(
			storage,
			{items: _List_Nil}),
		storage.items);
};
var $author$project$Data$Storage$takeOrUnload = function (maybeInt) {
	if (maybeInt.$ === 'Nothing') {
		return $author$project$Data$Storage$unload;
	} else {
		var n = maybeInt.a;
		return $author$project$Data$Storage$take(n);
	}
};
var $author$project$Data$World$takeOrUnload = function (maybeAmount) {
	return $author$project$Data$World$updateStorage(
		$author$project$Data$Storage$takeOrUnload(maybeAmount));
};
var $author$project$Data$World$take = function (n) {
	return $author$project$Data$World$takeOrUnload(
		$elm$core$Maybe$Just(n));
};
var $author$project$Data$World$transfer = F2(
	function (args, w) {
		return A2(
			$elm$core$Maybe$andThen,
			function (amount) {
				return A2(
					$elm$core$Maybe$andThen,
					function (_v0) {
						var world = _v0.a;
						var list = _v0.b;
						return $elm$core$List$isEmpty(list) ? $elm$core$Maybe$Just(
							_Utils_Tuple2(world, _List_Nil)) : A3($author$project$Data$World$load, args.to, list, world);
					},
					A3($author$project$Data$World$take, amount, args.from, w));
			},
			A2($author$project$Data$World$remainingSpace, args.to, w));
	});
var $author$project$Data$Behavior$Minecart$collideWith = F3(
	function (target, _v0, world) {
		var pos = _v0.a;
		var id1 = _v0.b;
		return A2(
			$elm$core$Maybe$andThen,
			function (_v1) {
				var from = _v1.a;
				var actor = _v1.b;
				if (actor.$ === 'Minecart') {
					return A2(
						$elm$core$Maybe$map,
						function (w) {
							return _Utils_Tuple2(w, _List_Nil);
						},
						A3(
							$author$project$Data$Behavior$Minecart$swapWith,
							target,
							_Utils_Tuple2(pos, id1),
							world));
				} else {
					return A2(
						$author$project$Data$World$transfer,
						{from: from, to: pos},
						world);
				}
			},
			A2($author$project$Data$World$getActor, target, world));
	});
var $author$project$Data$World$moveActorTo = F3(
	function (pos, id, world) {
		return A2(
			$elm$core$Maybe$withDefault,
			world,
			A2(
				$elm$core$Maybe$map,
				function (_v0) {
					var oldPos = _v0.a;
					return _Utils_update(
						world,
						{
							actors: A3(
								$elm$core$Dict$update,
								id,
								$elm$core$Maybe$map(
									$elm$core$Tuple$mapFirst(
										function (_v1) {
											return pos;
										})),
								world.actors),
							entities: A3(
								$elm$core$Dict$insert,
								pos,
								$author$project$Data$Entity$Actor(id),
								A2($elm$core$Dict$remove, oldPos, world.entities))
						});
				},
				A2($elm$core$Dict$get, id, world.actors)));
	});
var $author$project$Data$Minecart$stop = function (wagon) {
	return _Utils_update(
		wagon,
		{movedFrom: $elm$core$Maybe$Nothing});
};
var $author$project$Data$Behavior$Minecart$setMovement = F3(
	function (_v0, p, w) {
		var id = _v0.a;
		var pos = _v0.b;
		return A2(
			$elm$core$Maybe$withDefault,
			w,
			A2(
				$elm$core$Maybe$map,
				function (actor) {
					return A3(
						$author$project$Data$World$moveActorTo,
						p,
						id,
						A3($author$project$Data$World$setActor, id, actor, w));
				},
				A2(
					$elm$core$Maybe$map,
					$author$project$Data$Actor$Minecart,
					A2(
						$elm$core$Maybe$map,
						function () {
							var _v1 = A2($author$project$Data$World$getFloor, p, w);
							_v1$2:
							while (true) {
								if (_v1.$ === 'Just') {
									switch (_v1.a.$) {
										case 'Track':
											var _v2 = _v1.a;
											return $author$project$Data$Minecart$moveFrom(pos);
										case 'RailwayTrack':
											var _v3 = _v1.a;
											return $author$project$Data$Minecart$moveFrom(pos);
										default:
											break _v1$2;
									}
								} else {
									break _v1$2;
								}
							}
							return $author$project$Data$Minecart$stop;
						}(),
						A2(
							$elm$core$Maybe$map,
							$elm$core$Tuple$second,
							A2($author$project$Data$Behavior$Minecart$getMinecart, id, w))))));
	});
var $author$project$Data$Behavior$Minecart$moveOnGround = F3(
	function (args, _v0, world) {
		var pos = _v0.a;
		var id = _v0.b;
		var wagon = _v0.c;
		return A2(
			$elm$core$Maybe$withDefault,
			$author$project$Data$Effect$withNone(world),
			A2(
				$elm$core$Maybe$map,
				function (block) {
					if (block.a.$ === 'FloorBlock') {
						return $author$project$Data$Effect$withNone(
							A3(
								$author$project$Data$Behavior$Minecart$setMovement,
								_Utils_Tuple2(id, pos),
								args.forwardPos,
								world));
					} else {
						var entity = block.a.a;
						return function (_v3) {
							var w = _v3.a;
							var l = _v3.b;
							return $elm$random$Random$constant(
								_Utils_Tuple2(
									A3(
										$author$project$Data$Behavior$Minecart$setMovement,
										_Utils_Tuple2(id, pos),
										function () {
											var _v4 = A2($author$project$Data$World$get, args.backPos, world);
											if ((_v4.$ === 'Just') && (_v4.a.a.$ === 'FloorBlock')) {
												var _v5 = _v4.a;
												return args.backPos;
											} else {
												return pos;
											}
										}(),
										w),
									l));
						}(
							A2(
								$elm$core$Maybe$withDefault,
								_Utils_Tuple2(world, _List_Nil),
								function () {
									if (entity.$ === 'Actor') {
										var id0 = entity.a;
										return A3(
											$author$project$Data$Behavior$Minecart$collideWith,
											id0,
											_Utils_Tuple2(pos, id),
											world);
									} else {
										return $elm$core$Maybe$Nothing;
									}
								}()));
					}
				},
				A2($author$project$Data$World$get, args.forwardPos, world)));
	});
var $author$project$Data$Behavior$Minecart$neighborTracks = F2(
	function (pos, world) {
		return A2(
			$elm$core$List$filter,
			function (p) {
				var _v0 = A2($author$project$Data$World$getBlock, p, world);
				_v0$2:
				while (true) {
					if ((_v0.$ === 'Just') && (_v0.a.$ === 'FloorBlock')) {
						switch (_v0.a.a.$) {
							case 'Track':
								var _v1 = _v0.a.a;
								return true;
							case 'RailwayTrack':
								var _v2 = _v0.a.a;
								return true;
							default:
								break _v0$2;
						}
					} else {
						break _v0$2;
					}
				}
				return false;
			},
			$author$project$Data$Position$neighbors(pos));
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$random$Random$addOne = function (value) {
	return _Utils_Tuple2(1, value);
};
var $elm$random$Random$uniform = F2(
	function (value, valueList) {
		return A2(
			$elm$random$Random$weighted,
			$elm$random$Random$addOne(value),
			A2($elm$core$List$map, $elm$random$Random$addOne, valueList));
	});
var $author$project$Data$Behavior$Minecart$moveOnTrack = F3(
	function (args, _v0, world) {
		var pos = _v0.a;
		var id = _v0.b;
		var wagon = _v0.c;
		var _v1 = A2(
			$elm$core$List$filter,
			function (p) {
				return !_Utils_eq(p, args.backPos);
			},
			A2($author$project$Data$Behavior$Minecart$neighborTracks, pos, world));
		if (_v1.b) {
			var head = _v1.a;
			var tail = _v1.b;
			return A2(
				$elm$random$Random$andThen,
				function (p) {
					return $author$project$Data$Effect$withNone(
						A3(
							$author$project$Data$Behavior$Minecart$setMovement,
							_Utils_Tuple2(id, pos),
							p,
							world));
				},
				A2($elm$random$Random$uniform, head, tail));
		} else {
			return A3(
				$author$project$Data$Behavior$Minecart$moveOnGround,
				args,
				_Utils_Tuple3(pos, id, wagon),
				world);
		}
	});
var $author$project$Data$Behavior$Minecart$move = F3(
	function (id, _v0, world) {
		var pos = _v0.a;
		var wagon = _v0.b;
		return A2(
			$elm$core$Maybe$map,
			function (movedFrom) {
				return function (positions) {
					return A3(
						function () {
							var _v1 = A2($author$project$Data$World$getFloor, pos, world);
							_v1$2:
							while (true) {
								if (_v1.$ === 'Just') {
									switch (_v1.a.$) {
										case 'Track':
											var _v2 = _v1.a;
											return $author$project$Data$Behavior$Minecart$moveOnTrack;
										case 'RailwayTrack':
											var _v3 = _v1.a;
											return $author$project$Data$Behavior$Minecart$moveOnTrack;
										default:
											break _v1$2;
									}
								} else {
									break _v1$2;
								}
							}
							return $author$project$Data$Behavior$Minecart$moveOnGround;
						}(),
						positions,
						_Utils_Tuple3(pos, id, wagon),
						world);
				}(
					{
						backPos: movedFrom,
						forwardPos: A2(
							$author$project$Data$Position$plus,
							pos,
							A2($author$project$Data$Position$vecTo, pos, movedFrom))
					});
			},
			wagon.movedFrom);
	});
var $author$project$Data$Behavior$Minecart$unload = F2(
	function (id, world) {
		return A2(
			$elm$core$Maybe$andThen,
			function (args) {
				return A2($author$project$Data$World$transfer, args, world);
			},
			A2(
				$elm$core$Maybe$andThen,
				function (_v0) {
					var pos = _v0.a;
					return A2(
						$elm$core$Maybe$map,
						function (p) {
							return {from: pos, to: p};
						},
						$elm$core$List$head(
							A2(
								$elm$core$List$filter,
								function (p) {
									var _v1 = A2($author$project$Data$World$getActorAt, p, world);
									if ((_v1.$ === 'Just') && (_v1.a.b.$ === 'Train')) {
										var _v2 = _v1.a;
										return true;
									} else {
										return false;
									}
								},
								$author$project$Data$Position$neighbors(pos))));
				},
				A2($author$project$Data$Behavior$Minecart$getMinecart, id, world)));
	});
var $author$project$Data$Behavior$Minecart$act = F3(
	function (id, improvements, world) {
		return A2(
			$author$project$Data$Effect$map,
			function (g) {
				return A2(
					$elm$core$Maybe$withDefault,
					_Utils_Tuple2(g, _List_Nil),
					A2($author$project$Data$Behavior$Minecart$unload, id, g));
			},
			A2(
				$elm$core$Maybe$withDefault,
				$author$project$Data$Effect$withNone(world),
				A2(
					$elm$core$Maybe$andThen,
					function (_v0) {
						var pos = _v0.a;
						var wagon = _v0.b;
						return A2(
							$elm$core$Maybe$map,
							$author$project$Data$Effect$map(
								A3($author$project$Data$Behavior$Minecart$collect, pos, id, improvements)),
							A3(
								$author$project$Data$Behavior$Minecart$move,
								id,
								_Utils_Tuple2(pos, wagon),
								world));
					},
					A2($author$project$Data$Behavior$Minecart$getMinecart, id, world))));
	});
var $author$project$Data$Entity$Lava = {$: 'Lava'};
var $author$project$Data$Behavior$MovingWater$getLavaNeighbors = F2(
	function (pos, world) {
		return A2(
			$elm$core$List$filter,
			function (p) {
				return _Utils_eq(
					A2($author$project$Data$World$getBlock, p, world),
					$elm$core$Maybe$Just(
						$author$project$Data$Block$EntityBlock($author$project$Data$Entity$Lava)));
			},
			$author$project$Data$Position$neighbors(pos));
	});
var $author$project$Data$Behavior$MovingWater$destroyNearLava = F2(
	function (id, world) {
		return A2(
			$elm$core$Maybe$andThen,
			function (_v0) {
				var pos = _v0.a;
				return function (list) {
					return $elm$core$List$isEmpty(list) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
						A2(
							$author$project$Data$World$removeEntity,
							pos,
							A3($elm$core$List$foldl, $author$project$Data$World$removeEntity, world, list)));
				}(
					A2($author$project$Data$Behavior$MovingWater$getLavaNeighbors, pos, world));
			},
			A2($author$project$Data$World$getActor, id, world));
	});
var $author$project$Data$Momentum$applyTo = F2(
	function (p, _v0) {
		var momentum = _v0.momentum;
		return A2(
			$elm$core$Maybe$withDefault,
			p,
			A2(
				$elm$core$Maybe$map,
				$author$project$Data$Position$plus(p),
				momentum));
	});
var $author$project$Data$Behavior$MovingWater$getMomentum = F2(
	function (id, world) {
		return A2(
			$elm$core$Maybe$andThen,
			function (_v0) {
				var pos = _v0.a;
				var actor = _v0.b;
				if (actor.$ === 'MovingWater') {
					var momentum = actor.a;
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(pos, momentum));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			A2($author$project$Data$World$getActor, id, world));
	});
var $author$project$Data$Behavior$MovingWater$getNewPos = F2(
	function (id, world) {
		return A2(
			$elm$core$Maybe$map,
			function (_v0) {
				var pos = _v0.a;
				var momentum = _v0.b;
				return function (to) {
					return {from: pos, to: to};
				}(
					A2($author$project$Data$Momentum$applyTo, pos, momentum));
			},
			A2($author$project$Data$Behavior$MovingWater$getMomentum, id, world));
	});
var $author$project$Data$Entity$Water = {$: 'Water'};
var $author$project$Data$Behavior$MovingWater$moveAndStop = F2(
	function (args, world) {
		return A3(
			$author$project$Data$World$insertEntity,
			$author$project$Data$Entity$Water,
			args.to,
			A2($author$project$Data$World$removeEntity, args.from, world));
	});
var $author$project$Data$Behavior$MovingWater$move = F2(
	function (id, world) {
		return A2(
			$elm$core$Maybe$withDefault,
			world,
			A2(
				$elm$core$Maybe$map,
				function (_v0) {
					var from = _v0.from;
					var to = _v0.to;
					return A2($author$project$Data$World$isFloor, to, world) ? A2(
						$author$project$Data$Behavior$MovingWater$moveAndStop,
						{from: from, to: to},
						world) : A2(
						$author$project$Data$Behavior$MovingWater$moveAndStop,
						{from: from, to: from},
						world);
				},
				A2($author$project$Data$Behavior$MovingWater$getNewPos, id, world)));
	});
var $author$project$Data$Momentum$revert = function (_v0) {
	var momentum = _v0.momentum;
	return {
		momentum: A2(
			$elm$core$Maybe$map,
			A2(
				$elm$core$Tuple$mapBoth,
				$elm$core$Basics$mul(-1),
				$elm$core$Basics$mul(-1)),
			momentum)
	};
};
var $author$project$Data$Behavior$MovingWater$getFloorNeighbors = F2(
	function (pos, world) {
		return A2(
			$elm$core$List$filter,
			function (p) {
				return A2($author$project$Data$World$isFloor, p, world);
			},
			$author$project$Data$Position$neighbors(pos));
	});
var $author$project$Data$Behavior$MovingWater$setMomentum = F2(
	function (id, world) {
		return A2(
			$elm$core$Maybe$withDefault,
			$elm$random$Random$constant(world),
			A2(
				$elm$core$Maybe$map,
				function (_v0) {
					var from = _v0.a;
					var momentum = _v0.b;
					var forwardPos = A2($author$project$Data$Momentum$applyTo, from, momentum);
					var backwardPos = A2(
						$author$project$Data$Momentum$applyTo,
						from,
						$author$project$Data$Momentum$revert(momentum));
					return A2(
						$elm$random$Random$map,
						function (p) {
							return A3(
								$author$project$Data$World$setActor,
								id,
								$author$project$Data$Actor$MovingWater(
									$author$project$Data$Momentum$fromPoints(
										{from: from, to: p})),
								world);
						},
						function (list) {
							if (list.b) {
								var head = list.a;
								var tail = list.b;
								return A2($elm$core$List$member, forwardPos, list) ? $elm$random$Random$constant(forwardPos) : A2($elm$random$Random$uniform, head, tail);
							} else {
								return $elm$random$Random$constant(forwardPos);
							}
						}(
							A2(
								$elm$core$List$filter,
								$elm$core$Basics$neq(backwardPos),
								A2($author$project$Data$Behavior$MovingWater$getFloorNeighbors, from, world))));
				},
				A2($author$project$Data$Behavior$MovingWater$getMomentum, id, world)));
	});
var $author$project$Data$Behavior$MovingWater$act = F2(
	function (id, w) {
		var _v0 = A2($author$project$Data$Behavior$MovingWater$destroyNearLava, id, w);
		if (_v0.$ === 'Just') {
			var world = _v0.a;
			return $elm$random$Random$constant(world);
		} else {
			return A2(
				$elm$random$Random$map,
				$elm$core$Maybe$withDefault(w),
				A2(
					$elm$random$Random$map,
					function (world) {
						return A2(
							$elm$core$Maybe$map,
							function (_v2) {
								var block = _v2.block;
								var from = _v2.from;
								var to = _v2.to;
								if (block.$ === 'EntityBlock') {
									return A2(
										$author$project$Data$Behavior$MovingWater$move,
										id,
										A3(
											$author$project$Data$World$setActor,
											id,
											$author$project$Data$Actor$MovingWater(
												$author$project$Data$Momentum$revert(
													$author$project$Data$Momentum$fromPoints(
														{from: from, to: to}))),
											A2(
												$elm$core$Maybe$withDefault,
												world,
												A2(
													$author$project$Data$World$push,
													{from: from, pos: to},
													world))));
								} else {
									return A2($author$project$Data$Behavior$MovingWater$move, id, world);
								}
							},
							A2(
								$elm$core$Maybe$andThen,
								function (_v1) {
									var from = _v1.from;
									var to = _v1.to;
									return A2(
										$elm$core$Maybe$map,
										function (block) {
											return {block: block, from: from, to: to};
										},
										A2($author$project$Data$World$getBlock, to, world));
								},
								A2($author$project$Data$Behavior$MovingWater$getNewPos, id, world)));
					},
					A2($author$project$Data$Behavior$MovingWater$setMomentum, id, w)));
		}
	});
var $author$project$Data$Behavior$Path$act = F2(
	function (pos, world) {
		var floorNeighbors = $elm$core$List$length(
			A2(
				$elm$core$List$filter,
				function (p) {
					return A2($author$project$Data$World$isFloor, p, world);
				},
				$author$project$Data$Position$neighbors(pos)));
		return A2(
			$elm$random$Random$map,
			$author$project$Data$World$removeEntity(pos),
			((floorNeighbors > 1) ? A2(
				$elm$core$List$foldl,
				function (p) {
					return $elm$random$Random$andThen(
						function (w) {
							return $elm$random$Random$constant(
								A3($author$project$Data$World$insertEntity, $author$project$Data$Entity$Wall, p, w));
						});
				},
				$elm$random$Random$constant(world)) : A2(
				$elm$core$List$foldl,
				function (p) {
					return $elm$random$Random$andThen(
						function (w) {
							return A2(
								$elm$random$Random$andThen,
								function (rand) {
									return (_Utils_cmp(rand, 1 / 3) < 0) ? $elm$random$Random$constant(
										A3(
											$author$project$Data$World$insertActor,
											$author$project$Data$Actor$Helper($author$project$Data$Actor$Path),
											p,
											w)) : A2(
										$elm$random$Random$map,
										function (fun) {
											return A2(fun, p, w);
										},
										$author$project$Generation$Wall$wallGenerator(p));
								},
								A2($elm$random$Random$float, 0, 1));
						});
				},
				$elm$random$Random$constant(world)))(
				A2(
					$elm$core$List$filter,
					function (p) {
						return _Utils_eq(
							A2($author$project$Data$World$getBlock, p, world),
							$elm$core$Maybe$Nothing);
					},
					$author$project$Data$Position$neighbors(pos))));
	});
var $author$project$Data$Train$forwardPos = function (train) {
	var _v0 = train.pos;
	var x = _v0.a;
	var y = _v0.b;
	var _v1 = train.dir;
	var dirX = _v1.a;
	var dirY = _v1.b;
	return _Utils_Tuple2(x + dirX, y + dirY);
};
var $author$project$Data$Behavior$Train$getTrain = F2(
	function (id, world) {
		return A2(
			$elm$core$Maybe$andThen,
			function (_v0) {
				var actor = _v0.b;
				if (actor.$ === 'Train') {
					var train = actor.a;
					return $elm$core$Maybe$Just(train);
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			A2($author$project$Data$World$getActor, id, world));
	});
var $author$project$Config$hqPos = _Utils_Tuple2(0, 0);
var $author$project$ListBag$member = function (a) {
	return $elm$core$List$any(
		function (_v0) {
			var a1 = _v0.a;
			return _Utils_eq(a, a1);
		});
};
var $author$project$Data$Improvement$GetOneGoldEachLevel = {$: 'GetOneGoldEachLevel'};
var $author$project$Data$Item$Gold = {$: 'Gold'};
var $author$project$Data$Effect$LevelUp = {$: 'LevelUp'};
var $author$project$Data$Effect$OpenModal = function (a) {
	return {$: 'OpenModal', a: a};
};
var $author$project$Data$Train$addItem = function (item) {
	return $author$project$Data$Train$addAll(
		_List_fromArray(
			[item]));
};
var $author$project$Data$Train$addTracks = F2(
	function (tracks, train) {
		return _Utils_update(
			train,
			{tracks: train.tracks + tracks});
	});
var $author$project$Data$Improvement$TrainCanCollect = {$: 'TrainCanCollect'};
var $author$project$Data$Improvement$asList = _List_fromArray(
	[$author$project$Data$Improvement$GetOneGoldEachLevel, $author$project$Data$Improvement$MinecartCanCollect, $author$project$Data$Improvement$TrainCanCollect]);
var $elm_community$random_extra$Random$List$get = F2(
	function (index, list) {
		return $elm$core$List$head(
			A2($elm$core$List$drop, index, list));
	});
var $elm_community$random_extra$Random$List$choose = function (list) {
	if ($elm$core$List$isEmpty(list)) {
		return $elm$random$Random$constant(
			_Utils_Tuple2($elm$core$Maybe$Nothing, list));
	} else {
		var lastIndex = $elm$core$List$length(list) - 1;
		var gen = A2($elm$random$Random$int, 0, lastIndex);
		var front = function (i) {
			return A2($elm$core$List$take, i, list);
		};
		var back = function (i) {
			return A2($elm$core$List$drop, i + 1, list);
		};
		return A2(
			$elm$random$Random$map,
			function (index) {
				return _Utils_Tuple2(
					A2($elm_community$random_extra$Random$List$get, index, list),
					A2(
						$elm$core$List$append,
						front(index),
						back(index)));
			},
			gen);
	}
};
var $elm$random$Random$lazy = function (callback) {
	return $elm$random$Random$Generator(
		function (seed) {
			var _v0 = callback(_Utils_Tuple0);
			var gen = _v0.a;
			return gen(seed);
		});
};
var $elm_community$random_extra$Random$List$choices = F2(
	function (count, list) {
		return (count < 1) ? $elm$random$Random$constant(
			_Utils_Tuple2(_List_Nil, list)) : A2(
			$elm$random$Random$andThen,
			function (_v0) {
				var choice = _v0.a;
				var remaining = _v0.b;
				var genRest = $elm$random$Random$lazy(
					function (_v3) {
						return A2($elm_community$random_extra$Random$List$choices, count - 1, remaining);
					});
				var addToChoices = F2(
					function (elem, _v2) {
						var chosen = _v2.a;
						var unchosen = _v2.b;
						return _Utils_Tuple2(
							A2($elm$core$List$cons, elem, chosen),
							unchosen);
					});
				if (choice.$ === 'Nothing') {
					return $elm$random$Random$constant(
						_Utils_Tuple2(_List_Nil, list));
				} else {
					var elem = choice.a;
					return A2(
						$elm$random$Random$map,
						addToChoices(elem),
						genRest);
				}
			},
			$elm_community$random_extra$Random$List$choose(list));
	});
var $author$project$Data$Modal$LevelUpModal = function (a) {
	return {$: 'LevelUpModal', a: a};
};
var $author$project$Data$Modal$levelUp = $author$project$Data$Modal$LevelUpModal;
var $author$project$Data$Train$move = function (train) {
	return _Utils_update(
		train,
		{
			pos: $author$project$Data$Train$forwardPos(train)
		});
};
var $author$project$Data$Behavior$Train$setTrainOf = F3(
	function (world, id, train) {
		return A3(
			$author$project$Data$World$setActor,
			id,
			$author$project$Data$Actor$Train(train),
			world);
	});
var $author$project$Data$Behavior$Train$move = F2(
	function (id, world) {
		return A2(
			$elm$core$Maybe$andThen,
			function (_v0) {
				var newPos = _v0.a;
				var train = _v0.b;
				return A2(
					$elm$core$Maybe$map,
					A2($author$project$Data$World$moveActorTo, newPos, id),
					A2(
						$elm$core$Maybe$map,
						A2($author$project$Data$Behavior$Train$setTrainOf, world, id),
						A2(
							$elm$core$Maybe$map,
							$author$project$Data$Train$move,
							A3($author$project$Data$Train$removeItem, 1, $author$project$Data$Item$Coal, train))));
			},
			A2(
				$elm$core$Maybe$map,
				function (train) {
					return _Utils_Tuple2(
						$author$project$Data$Train$forwardPos(train),
						train);
				},
				A2($author$project$Data$Behavior$Train$getTrain, id, world)));
	});
var $elm$core$Basics$not = _Basics_not;
var $author$project$Data$Train$turnDownwards = function (train) {
	return _Utils_update(
		train,
		{
			dir: _Utils_Tuple2(0, 1)
		});
};
var $author$project$Data$Behavior$Train$stockUpAtBase = F3(
	function (id, improvements, world) {
		return A2(
			$elm$core$Maybe$withDefault,
			$author$project$Data$Effect$withNone(world),
			A2(
				$elm$core$Maybe$map,
				function (w) {
					return A2(
						$elm$random$Random$map,
						function (list) {
							return _Utils_Tuple2(
								w,
								_List_fromArray(
									[
										$author$project$Data$Effect$OpenModal(
										$author$project$Data$Modal$levelUp(list)),
										$author$project$Data$Effect$LevelUp
									]));
						},
						A2(
							$elm$random$Random$map,
							$elm$core$Tuple$first,
							A2(
								$elm_community$random_extra$Random$List$choices,
								2,
								A2(
									$elm$core$List$filter,
									function (e) {
										return !A2($elm$core$List$member, e, improvements);
									},
									$author$project$Data$Improvement$asList))));
				},
				A2(
					$elm$core$Maybe$andThen,
					function (t) {
						return A2(
							$author$project$Data$Behavior$Train$move,
							id,
							function (train) {
								return A3($author$project$Data$World$setActor, id, train, world);
							}(
								$author$project$Data$Actor$Train(
									$author$project$Data$Train$turnDownwards(
										(A2($elm$core$List$member, $author$project$Data$Improvement$GetOneGoldEachLevel, improvements) ? $author$project$Data$Train$addItem($author$project$Data$Item$Gold) : $elm$core$Basics$identity)(
											A2($author$project$Data$Train$addTracks, $author$project$Config$tracksPerTrip, t))))));
					},
					A2($author$project$Data$Behavior$Train$getTrain, id, world))));
	});
var $author$project$Data$Behavior$Train$collect = F3(
	function (id, improvements, world) {
		return A2($elm$core$List$member, $author$project$Data$Improvement$TrainCanCollect, improvements) ? A2(
			$elm$core$Maybe$withDefault,
			world,
			A2(
				$elm$core$Maybe$andThen,
				function (train) {
					return A2(
						$elm$core$Maybe$map,
						A2($author$project$Data$Behavior$Train$setTrainOf, world, id),
						A2(
							$elm$core$Maybe$map,
							function (item) {
								return A2($author$project$Data$Train$addItem, item, train);
							},
							function (pos) {
								return A2($author$project$Data$World$getItem, pos, world);
							}(
								$author$project$Data$Train$forwardPos(train))));
				},
				A2($author$project$Data$Behavior$Train$getTrain, id, world))) : world;
	});
var $author$project$Data$Behavior$Train$mine = F2(
	function (id, world) {
		return A2(
			$elm$core$Maybe$map,
			function (pos) {
				return A2($author$project$Generation$mine, pos, world);
			},
			A2(
				$elm$core$Maybe$map,
				$author$project$Data$Train$forwardPos,
				A2($author$project$Data$Behavior$Train$getTrain, id, world)));
	});
var $author$project$Data$Train$turnUpwards = function (train) {
	return _Utils_update(
		train,
		{
			dir: _Utils_Tuple2(0, -1)
		});
};
var $author$project$Data$Behavior$Train$turnToHQ = F2(
	function (id, world) {
		return A2(
			$elm$core$Maybe$map,
			A2($author$project$Data$Behavior$Train$setTrainOf, world, id),
			A2(
				$elm$core$Maybe$map,
				$author$project$Data$Train$turnUpwards,
				A2($author$project$Data$Behavior$Train$getTrain, id, world)));
	});
var $author$project$Data$Behavior$Train$collideWith = F3(
	function (_v0, id, world) {
		var newPos = _v0.a;
		var entity = _v0.b;
		return A2(
			$elm$core$Maybe$andThen,
			function (train) {
				if (entity.$ === 'Actor') {
					var targetId = entity.a;
					return A2(
						$elm$core$Maybe$andThen,
						function (_v2) {
							var actor = _v2.b;
							switch (actor.$) {
								case 'Minecart':
									var wagon = actor.a;
									return $elm$core$Maybe$Just(
										$author$project$Data$Effect$withNone(
											A3(
												$author$project$Data$World$insertAllItems,
												wagon.storage.items,
												newPos,
												A2(
													$author$project$Data$World$removeEntity,
													newPos,
													A3(
														$author$project$Data$Behavior$Train$setTrainOf,
														world,
														id,
														A2(
															$author$project$Data$Train$addAll,
															A2($elm$core$List$repeat, $author$project$Config$wagonCost, $author$project$Data$Item$Iron),
															train))))));
								case 'Helper':
									return $elm$core$Maybe$Nothing;
								case 'Train':
									return $elm$core$Maybe$Nothing;
								case 'Bomb':
									return $elm$core$Maybe$Just(
										$author$project$Data$Effect$withNone(world));
								default:
									return $elm$core$Maybe$Nothing;
							}
						},
						A2($author$project$Data$World$getActor, targetId, world));
				} else {
					return A2(
						$elm$core$Maybe$map,
						$author$project$Data$Effect$genWithNone,
						A2(
							$elm$core$Maybe$andThen,
							$author$project$Data$Behavior$Train$mine(id),
							((train.tracks > 0) ? $elm$core$Maybe$Just : $author$project$Data$Behavior$Train$turnToHQ(id))(world)));
				}
			},
			A2($author$project$Data$Behavior$Train$getTrain, id, world));
	});
var $author$project$Data$Train$removeTrack = function (train) {
	return (train.moving && (train.tracks > 0)) ? $elm$core$Maybe$Just(
		_Utils_update(
			train,
			{tracks: train.tracks - 1})) : $elm$core$Maybe$Nothing;
};
var $author$project$Data$Behavior$Train$mineAndPlaceTrack = F2(
	function (id, world) {
		return A2(
			$elm$core$Maybe$andThen,
			function (train) {
				var newPos = $author$project$Data$Train$forwardPos(train);
				return A2(
					$elm$core$Maybe$map,
					$elm$random$Random$map(
						A2($author$project$Data$World$insertFloorAt, newPos, $author$project$Data$Floor$RailwayTrack)),
					A2(
						$elm$core$Maybe$andThen,
						$author$project$Data$Behavior$Train$mine(id),
						A2(
							$elm$core$Maybe$map,
							function (t) {
								return A3($author$project$Data$World$setActor, id, t, world);
							},
							A2(
								$elm$core$Maybe$map,
								$author$project$Data$Actor$Train,
								$author$project$Data$Train$removeTrack(train)))));
			},
			A2($author$project$Data$Behavior$Train$getTrain, id, world));
	});
var $author$project$Data$Behavior$Train$tryMovingTo = F4(
	function (_v0, id, improvements, world) {
		var newPos = _v0.a;
		var block = _v0.b;
		return A2(
			$elm$core$Maybe$map,
			$elm$random$Random$map(
				$elm$core$Tuple$mapFirst(
					A2($author$project$Data$Behavior$Train$collect, id, improvements))),
			A2(
				$elm$core$Maybe$andThen,
				function (train) {
					if (block.a.$ === 'FloorBlock') {
						var floor = block.a.a;
						var maybeItem = block.b;
						return A2(
							$elm$core$Maybe$map,
							$elm$random$Random$map(
								$elm$core$Tuple$mapFirst(
									function (w) {
										return A2(
											$elm$core$Maybe$withDefault,
											w,
											A2(
												$elm$core$Maybe$andThen,
												function (item) {
													return A2(
														$elm$core$Maybe$map,
														A2($author$project$Data$Behavior$Train$setTrainOf, w, id),
														A2(
															$elm$core$Maybe$map,
															$author$project$Data$Train$addItem(item),
															A2($author$project$Data$Behavior$Train$getTrain, id, w)));
												},
												maybeItem));
									})),
							function () {
								switch (floor.$) {
									case 'Ground':
										return (train.tracks > 0) ? A2(
											$elm$core$Maybe$map,
											$author$project$Data$Effect$genWithNone,
											A2($author$project$Data$Behavior$Train$mineAndPlaceTrack, id, world)) : A2(
											$elm$core$Maybe$map,
											$author$project$Data$Effect$withNone,
											A2($author$project$Data$Behavior$Train$turnToHQ, id, world));
									case 'RailwayTrack':
										return (train.tracks > 0) ? A2(
											$elm$core$Maybe$map,
											$elm$random$Random$constant,
											A2(
												$elm$core$Maybe$map,
												function (g) {
													return _Utils_Tuple2(
														g,
														_List_fromArray(
															[
																$author$project$Data$Effect$PlaySound($author$project$Data$Sound$MovingTrain)
															]));
												},
												A2($author$project$Data$Behavior$Train$move, id, world))) : ((_Utils_cmp(
											$author$project$Data$Train$coalNeeded(train),
											A2($author$project$ListBag$count, $author$project$Data$Item$Coal, train.items)) < 1) ? A2(
											$elm$core$Maybe$map,
											$elm$random$Random$constant,
											A2(
												$elm$core$Maybe$map,
												function (g) {
													return _Utils_Tuple2(
														g,
														_List_fromArray(
															[
																$author$project$Data$Effect$PlaySound($author$project$Data$Sound$MovingTrain)
															]));
												},
												A2($author$project$Data$Behavior$Train$move, id, world))) : $elm$core$Maybe$Nothing);
									default:
										return (train.tracks > 0) ? A2(
											$elm$core$Maybe$map,
											$author$project$Data$Effect$genWithNone,
											A2($author$project$Data$Behavior$Train$mineAndPlaceTrack, id, world)) : A2(
											$elm$core$Maybe$map,
											$author$project$Data$Effect$genWithNone,
											A2(
												$elm$core$Maybe$andThen,
												$author$project$Data$Behavior$Train$mine(id),
												A2($author$project$Data$Behavior$Train$turnToHQ, id, world)));
								}
							}());
					} else {
						var entity = block.a.a;
						return A3(
							$author$project$Data$Behavior$Train$collideWith,
							_Utils_Tuple2(newPos, entity),
							id,
							world);
					}
				},
				A2($author$project$Data$Behavior$Train$getTrain, id, world)));
	});
var $author$project$Data$Behavior$Train$act = F3(
	function (improvements, id, world) {
		return A2(
			$elm$core$Maybe$withDefault,
			$author$project$Data$Effect$withNone(world),
			A2(
				$elm$core$Maybe$andThen,
				function (train) {
					var newPos = $author$project$Data$Train$forwardPos(train);
					return _Utils_eq(train.pos, $author$project$Config$hqPos) ? $elm$core$Maybe$Just(
						A3($author$project$Data$Behavior$Train$stockUpAtBase, id, improvements, world)) : ((train.moving && A2($author$project$ListBag$member, $author$project$Data$Item$Coal, train.items)) ? A2(
						$elm$core$Maybe$andThen,
						function (block) {
							return A4(
								$author$project$Data$Behavior$Train$tryMovingTo,
								_Utils_Tuple2(newPos, block),
								id,
								improvements,
								world);
						},
						A2($author$project$Data$World$get, newPos, world)) : $elm$core$Maybe$Nothing);
				},
				A2($author$project$Data$Behavior$Train$getTrain, id, world)));
	});
var $author$project$Data$Actor$Mine = {$: 'Mine'};
var $author$project$Generation$caveGenerator = F3(
	function (args, pos, world) {
		return A2(
			$elm$random$Random$andThen,
			function (fun) {
				return A3(
					$author$project$Generation$generateContent,
					pos,
					A2(
						$elm$random$Random$andThen,
						$elm$core$Basics$identity,
						A2(
							$elm$random$Random$weighted,
							_Utils_Tuple2(
								0,
								$elm$random$Random$constant(
									$author$project$Data$World$insertEntity($author$project$Data$Entity$Wall))),
							_List_fromArray(
								[
									_Utils_Tuple2(
									1,
									$elm$random$Random$constant(
										$author$project$Data$World$insertActor(
											$author$project$Data$Actor$Helper(
												$author$project$Data$Actor$Cave(args.cave))))),
									_Utils_Tuple2(
									1 / 2,
									$author$project$Generation$Wall$wallGenerator(pos)),
									_Utils_Tuple2(
									1 / 4,
									$elm$random$Random$constant(
										$author$project$Data$World$insertActor(
											$author$project$Data$Actor$Helper($author$project$Data$Actor$Path)))),
									_Utils_Tuple2(
									1 / 16,
									$elm$random$Random$constant(
										$author$project$Data$World$insertActor(
											$author$project$Data$Actor$Helper($author$project$Data$Actor$Mine))))
								]))),
					A2(
						fun,
						pos,
						A2($author$project$Data$World$removeEntity, pos, world)));
			},
			args.ground);
	});
var $author$project$Data$World$insertItem = F2(
	function (item, pos) {
		return A2($author$project$Data$World$insertItemAt, pos, item);
	});
var $author$project$Generation$Cave$exposedCave = function (caveType) {
	return function (ground) {
		return $author$project$Generation$caveGenerator(
			{cave: caveType, ground: ground});
	}(
		function () {
			switch (caveType.$) {
				case 'CoalCave':
					return A2(
						$elm$random$Random$weighted,
						_Utils_Tuple2(
							1,
							$author$project$Data$World$insertItem($author$project$Data$Item$Coal)),
						_List_fromArray(
							[
								_Utils_Tuple2(
								1 / 2,
								$author$project$Data$World$insertActor(
									$author$project$Data$Actor$Helper(
										$author$project$Data$Actor$Falling(
											$author$project$Data$Entity$Vein($author$project$Data$Item$Coal)))))
							]));
				case 'IronCave':
					return A2(
						$elm$random$Random$weighted,
						_Utils_Tuple2(
							1,
							$author$project$Data$World$insertItem($author$project$Data$Item$Iron)),
						_List_fromArray(
							[
								_Utils_Tuple2(
								1 / 2,
								$author$project$Data$World$insertActor(
									$author$project$Data$Actor$Helper(
										$author$project$Data$Actor$Falling(
											$author$project$Data$Entity$Vein($author$project$Data$Item$Coal))))),
								_Utils_Tuple2(
								1 / 4,
								$author$project$Data$World$insertActor(
									$author$project$Data$Actor$Helper(
										$author$project$Data$Actor$Falling(
											$author$project$Data$Entity$Vein($author$project$Data$Item$Iron))))),
								_Utils_Tuple2(
								1 / 8,
								$author$project$Data$World$insertEntity($author$project$Data$Entity$Wall))
							]));
				case 'WaterCave':
					return A2(
						$elm$random$Random$weighted,
						_Utils_Tuple2(
							1,
							$author$project$Data$World$insertItem($author$project$Data$Item$Gold)),
						_List_fromArray(
							[
								_Utils_Tuple2(
								1 / 2,
								$author$project$Data$World$insertActor(
									$author$project$Data$Actor$Helper(
										$author$project$Data$Actor$Falling($author$project$Data$Entity$Water)))),
								_Utils_Tuple2(
								1 / 4,
								$author$project$Data$World$insertItem($author$project$Data$Item$Coal))
							]));
				case 'LavaCave':
					return A2(
						$elm$random$Random$weighted,
						_Utils_Tuple2(
							1,
							$author$project$Data$World$insertEntity($author$project$Data$Entity$Lava)),
						_List_fromArray(
							[
								_Utils_Tuple2(
								1 / 2,
								$author$project$Data$World$insertItem($author$project$Data$Item$Coal)),
								_Utils_Tuple2(
								1 / 4,
								$author$project$Data$World$insertItem($author$project$Data$Item$Gold))
							]));
				default:
					return A2(
						$elm$random$Random$weighted,
						_Utils_Tuple2(
							1,
							$author$project$Data$World$insertActor(
								$author$project$Data$Actor$Helper($author$project$Data$Actor$Path))),
						_List_fromArray(
							[
								_Utils_Tuple2(
								1 / 2,
								$author$project$Data$World$insertActor(
									$author$project$Data$Actor$Helper(
										$author$project$Data$Actor$Falling($author$project$Data$Entity$Wall)))),
								_Utils_Tuple2(
								1 / 4,
								$author$project$Data$World$insertActor(
									$author$project$Data$Actor$Helper(
										$author$project$Data$Actor$Falling(
											$author$project$Data$Entity$Vein($author$project$Data$Item$Coal))))),
								_Utils_Tuple2(
								1 / 8,
								$author$project$Data$World$insertActor(
									$author$project$Data$Actor$Helper(
										$author$project$Data$Actor$Falling(
											$author$project$Data$Entity$Vein($author$project$Data$Item$Gold)))))
							]));
			}
		}());
};
var $author$project$Data$Minecart$emptyWagon = {
	movedFrom: $elm$core$Maybe$Nothing,
	storage: $author$project$Data$Storage$empty($author$project$Config$wagonMaxItems)
};
var $author$project$Data$Storage$full = F2(
	function (maxAmount, item) {
		return {
			items: A2($elm$core$List$repeat, maxAmount, item),
			maxAmount: maxAmount
		};
	});
var $author$project$Data$Minecart$fullWagon = function (item) {
	return _Utils_update(
		$author$project$Data$Minecart$emptyWagon,
		{
			storage: A2($author$project$Data$Storage$full, $author$project$Config$wagonMaxItems, item)
		});
};
var $author$project$Data$World$insertFloor = F2(
	function (floor, pos) {
		return A2($author$project$Data$World$insertFloorAt, pos, floor);
	});
var $author$project$Generation$Mine$mineGenerator = F2(
	function (pos, world) {
		var _v0 = A2(
			$elm$core$List$filter,
			function (p) {
				return _Utils_eq(
					A2($author$project$Data$World$get, p, world),
					$elm$core$Maybe$Nothing);
			},
			$author$project$Data$Position$neighbors(pos));
		if (_v0.b) {
			var head = _v0.a;
			var tail = _v0.b;
			return A2(
				$elm$random$Random$andThen,
				function (stop) {
					return stop ? A2(
						$elm$random$Random$map,
						function (actor) {
							return A3(
								$author$project$Data$World$insertActor,
								actor,
								pos,
								A3(
									$elm$core$List$foldl,
									function (p) {
										return A2(
											$author$project$Data$World$insertEntity,
											$author$project$Data$Entity$Vein($author$project$Data$Item$Coal),
											p);
									},
									world,
									A2(
										$elm$core$List$filter,
										function (p) {
											return _Utils_eq(
												A2($author$project$Data$World$get, p, world),
												$elm$core$Maybe$Nothing);
										},
										$author$project$Data$Position$neighbors(pos))));
						},
						A2(
							$elm$random$Random$map,
							$author$project$Data$Actor$Minecart,
							A2(
								$elm$random$Random$map,
								$author$project$Data$Minecart$fullWagon,
								A2(
									$elm$random$Random$weighted,
									_Utils_Tuple2(1, $author$project$Data$Item$Coal),
									_List_Nil)))) : A2(
						$elm$random$Random$andThen,
						function (nextPos) {
							return A2(
								$elm$random$Random$map,
								A2($author$project$Data$World$insertFloor, $author$project$Data$Floor$Track, pos),
								A2(
									$elm$random$Random$map,
									$author$project$Data$World$removeEntity(pos),
									A3(
										$elm$core$List$foldl,
										function (p) {
											return $elm$random$Random$andThen(
												function (it) {
													return _Utils_eq(p, nextPos) ? $elm$random$Random$constant(
														A3(
															$author$project$Data$World$insertActor,
															$author$project$Data$Actor$Helper($author$project$Data$Actor$Mine),
															p,
															it)) : A2(
														$elm$random$Random$map,
														function (fun) {
															return A2(fun, p, it);
														},
														$author$project$Generation$Wall$wallGenerator(p));
												});
										},
										$elm$random$Random$constant(world),
										A2(
											$elm$core$List$filter,
											function (p) {
												return _Utils_eq(
													A2($author$project$Data$World$get, p, world),
													$elm$core$Maybe$Nothing);
											},
											$author$project$Data$Position$neighbors(pos)))));
						},
						A2($elm$random$Random$uniform, head, tail));
				},
				A2(
					$elm$random$Random$weighted,
					_Utils_Tuple2(1, false),
					_List_fromArray(
						[
							_Utils_Tuple2(1 / 4, true)
						])));
		} else {
			return $elm$random$Random$constant(
				A2($author$project$Data$World$removeEntity, pos, world));
		}
	});
var $author$project$Data$Actor$Bomb = function (a) {
	return {$: 'Bomb', a: a};
};
var $author$project$Data$Behavior$Bomb$getBomb = F2(
	function (id, world) {
		return A2(
			$elm$core$Maybe$andThen,
			function (_v0) {
				var pos = _v0.a;
				var actor = _v0.b;
				if (actor.$ === 'Bomb') {
					var bomb = actor.a;
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(pos, bomb));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			A2($author$project$Data$World$getActor, id, world));
	});
var $author$project$Data$Bomb$tick = function (bomb) {
	return (bomb.explodesIn > 1) ? $elm$core$Maybe$Just(
		{explodesIn: bomb.explodesIn - 1}) : $elm$core$Maybe$Nothing;
};
var $author$project$Data$Behavior$Bomb$timePassed = F2(
	function (id, world) {
		return A2(
			$elm$core$Maybe$withDefault,
			$elm$random$Random$constant(world),
			A2(
				$elm$core$Maybe$map,
				function (_v0) {
					var pos = _v0.a;
					var maybeBomb = _v0.b;
					if (maybeBomb.$ === 'Just') {
						var bomb = maybeBomb.a;
						return $elm$random$Random$constant(
							A3(
								$author$project$Data$World$setActor,
								id,
								$author$project$Data$Actor$Bomb(bomb),
								world));
					} else {
						return A3(
							$elm$core$List$foldl,
							function (p) {
								return $elm$random$Random$andThen(
									$author$project$Generation$mine(p));
							},
							$elm$random$Random$constant(
								A2($author$project$Data$World$removeEntity, pos, world)),
							$author$project$Data$Position$neighbors(pos));
					}
				},
				A2(
					$elm$core$Maybe$map,
					$elm$core$Tuple$mapSecond($author$project$Data$Bomb$tick),
					A2($author$project$Data$Behavior$Bomb$getBomb, id, world))));
	});
var $author$project$Data$Behavior$actorsAct = F3(
	function (_v0, improvements, world) {
		var id = _v0.a;
		var _v1 = _v0.b;
		var pos = _v1.a;
		var actor = _v1.b;
		switch (actor.$) {
			case 'Minecart':
				return A3($author$project$Data$Behavior$Minecart$act, id, improvements, world);
			case 'Bomb':
				return $author$project$Data$Effect$genWithNone(
					A2($author$project$Data$Behavior$Bomb$timePassed, id, world));
			case 'Helper':
				var helper = actor.a;
				switch (helper.$) {
					case 'Cave':
						var caveType = helper.a;
						return $author$project$Data$Effect$genWithNone(
							A3(
								$author$project$Generation$Cave$exposedCave,
								caveType,
								pos,
								A2($author$project$Data$World$removeEntity, pos, world)));
					case 'Mine':
						return $author$project$Data$Effect$genWithNone(
							A2(
								$author$project$Generation$Mine$mineGenerator,
								pos,
								A2($author$project$Data$World$removeEntity, pos, world)));
					case 'Falling':
						var entity = helper.a;
						return $author$project$Data$Effect$genWithNone(
							A3($author$project$Data$Behavior$Falling$act, entity, pos, world));
					default:
						return $author$project$Data$Effect$genWithNone(
							A2($author$project$Data$Behavior$Path$act, pos, world));
				}
			case 'Train':
				return A3($author$project$Data$Behavior$Train$act, improvements, id, world);
			default:
				return $author$project$Data$Effect$genWithNone(
					A2($author$project$Data$Behavior$MovingWater$act, id, world));
		}
	});
var $author$project$Data$Effect$andThen = function (fun) {
	return $elm$random$Random$andThen(
		function (_v0) {
			var a = _v0.a;
			var l = _v0.b;
			return A2(
				$elm$random$Random$map,
				$elm$core$Tuple$mapSecond(
					$elm$core$Basics$append(l)),
				fun(a));
		});
};
var $author$project$Data$World$getActors = function (world) {
	return $elm$core$Dict$toList(world.actors);
};
var $author$project$Data$Behavior$passTime = function (game) {
	return A2(
		$author$project$Data$Effect$andThen,
		function (g) {
			return A2(
				$elm$random$Random$map,
				$elm$core$Tuple$mapFirst(
					$author$project$Data$Game$setWorldOf(g)),
				A3(
					$elm$core$List$foldl,
					function (a) {
						return $author$project$Data$Effect$andThen(
							A2($author$project$Data$Behavior$actorsAct, a, game.improvements));
					},
					$elm$random$Random$constant(
						_Utils_Tuple2(g.world, _List_Nil)),
					$author$project$Data$World$getActors(g.world)));
		},
		$author$project$Data$Behavior$Player$act(game));
};
var $author$project$Data$Player$startMovingTo = F2(
	function (pos, player) {
		return _Utils_update(
			player,
			{
				targetPos: $elm$core$Maybe$Just(pos)
			});
	});
var $author$project$Data$Game$select = F2(
	function (pos, game) {
		return _Utils_update(
			game,
			{
				player: A2($author$project$Data$Player$startMovingTo, pos, game.player),
				selected: pos
			});
	});
var $elm$json$Json$Encode$float = _Json_wrap;
var $author$project$Main$setVolume = _Platform_outgoingPort('setVolume', $elm$json$Json$Encode$float);
var $author$project$Data$Modal$timePassed = function (m) {
	if (m.$ === 'TitleModal') {
		var modal = m.a;
		return $author$project$Data$Modal$TitleModal(
			_Utils_update(
				modal,
				{animationFrame: modal.animationFrame + 1}));
	} else {
		return m;
	}
};
var $author$project$Data$Sound$toFile = function (sound) {
	switch (sound.$) {
		case 'Build':
			return 'build.mp3';
		case 'PickUp':
			return 'pickup.mp3';
		case 'Unload':
			return 'unload.mp3';
		case 'MovingTrain':
			return 'movingTrain.mp3';
		case 'Destruct':
			return 'destruct.mp3';
		default:
			return 'error.mp3';
	}
};
var $author$project$Data$Sound$toString = function (sound) {
	switch (sound.$) {
		case 'Build':
			return 'Build';
		case 'PickUp':
			return 'PickUp';
		case 'Unload':
			return 'Unload';
		case 'MovingTrain':
			return 'MovingTrain';
		case 'Destruct':
			return 'Destruct';
		default:
			return 'Error';
	}
};
var $author$project$Data$Zoom$Zoom = function (a) {
	return {$: 'Zoom', a: a};
};
var $author$project$Data$Zoom$fromPercent = function (n) {
	var power = n / 100;
	return $author$project$Data$Zoom$Zoom(
		A2($elm$core$Basics$pow, 3, power));
};
var $author$project$Data$Zoom$get = function (_v0) {
	var v = _v0.a;
	return v;
};
var $elm$core$Basics$round = _Basics_round;
var $author$project$Config$height = function (zoom) {
	return $elm$core$Basics$round(
		15 * $author$project$Data$Zoom$get(zoom));
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $author$project$Config$maxCameraDistance = F3(
	function (z, w, h) {
		return $elm$core$Basics$round(
			A2(
				$elm$core$Basics$min,
				w(z),
				h(z)) / 4);
	});
var $author$project$Config$width = F2(
	function (widthOverHeight, zoom) {
		return $elm$core$Basics$round(
			(15 * $author$project$Data$Zoom$get(zoom)) * widthOverHeight);
	});
var $author$project$Main$updateCamera = function (model) {
	var _v0 = model.camera;
	var x = _v0.a;
	var y = _v0.b;
	var _v1 = model.game.player.pos;
	var pX = _v1.a;
	var pY = _v1.b;
	return (_Utils_cmp(
		$elm$core$Basics$abs(pX - x) + $elm$core$Basics$abs(pY - y),
		A3(
			$author$project$Config$maxCameraDistance,
			$author$project$Data$Zoom$fromPercent(model.zoomPercent),
			$author$project$Config$width(model.widthOverHeight),
			$author$project$Config$height)) > 0) ? _Utils_update(
		model,
		{
			camera: _Utils_Tuple2(pX, pY)
		}) : model;
};
var $author$project$Main$playSound = _Platform_outgoingPort('playSound', $elm$json$Json$Encode$string);
var $author$project$Main$updateGame = F2(
	function (fun, model) {
		return function (_v0) {
			var _v1 = _v0.a;
			var game = _v1.a;
			var list = _v1.b;
			var seed = _v0.b;
			return A2(
				$elm$core$Tuple$mapSecond,
				$elm$core$Platform$Cmd$batch,
				A3(
					$elm$core$List$foldl,
					function (effect) {
						switch (effect.$) {
							case 'PlaySound':
								var sound = effect.a;
								return $elm$core$Tuple$mapSecond(
									$elm$core$List$cons(
										$author$project$Main$playSound(
											$author$project$Data$Sound$toString(sound))));
							case 'OpenModal':
								var modal = effect.a;
								return $elm$core$Tuple$mapFirst(
									function (m) {
										return _Utils_update(
											m,
											{
												modal: $elm$core$Maybe$Just(modal)
											});
									});
							default:
								return $elm$core$Tuple$mapFirst(
									function (m) {
										return _Utils_update(
											m,
											{level: m.level + 1, tickInterval: m.tickInterval * 0.9});
									});
						}
					},
					_Utils_Tuple2(
						_Utils_update(
							model,
							{game: game, modal: $elm$core$Maybe$Nothing, seed: seed}),
						_List_Nil),
					list));
		}(
			A2(
				$elm$random$Random$step,
				fun(model.game),
				model.seed));
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'Restart':
				var seed = msg.a;
				return _Utils_Tuple2(
					A2($author$project$Main$restart, model.widthOverHeight, seed),
					$elm$core$Platform$Cmd$none);
			case 'TileClicked':
				var pos = msg.a;
				return A2(
					$author$project$Main$updateGame,
					function (game) {
						var _v1 = model.building;
						if (_v1.$ === 'Just') {
							var buildingMode = _v1.a;
							return $elm$random$Random$constant(
								A2(
									$elm$core$Maybe$withDefault,
									_Utils_Tuple2(
										model.game,
										_List_fromArray(
											[
												$author$project$Data$Effect$PlaySound($author$project$Data$Sound$Error)
											])),
									function () {
										switch (buildingMode.$) {
											case 'BuildingBlock':
												var _v3 = buildingMode.a;
												var cost = _v3.a;
												var block = _v3.b;
												return A4($author$project$Data$Game$buildBlock, pos, cost, block, game);
											case 'BuildingActor':
												var _v4 = buildingMode.a;
												var cost = _v4.a;
												var actor = _v4.b;
												return A4($author$project$Data$Game$buildActor, pos, cost, actor, game);
											default:
												return A2($author$project$Data$Game$destroyBlock, pos, game);
										}
									}()));
						} else {
							return $author$project$Data$Effect$withNone(
								A2($author$project$Data$Game$select, pos, game));
						}
					},
					model);
			case 'TimePassed':
				var _v5 = model.modal;
				if (_v5.$ === 'Just') {
					var modal = _v5.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								modal: $elm$core$Maybe$Just(
									$author$project$Data$Modal$timePassed(modal))
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_eq(model.building, $elm$core$Maybe$Nothing) ? A2(
						$author$project$Main$updateGame,
						$author$project$Data$Behavior$passTime,
						$author$project$Main$updateCamera(model)) : _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'StartBuilding':
				var a = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							building: $elm$core$Maybe$Just(a)
						}),
					$elm$core$Platform$Cmd$none);
			case 'StopBuilding':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{building: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'CloseModal':
				var maybeImprovement = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							game: A2(
								$elm$core$Maybe$withDefault,
								model.game,
								A2(
									$elm$core$Maybe$map,
									$author$project$Data$Game$addImprovementTo(model.game),
									maybeImprovement)),
							modal: $elm$core$Maybe$Nothing
						}),
					$elm$core$Platform$Cmd$batch(
						A2(
							$elm$core$List$map,
							function (sound) {
								return $author$project$Main$loadSound(
									_Utils_Tuple2(
										$author$project$Data$Sound$toFile(sound),
										$author$project$Data$Sound$toString(sound)));
							},
							$author$project$Data$Sound$asList)));
			case 'SetVolume':
				var amount = msg.a;
				return A2(
					$elm$core$Maybe$withDefault,
					_Utils_Tuple2(model, $elm$core$Platform$Cmd$none),
					A2(
						$elm$core$Maybe$map,
						function (_int) {
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{volume: _int}),
								$author$project$Main$setVolume(_int / 100));
						},
						amount));
			case 'SetTab':
				var tab = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{sidebarTab: tab}),
					$elm$core$Platform$Cmd$none);
			case 'SetZoom':
				var amount = msg.a;
				return A2(
					$elm$core$Maybe$withDefault,
					_Utils_Tuple2(model, $elm$core$Platform$Cmd$none),
					A2(
						$elm$core$Maybe$map,
						function (_float) {
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{zoomPercent: _float}),
								$elm$core$Platform$Cmd$none);
						},
						amount));
			default:
				var _float = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{widthOverHeight: _float}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Main$BuildingActor = function (a) {
	return {$: 'BuildingActor', a: a};
};
var $author$project$Main$BuildingBlock = function (a) {
	return {$: 'BuildingBlock', a: a};
};
var $author$project$Main$CloseModal = function (a) {
	return {$: 'CloseModal', a: a};
};
var $author$project$Main$RemovingBlock = {$: 'RemovingBlock'};
var $author$project$Main$SetTab = function (a) {
	return {$: 'SetTab', a: a};
};
var $author$project$Main$SetVolume = function (a) {
	return {$: 'SetVolume', a: a};
};
var $author$project$Main$SetZoom = function (a) {
	return {$: 'SetZoom', a: a};
};
var $author$project$Main$StartBuilding = function (a) {
	return {$: 'StartBuilding', a: a};
};
var $author$project$Main$StopBuilding = {$: 'StopBuilding'};
var $author$project$Main$TileClicked = function (a) {
	return {$: 'TileClicked', a: a};
};
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $Orasund$elm_layout$Layout$column = function (attrs) {
	return $elm$html$Html$div(
		_Utils_ap(
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'display', 'flex'),
					A2($elm$html$Html$Attributes$style, 'flex-direction', 'column')
				]),
			attrs));
};
var $Orasund$elm_layout$Layout$el = F2(
	function (attrs, content) {
		return A2(
			$elm$html$Html$div,
			A2(
				$elm$core$List$cons,
				A2($elm$html$Html$Attributes$style, 'display', 'flex'),
				attrs),
			_List_fromArray(
				[content]));
	});
var $Orasund$elm_layout$Layout$container = function (attrs) {
	return $Orasund$elm_layout$Layout$el(
		_Utils_ap(
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'width', '100%'),
					A2($elm$html$Html$Attributes$style, 'height', '100%'),
					A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
					A2($elm$html$Html$Attributes$style, 'left', '0px'),
					A2($elm$html$Html$Attributes$style, 'top', '0px')
				]),
			attrs));
};
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $author$project$Data$Info$new = function (args) {
	return {additionalInfo: _List_Nil, content: _List_Nil, description: args.description, title: args.title};
};
var $author$project$Data$Item$toChar = function (item) {
	switch (item.$) {
		case 'Coal':
			return _Utils_chr('⚫');
		case 'Iron':
			return _Utils_chr('🔘');
		default:
			return _Utils_chr('🟡');
	}
};
var $JohnBugner$elm_bag$Bag$Bag = function (a) {
	return {$: 'Bag', a: a};
};
var $JohnBugner$elm_bag$Bag$empty = $JohnBugner$elm_bag$Bag$Bag($elm$core$Dict$empty);
var $JohnBugner$elm_bag$Bag$dict = function (_v0) {
	var d = _v0.a;
	return d;
};
var $JohnBugner$elm_bag$Bag$insert = F3(
	function (n_, v, b) {
		var f = function (ma) {
			var n__ = n_ + A2($elm$core$Maybe$withDefault, 0, ma);
			return (n__ > 0) ? $elm$core$Maybe$Just(n__) : $elm$core$Maybe$Nothing;
		};
		return $JohnBugner$elm_bag$Bag$Bag(
			A3(
				$elm$core$Dict$update,
				v,
				f,
				$JohnBugner$elm_bag$Bag$dict(b)));
	});
var $JohnBugner$elm_bag$Bag$fromList = A2(
	$elm$core$List$foldl,
	$JohnBugner$elm_bag$Bag$insert(1),
	$JohnBugner$elm_bag$Bag$empty);
var $author$project$AnyBag$fromList = F2(
	function (encode, list) {
		return {
			content: $JohnBugner$elm_bag$Bag$fromList(
				A2($elm$core$List$map, encode, list)),
			encode: encode
		};
	});
var $JohnBugner$elm_bag$Bag$toAssociationList = function (b) {
	return $elm$core$Dict$toList(
		$JohnBugner$elm_bag$Bag$dict(b));
};
var $author$project$AnyBag$toAssociationList = function (bag) {
	return $JohnBugner$elm_bag$Bag$toAssociationList(bag.content);
};
var $author$project$Data$Item$toString = function (item) {
	switch (item.$) {
		case 'Coal':
			return 'Coal';
		case 'Iron':
			return 'Iron';
		default:
			return 'Gold';
	}
};
var $author$project$Data$Storage$toList = function (storage) {
	return $author$project$AnyBag$toAssociationList(
		A2($author$project$AnyBag$fromList, $author$project$Data$Item$toString, storage.items));
};
var $author$project$Data$Info$withContent = F2(
	function (content, info) {
		return _Utils_update(
			info,
			{content: content});
	});
var $author$project$Data$Info$fromActor = function (actor) {
	switch (actor.$) {
		case 'Minecart':
			var wagon = actor.a;
			return A2(
				$author$project$Data$Info$withContent,
				A2(
					$elm$core$List$map,
					function (_v1) {
						var k = _v1.a;
						var n = _v1.b;
						return $elm$core$String$fromInt(n) + ('x ' + k);
					},
					$author$project$Data$Storage$toList(wagon.storage)),
				$author$project$Data$Info$new(
					{
						description: 'Can store up to ' + ($elm$core$String$fromInt($author$project$Config$wagonMaxItems) + ' items. You can also push it along.'),
						title: 'Minecart'
					}));
		case 'Helper':
			return $author$project$Data$Info$new(
				{description: 'Helper Block used to generate content', title: 'Helper'});
		case 'Bomb':
			var explodesIn = actor.a.explodesIn;
			return $author$project$Data$Info$new(
				{
					description: 'Explodes in ' + ($elm$core$String$fromInt(explodesIn) + ' turns.'),
					title: 'Bomb'
				});
		case 'Train':
			var train = actor.a;
			return A2(
				$author$project$Data$Info$withContent,
				A2(
					$elm$core$List$map,
					function (_v2) {
						var k = _v2.a;
						var n = _v2.b;
						return $elm$core$String$fromInt(n) + ('x ' + ($elm$core$String$fromChar(
							$author$project$Data$Item$toChar(k)) + $author$project$Data$Item$toString(k)));
					},
					train.items),
				$author$project$Data$Info$new(
					{description: 'Stores all your items. If it has tracks stored, it will place them and move forward. Needs coal to move. Will regularly fetch new tracks from above ground.', title: 'Train'}));
		default:
			return $author$project$Data$Info$new(
				{description: 'Will stop moving as soon as possible', title: 'Moving Water'});
	}
};
var $author$project$Data$Info$fromEntity = F2(
	function (game, entity) {
		switch (entity.$) {
			case 'Vein':
				var item = entity.a;
				return $author$project$Data$Info$new(
					{
						description: 'Drops one ' + ($author$project$Data$Item$toString(item) + ' when mined.'),
						title: $author$project$Data$Item$toString(item) + ' Vein'
					});
			case 'Wall':
				return $author$project$Data$Info$new(
					{description: 'Can be mind by bombs, but will not drop anything.', title: 'Wall'});
			case 'Water':
				return $author$project$Data$Info$new(
					{description: 'Will be pushed aside when you walk through it. Wagons can\'t pass through it.', title: 'Water'});
			case 'Lava':
				return $author$project$Data$Info$new(
					{description: 'Gets removed if water gets moved into it.', title: 'Lava'});
			default:
				var id = entity.a;
				return A2(
					$elm$core$Maybe$withDefault,
					$author$project$Data$Info$new(
						{description: 'This is a bug. Please report how to managed to create this entity', title: 'Unkown Actor'}),
					A2(
						$elm$core$Maybe$map,
						function (_v1) {
							var actor = _v1.b;
							return $author$project$Data$Info$fromActor(actor);
						},
						A2($elm$core$Dict$get, id, game.world.actors)));
		}
	});
var $author$project$Data$Info$fromFloor = function (floor) {
	switch (floor.$) {
		case 'Ground':
			return $author$project$Data$Info$new(
				{description: 'May contain an item that can be picked up.', title: 'Ground'});
		case 'Track':
			return $author$project$Data$Info$new(
				{description: 'Pushed wagons will automatically move to adjacent tracks', title: 'Track'});
		default:
			return $author$project$Data$Info$new(
				{description: 'Trains will move along railway tracks', title: 'Railway Track'});
	}
};
var $author$project$Data$Info$fromBlock = F2(
	function (game, block) {
		if (block.$ === 'FloorBlock') {
			var floor = block.a;
			return $author$project$Data$Info$fromFloor(floor);
		} else {
			var entity = block.a;
			return A2($author$project$Data$Info$fromEntity, game, entity);
		}
	});
var $Orasund$elm_layout$Layout$noWrap = A2($elm$html$Html$Attributes$style, 'flex-wrap', 'nowrap');
var $Orasund$elm_layout$Layout$row = function (attrs) {
	return $elm$html$Html$div(
		_Utils_ap(
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'display', 'flex'),
					A2($elm$html$Html$Attributes$style, 'flex-direction', 'row'),
					A2($elm$html$Html$Attributes$style, 'flex-wrap', 'wrap')
				]),
			attrs));
};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $Orasund$elm_layout$Layout$asButton = function (args) {
	return _Utils_ap(
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
				A2($elm$html$Html$Attributes$attribute, 'aria-label', args.label),
				A2($elm$html$Html$Attributes$attribute, 'role', 'button')
			]),
		A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2(
				$elm$core$Maybe$map,
				function (msg) {
					return _List_fromArray(
						[
							$elm$html$Html$Events$onClick(msg)
						]);
				},
				args.onPress)));
};
var $author$project$View$Color$black = 'Black';
var $author$project$View$Color$gray = 'Gray';
var $author$project$Data$Tile$ImageTile = function (a) {
	return {$: 'ImageTile', a: a};
};
var $author$project$Data$Tile$image = function (args) {
	return $author$project$Data$Tile$ImageTile(
		{animation: false, color: args.color, source: args.source});
};
var $author$project$Data$Tile$CharTile = function (a) {
	return {$: 'CharTile', a: a};
};
var $author$project$Data$Tile$new = function (args) {
	return $author$project$Data$Tile$CharTile(
		{animation: false, bold: false, color: args.color, content: args.content, size: 1});
};
var $author$project$Data$Tile$fromFloor = F3(
	function (pos, game, floor) {
		switch (floor.$) {
			case 'Ground':
				return $author$project$Data$Tile$new(
					{
						color: $author$project$View$Color$gray,
						content: _Utils_chr(' ')
					});
			case 'Track':
				return function (list) {
					if ((((list.b && list.b.b) && list.b.b.b) && list.b.b.b.b) && (!list.b.b.b.b.b)) {
						var down = list.a;
						var _v3 = list.b;
						var left = _v3.a;
						var _v4 = _v3.b;
						var up = _v4.a;
						var _v5 = _v4.b;
						var right = _v5.a;
						return $author$project$Data$Tile$image(
							{color: $author$project$View$Color$gray, source: 'assets/svg/track/track_' + (down + (left + (up + (right + '.svg'))))});
					} else {
						return $author$project$Data$Tile$image(
							{color: $author$project$View$Color$gray, source: 'assets/svg/track/track_1111.svg'});
					}
				}(
					A2(
						$elm$core$List$map,
						function (p) {
							return A2(
								$elm$core$Maybe$withDefault,
								'0',
								A2(
									$elm$core$Maybe$map,
									function (track) {
										switch (track.$) {
											case 'Track':
												return '1';
											case 'RailwayTrack':
												return '1';
											default:
												return '0';
										}
									},
									A2($author$project$Data$World$getFloor, p, game.world)));
						},
						$author$project$Data$Position$neighbors(pos)));
			default:
				return $author$project$Data$Tile$image(
					{color: $author$project$View$Color$gray, source: 'assets/svg/railwayTrack.svg'});
		}
	});
var $author$project$View$Color$green = 'Green';
var $elm$core$String$toLower = _String_toLower;
var $author$project$Data$Tile$withAnimation = function (tile) {
	if (tile.$ === 'CharTile') {
		var content = tile.a;
		return $author$project$Data$Tile$CharTile(
			_Utils_update(
				content,
				{animation: true}));
	} else {
		var content = tile.a;
		return $author$project$Data$Tile$ImageTile(
			_Utils_update(
				content,
				{animation: true}));
	}
};
var $author$project$Data$Tile$fromPlayer = F2(
	function (game, player) {
		return _List_fromArray(
			[
				A3(
				$author$project$Data$Tile$fromFloor,
				player.pos,
				game,
				A2(
					$elm$core$Maybe$withDefault,
					$author$project$Data$Floor$Ground,
					A2($author$project$Data$World$getFloor, player.pos, game.world))),
				function (source) {
				return $author$project$Data$Tile$withAnimation(
					$author$project$Data$Tile$image(
						{color: $author$project$View$Color$green, source: source}));
			}(
				A2(
					$elm$core$Maybe$withDefault,
					'assets/svg/player.svg',
					A2(
						$elm$core$Maybe$map,
						function (item) {
							return 'assets/svg/player_' + ($elm$core$String$toLower(item) + '.svg');
						},
						A2($elm$core$Maybe$map, $author$project$Data$Item$toString, player.item))))
			]);
	});
var $author$project$View$Color$blue = 'Blue';
var $author$project$Config$bombExplosionTime = 10;
var $author$project$Data$Storage$isEmpty = function (storage) {
	return _Utils_eq(storage.items, _List_Nil);
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $author$project$View$Color$red = 'Red';
var $author$project$Data$Tile$withBold = function (tile) {
	if (tile.$ === 'CharTile') {
		var content = tile.a;
		return $author$project$Data$Tile$CharTile(
			_Utils_update(
				content,
				{bold: true}));
	} else {
		return tile;
	}
};
var $author$project$Data$Tile$fromActor = function (actor) {
	switch (actor.$) {
		case 'Minecart':
			var wagon = actor.a;
			return function (it) {
				return $author$project$Data$Storage$isFull(wagon.storage) ? $author$project$Data$Tile$withBold(
					$author$project$Data$Tile$withAnimation(it)) : $author$project$Data$Tile$withBold(it);
			}(
				function (source) {
					return $author$project$Data$Tile$image(
						{color: $author$project$View$Color$black, source: source});
				}(
					$author$project$Data$Storage$isEmpty(wagon.storage) ? 'assets/svg/minecart.svg' : 'assets/svg/minecart_full.svg'));
		case 'Helper':
			return $author$project$Data$Tile$withBold(
				$author$project$Data$Tile$new(
					{
						color: $author$project$View$Color$red,
						content: _Utils_chr('?')
					}));
		case 'Bomb':
			var bomb = actor.a;
			return function (source) {
				return $author$project$Data$Tile$image(
					{color: $author$project$View$Color$red, source: source});
			}(
				((_Utils_cmp(bomb.explodesIn, ($author$project$Config$bombExplosionTime / 2) | 0) > 0) || ((_Utils_cmp(bomb.explodesIn, $author$project$Config$bombExplosionTime) < 0) && (!A2($elm$core$Basics$modBy, 2, bomb.explodesIn)))) ? 'assets/svg/bomb_active.svg' : 'assets/svg/bomb.svg');
		case 'Train':
			var train = actor.a;
			return ((train.moving || (train.tracks > 0)) ? $author$project$Data$Tile$withBold : $elm$core$Basics$identity)(
				function (source) {
					return $author$project$Data$Tile$image(
						{color: $author$project$View$Color$black, source: source});
				}('assets/svg/train.svg'));
		default:
			return $author$project$Data$Tile$withAnimation(
				$author$project$Data$Tile$image(
					{color: $author$project$View$Color$blue, source: 'assets/svg/water.svg'}));
	}
};
var $author$project$Data$Tile$fromEntity = F2(
	function (game, entity) {
		switch (entity.$) {
			case 'Vein':
				var item = entity.a;
				return $elm$core$List$singleton(
					$author$project$Data$Tile$image(
						{
							color: $author$project$View$Color$black,
							source: 'assets/svg/vein_' + ($elm$core$String$toLower(
								$author$project$Data$Item$toString(item)) + '.svg')
						}));
			case 'Wall':
				return $elm$core$List$singleton(
					$author$project$Data$Tile$image(
						{color: $author$project$View$Color$black, source: 'assets/svg/wall.svg'}));
			case 'Water':
				return $elm$core$List$singleton(
					$author$project$Data$Tile$image(
						{color: $author$project$View$Color$blue, source: 'assets/svg/water.svg'}));
			case 'Lava':
				return $elm$core$List$singleton(
					$author$project$Data$Tile$image(
						{color: $author$project$View$Color$red, source: 'assets/svg/lava.svg'}));
			default:
				var id = entity.a;
				return A2(
					$elm$core$Maybe$withDefault,
					$elm$core$List$singleton(
						$author$project$Data$Tile$new(
							{
								color: $author$project$View$Color$red,
								content: _Utils_chr('?')
							})),
					A2(
						$elm$core$Maybe$map,
						function (_v1) {
							var pos = _v1.a;
							var actor = _v1.b;
							return _List_fromArray(
								[
									A3(
									$author$project$Data$Tile$fromFloor,
									pos,
									game,
									A2(
										$elm$core$Maybe$withDefault,
										$author$project$Data$Floor$Ground,
										A2($author$project$Data$World$getFloor, pos, game.world))),
									$author$project$Data$Tile$fromActor(actor)
								]);
						},
						A2($author$project$Data$World$getActor, id, game.world)));
		}
	});
var $author$project$Data$Tile$fromItem = function (item) {
	return $elm$core$List$singleton(
		function (source) {
			return $author$project$Data$Tile$image(
				{color: $author$project$View$Color$gray, source: source});
		}(
			'assets/svg/item_' + ($elm$core$String$toLower(
				$author$project$Data$Item$toString(item)) + '.svg')));
};
var $author$project$Data$Tile$fromPos = F2(
	function (pos, game) {
		return A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2(
				$elm$core$Maybe$map,
				function (_v0) {
					var block = _v0.a;
					var items = _v0.b;
					if (block.$ === 'FloorBlock') {
						var floor = block.a;
						return A2(
							$elm$core$List$cons,
							A3($author$project$Data$Tile$fromFloor, pos, game, floor),
							A2(
								$elm$core$Maybe$withDefault,
								_List_Nil,
								A2($elm$core$Maybe$map, $author$project$Data$Tile$fromItem, items)));
					} else {
						var entity = block.a;
						return A2($author$project$Data$Tile$fromEntity, game, entity);
					}
				},
				A2($author$project$Data$World$get, pos, game.world)));
	});
var $Orasund$elm_layout$Layout$stack = F2(
	function (attrs, list) {
		return A2(
			$elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'flex'),
						A2($elm$html$Html$Attributes$style, 'position', 'relative')
					]),
				attrs),
			A2(
				$elm$core$List$map,
				function (_v0) {
					var attr = _v0.a;
					var content = _v0.b;
					return A2(
						$elm$html$Html$div,
						A2(
							$elm$core$List$cons,
							A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
							attr),
						_List_fromArray(
							[content]));
				},
				list));
	});
var $author$project$Config$tileSize = F2(
	function (widthOverHeight, zoom) {
		return 'min(' + (('100vw/' + ($elm$core$String$fromInt(
			A2($author$project$Config$width, widthOverHeight, zoom)) + ', ')) + (('100vh/' + $elm$core$String$fromInt(
			$author$project$Config$height(zoom))) + ')'));
	});
var $Orasund$elm_layout$Layout$alignAtCenter = A2($elm$html$Html$Attributes$style, 'align-items', 'center');
var $Orasund$elm_layout$Layout$contentCentered = A2($elm$html$Html$Attributes$style, 'justify-content', 'center');
var $Orasund$elm_layout$Layout$centered = _List_fromArray(
	[$Orasund$elm_layout$Layout$contentCentered, $Orasund$elm_layout$Layout$alignAtCenter]);
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$core$String$fromFloat = _String_fromNumber;
var $author$project$Config$fontSize = F3(
	function (widthOverHeight, size, zoom) {
		return 'min(' + (($elm$core$String$fromFloat(100 * size) + ('vw/' + ($elm$core$String$fromInt(
			A2($author$project$Config$width, widthOverHeight, zoom)) + ', '))) + (($elm$core$String$fromFloat(100 * size) + ('vh/' + $elm$core$String$fromInt(
			$author$project$Config$height(zoom)))) + ')'));
	});
var $elm$html$Html$img = _VirtualDom_node('img');
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$View$Tile$toHtml = F3(
	function (widthOverHeight, zoom, tile) {
		if (tile.$ === 'CharTile') {
			var content = tile.a.content;
			var color = tile.a.color;
			var size = tile.a.size;
			var bold = tile.a.bold;
			var animation = tile.a.animation;
			return A2(
				$Orasund$elm_layout$Layout$el,
				_Utils_ap(
					_List_fromArray(
						[
							A2(
							$elm$html$Html$Attributes$style,
							'width',
							A2($author$project$Config$tileSize, widthOverHeight, zoom)),
							A2(
							$elm$html$Html$Attributes$style,
							'height',
							A2($author$project$Config$tileSize, widthOverHeight, zoom)),
							A2(
							$elm$html$Html$Attributes$style,
							'font-size',
							A3($author$project$Config$fontSize, widthOverHeight, size, zoom)),
							A2($elm$html$Html$Attributes$style, 'color', color)
						]),
					_Utils_ap(
						bold ? _List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'font-weight', 'bold')
							]) : _List_Nil,
						_Utils_ap(
							animation ? _List_fromArray(
								[
									$elm$html$Html$Attributes$class('animate__animated animate__pulse animate__infinite animate__faster')
								]) : _List_Nil,
							$Orasund$elm_layout$Layout$centered))),
				$elm$html$Html$text(
					$elm$core$String$fromChar(content)));
		} else {
			var source = tile.a.source;
			var animation = tile.a.animation;
			return A2(
				$elm$html$Html$img,
				_Utils_ap(
					_List_fromArray(
						[
							$elm$html$Html$Attributes$src(source),
							A2(
							$elm$html$Html$Attributes$style,
							'width',
							A2($author$project$Config$tileSize, widthOverHeight, zoom)),
							A2(
							$elm$html$Html$Attributes$style,
							'height',
							A2($author$project$Config$tileSize, widthOverHeight, zoom))
						]),
					animation ? _List_fromArray(
						[
							$elm$html$Html$Attributes$class('animate__animated animate__pulse animate__infinite animate__faster')
						]) : _List_Nil),
				_List_Nil);
		}
	});
var $author$project$Data$Tile$wall = $author$project$Data$Tile$new(
	{
		color: $author$project$View$Color$black,
		content: _Utils_chr(' ')
	});
var $author$project$View$Color$yellow = 'Yellow';
var $author$project$View$Screen$tile = F3(
	function (args, pos, game) {
		return function (list) {
			return A2(
				$Orasund$elm_layout$Layout$stack,
				_Utils_ap(
					_Utils_eq(game.selected, pos) ? _List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'background-color', $author$project$View$Color$yellow)
						]) : (_Utils_eq(list, _List_Nil) ? _List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'background-color', $author$project$View$Color$black)
						]) : _List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'background-color', 'white')
						])),
					_Utils_ap(
						A2(
							$elm$core$Maybe$withDefault,
							_List_Nil,
							A2(
								$elm$core$Maybe$map,
								function (_v0) {
									return $Orasund$elm_layout$Layout$asButton(
										{label: 'Activate', onPress: args.onPress});
								},
								args.onPress)),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$Attributes$style,
								'width',
								A2($author$project$Config$tileSize, args.widthOverHeight, args.zoom)),
								A2(
								$elm$html$Html$Attributes$style,
								'height',
								A2($author$project$Config$tileSize, args.widthOverHeight, args.zoom))
							]))),
				A2(
					$elm$core$List$map,
					function (html) {
						return _Utils_Tuple2(_List_Nil, html);
					},
					A2(
						$elm$core$List$map,
						A2($author$project$View$Tile$toHtml, args.widthOverHeight, args.zoom),
						$elm$core$List$isEmpty(list) ? _List_fromArray(
							[$author$project$Data$Tile$wall]) : list)));
		}(
			_Utils_eq(pos, game.player.pos) ? A2($author$project$Data$Tile$fromPlayer, game, game.player) : A2($author$project$Data$Tile$fromPos, pos, game));
	});
var $author$project$View$Screen$fromGame = F2(
	function (args, game) {
		return A2(
			$Orasund$elm_layout$Layout$column,
			_List_Nil,
			A2(
				$elm$core$List$map,
				function (y) {
					return A2(
						$Orasund$elm_layout$Layout$row,
						_List_fromArray(
							[$Orasund$elm_layout$Layout$noWrap]),
						A2(
							$elm$core$List$map,
							function (x) {
								var _v0 = args.camera;
								var playerX = _v0.a;
								var playerY = _v0.b;
								var pos = _Utils_Tuple2(
									(playerX + x) - ((A2($author$project$Config$width, args.widthOverHeight, args.zoom) / 2) | 0),
									(playerY + y) - (($author$project$Config$height(args.zoom) / 2) | 0));
								return A3(
									$author$project$View$Screen$tile,
									{
										onPress: $elm$core$Maybe$Just(
											args.onPress(pos)),
										widthOverHeight: args.widthOverHeight,
										zoom: args.zoom
									},
									pos,
									game);
							},
							A2(
								$elm$core$List$range,
								0,
								A2($author$project$Config$width, args.widthOverHeight, args.zoom) - 1)));
				},
				A2(
					$elm$core$List$range,
					0,
					$author$project$Config$height(args.zoom) - 1)));
	});
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $Orasund$elm_layout$Layout$none = $elm$html$Html$text('');
var $elm$html$Html$Attributes$rel = _VirtualDom_attribute('rel');
var $author$project$Config$bombCost = 2;
var $author$project$View$Tab$buildActorButton = F2(
	function (buildActor, args) {
		return {
			build: $author$project$Data$Info$fromActor(args.actor).title,
			cost: args.cost,
			onPress: buildActor(args)
		};
	});
var $author$project$View$Tab$buildBlockButton = F3(
	function (buildBlock, game, args) {
		return {
			build: A2($author$project$Data$Info$fromBlock, game, args.block).title,
			cost: args.cost,
			onPress: buildBlock(args)
		};
	});
var $Orasund$elm_layout$Layout$spaceBetween = A2($elm$html$Html$Attributes$style, 'justify-content', 'space-between');
var $Orasund$elm_layout$Layout$asEl = A2($elm$html$Html$Attributes$style, 'display', 'flex');
var $elm$html$Html$button = _VirtualDom_node('button');
var $Orasund$elm_layout$Layout$buttonEl = F3(
	function (args, attrs, content) {
		return A2(
			$elm$html$Html$button,
			A2(
				$elm$core$List$cons,
				$Orasund$elm_layout$Layout$asEl,
				_Utils_ap(
					$Orasund$elm_layout$Layout$asButton(args),
					attrs)),
			_List_fromArray(
				[content]));
	});
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $author$project$View$Button$toHtml = F2(
	function (onPress, label) {
		return A3(
			$Orasund$elm_layout$Layout$buttonEl,
			{label: label, onPress: onPress},
			function () {
				if (onPress.$ === 'Just') {
					return _List_Nil;
				} else {
					return _List_fromArray(
						[
							$elm$html$Html$Attributes$disabled(true)
						]);
				}
			}(),
			$elm$html$Html$text(label));
	});
var $author$project$View$Tab$buildButton = F2(
	function (game, args) {
		var _v0 = args.cost;
		var item = _v0.a;
		var cost = _v0.b;
		var gotAmount = A2(
			$author$project$ListBag$count,
			item,
			$author$project$Data$Game$getTrain(game).items);
		return A2(
			$Orasund$elm_layout$Layout$row,
			_List_fromArray(
				[$Orasund$elm_layout$Layout$spaceBetween]),
			_List_fromArray(
				[
					$elm$html$Html$text(args.build),
					A2(
					$author$project$View$Button$toHtml,
					(_Utils_cmp(cost, gotAmount) < 1) ? $elm$core$Maybe$Just(args.onPress) : $elm$core$Maybe$Nothing,
					'Build for ' + ($elm$core$String$fromInt(cost) + (' ' + $elm$core$String$fromChar(
						$author$project$Data$Item$toChar(item)))))
				]));
	});
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $Orasund$elm_layout$Layout$heading3 = F2(
	function (attrs, content) {
		return A2(
			$elm$html$Html$h3,
			A2(
				$elm$core$List$cons,
				A2($elm$html$Html$Attributes$style, 'display', 'flex'),
				attrs),
			_List_fromArray(
				[content]));
	});
var $author$project$Data$Bomb$new = {explodesIn: $author$project$Config$bombExplosionTime};
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$Attributes$max = $elm$html$Html$Attributes$stringProperty('max');
var $elm$html$Html$Attributes$min = $elm$html$Html$Attributes$stringProperty('min');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$string = _Json_decodeString;
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $Orasund$elm_layout$Layout$spacing = function (n) {
	return A2(
		$elm$html$Html$Attributes$style,
		'gap',
		$elm$core$String$fromFloat(n) + 'px');
};
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$View$Tab$Settings$percentRange = function (args) {
	return A2(
		$Orasund$elm_layout$Layout$row,
		_List_fromArray(
			[
				$Orasund$elm_layout$Layout$spacing(8)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(args.name),
				A2(
				$elm$html$Html$input,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$type_('range'),
						$elm$html$Html$Attributes$min('0'),
						$elm$html$Html$Attributes$max('100'),
						$elm$html$Html$Attributes$value(
						$elm$core$String$fromInt(args.value)),
						$elm$html$Html$Events$onInput(
						function (_int) {
							return args.onInput(
								$elm$core$String$toInt(_int));
						})
					]),
				_List_Nil)
			]));
};
var $author$project$View$Tab$Settings$settings = function (args) {
	return A2(
		$Orasund$elm_layout$Layout$column,
		_List_fromArray(
			[
				$Orasund$elm_layout$Layout$spacing(8)
			]),
		_List_fromArray(
			[
				A2(
				$author$project$View$Button$toHtml,
				$elm$core$Maybe$Just(args.restart),
				'Restarts'),
				$author$project$View$Tab$Settings$percentRange(
				{name: 'Volume', onInput: args.setVolume, value: args.volume}),
				$author$project$View$Tab$Settings$percentRange(
				{name: 'Zoom', onInput: args.setZoom, value: args.zoom})
			]));
};
var $author$project$View$Tab$BuildTab = {$: 'BuildTab'};
var $author$project$View$Tab$SettingTab = {$: 'SettingTab'};
var $author$project$View$Tab$tabList = _List_fromArray(
	[$author$project$View$Tab$SettingTab, $author$project$View$Tab$DetailTab, $author$project$View$Tab$BuildTab]);
var $elm$html$Html$p = _VirtualDom_node('p');
var $Orasund$elm_layout$Layout$paragraph = F2(
	function (attrs, content) {
		return A2(
			$elm$html$Html$p,
			A2(
				$elm$core$List$cons,
				A2($elm$html$Html$Attributes$style, 'display', 'flex'),
				attrs),
			_List_fromArray(
				[content]));
	});
var $author$project$View$Info$toHtml = function (info) {
	return A2(
		$Orasund$elm_layout$Layout$column,
		_List_Nil,
		_Utils_ap(
			_List_fromArray(
				[
					$elm$html$Html$text('Selected: ' + info.title),
					A2(
					$Orasund$elm_layout$Layout$paragraph,
					_List_Nil,
					$elm$html$Html$text(info.description)),
					A2(
					$Orasund$elm_layout$Layout$el,
					_List_Nil,
					$elm$html$Html$text(
						_Utils_eq(info.content, _List_Nil) ? 'Contains no items' : ('Contains ' + A2($elm$core$String$join, ', ', info.content))))
				]),
			A2(
				$elm$core$List$map,
				function (string) {
					return A2(
						$Orasund$elm_layout$Layout$el,
						_List_Nil,
						$elm$html$Html$text(string));
				},
				info.additionalInfo)));
};
var $author$project$View$Tab$toString = function (tab) {
	switch (tab.$) {
		case 'SettingTab':
			return 'Settings';
		case 'DetailTab':
			return 'Detail';
		default:
			return 'Build';
	}
};
var $author$project$Config$trackCost = 1;
var $author$project$View$Tab$sidebar = F2(
	function (args, game) {
		var selected = A2($author$project$Data$World$get, game.selected, game.world);
		return A2(
			$Orasund$elm_layout$Layout$column,
			_List_fromArray(
				[
					$Orasund$elm_layout$Layout$spacing(8)
				]),
			_List_fromArray(
				[
					A2(
					$Orasund$elm_layout$Layout$row,
					_List_fromArray(
						[
							$Orasund$elm_layout$Layout$spacing(8)
						]),
					A2(
						$elm$core$List$map,
						function (tab) {
							return A2(
								$author$project$View$Button$toHtml,
								_Utils_eq(
									$elm$core$Maybe$Just(tab),
									args.tab) ? $elm$core$Maybe$Just(
									args.setTab($elm$core$Maybe$Nothing)) : $elm$core$Maybe$Just(
									args.setTab(
										$elm$core$Maybe$Just(tab))),
								$author$project$View$Tab$toString(tab));
						},
						$author$project$View$Tab$tabList)),
					A2(
					$elm$core$Maybe$withDefault,
					$Orasund$elm_layout$Layout$none,
					A2(
						$elm$core$Maybe$map,
						function (tab) {
							return A2(
								$Orasund$elm_layout$Layout$column,
								_List_fromArray(
									[
										$Orasund$elm_layout$Layout$spacing(8),
										A2($elm$html$Html$Attributes$style, 'width', '200px'),
										A2($elm$html$Html$Attributes$style, 'background-color', 'white'),
										A2($elm$html$Html$Attributes$style, 'padding', '8px'),
										A2($elm$html$Html$Attributes$style, 'border-radius', '8px'),
										A2($elm$html$Html$Attributes$style, 'border', 'solid 1px black')
									]),
								_List_fromArray(
									[
										A2(
										$Orasund$elm_layout$Layout$heading3,
										_List_Nil,
										$elm$html$Html$text(
											$author$project$View$Tab$toString(tab))),
										function () {
										switch (tab.$) {
											case 'SettingTab':
												return $author$project$View$Tab$Settings$settings(
													{restart: args.restart, setVolume: args.setVolume, setZoom: args.setZoom, volume: args.volume, zoom: args.zoom});
											case 'DetailTab':
												return A2(
													$elm$core$Maybe$withDefault,
													$elm$html$Html$text('Nothing selected'),
													A2(
														$elm$core$Maybe$map,
														function (_v1) {
															var block = _v1.a;
															return A2(
																$Orasund$elm_layout$Layout$column,
																_List_Nil,
																A2(
																	$elm$core$List$cons,
																	$author$project$View$Info$toHtml(
																		A2($author$project$Data$Info$fromBlock, game, block)),
																	function () {
																		_v2$2:
																		while (true) {
																			if (block.$ === 'EntityBlock') {
																				if (block.a.$ === 'Actor') {
																					var id = block.a.a;
																					var _v3 = A2($author$project$Data$World$getActor, id, game.world);
																					if ((_v3.$ === 'Just') && (_v3.a.b.$ === 'Minecart')) {
																						var _v4 = _v3.a;
																						return true;
																					} else {
																						return false;
																					}
																				} else {
																					break _v2$2;
																				}
																			} else {
																				if (block.a.$ === 'Track') {
																					var _v5 = block.a;
																					return true;
																				} else {
																					break _v2$2;
																				}
																			}
																		}
																		return false;
																	}() ? $elm$core$List$singleton(
																		A2(
																			$author$project$View$Button$toHtml,
																			$elm$core$Maybe$Just(args.destroyBlock),
																			'Destroy')) : _List_Nil));
														},
														selected));
											default:
												return A2(
													$Orasund$elm_layout$Layout$column,
													_List_fromArray(
														[
															$Orasund$elm_layout$Layout$spacing(8)
														]),
													_Utils_ap(
														A2(
															$elm$core$List$map,
															$author$project$View$Tab$buildButton(game),
															_List_fromArray(
																[
																	A2(
																	$author$project$View$Tab$buildActorButton,
																	args.buildActor,
																	{
																		actor: $author$project$Data$Actor$Minecart($author$project$Data$Minecart$emptyWagon),
																		cost: _Utils_Tuple2($author$project$Data$Item$Iron, $author$project$Config$wagonCost)
																	}),
																	A2(
																	$author$project$View$Tab$buildActorButton,
																	args.buildActor,
																	{
																		actor: $author$project$Data$Actor$Bomb($author$project$Data$Bomb$new),
																		cost: _Utils_Tuple2($author$project$Data$Item$Gold, $author$project$Config$bombCost)
																	})
																])),
														_Utils_ap(
															A2(
																$elm$core$List$map,
																$author$project$View$Tab$buildButton(game),
																_List_fromArray(
																	[
																		A3(
																		$author$project$View$Tab$buildBlockButton,
																		args.buildBlock,
																		game,
																		{
																			block: $author$project$Data$Block$FloorBlock($author$project$Data$Floor$Track),
																			cost: _Utils_Tuple2($author$project$Data$Item$Iron, $author$project$Config$trackCost)
																		})
																	])),
															_List_fromArray(
																[
																	A2(
																	$author$project$View$Button$toHtml,
																	$elm$core$Maybe$Just(args.destroyBlock),
																	'Destroy')
																]))));
										}
									}()
									]));
						},
						args.tab))
				]));
	});
var $author$project$Data$Zoom$none = $author$project$Data$Zoom$Zoom(1);
var $author$project$View$Screen$animation = F2(
	function (args, game) {
		return A2(
			$Orasund$elm_layout$Layout$column,
			_List_Nil,
			A2(
				$elm$core$List$map,
				function (y) {
					return A2(
						$Orasund$elm_layout$Layout$row,
						_List_fromArray(
							[$Orasund$elm_layout$Layout$noWrap]),
						A2(
							$elm$core$List$map,
							function (x) {
								return A3(
									$author$project$View$Screen$tile,
									{onPress: $elm$core$Maybe$Nothing, widthOverHeight: args.widthOverHeight, zoom: $author$project$Data$Zoom$none},
									_Utils_Tuple2(x, y),
									game);
							},
							A2($elm$core$List$range, 0, args.width - 1)));
				},
				A2($elm$core$List$range, 0, args.height - 1)));
	});
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $author$project$View$Animation$fromArray = F2(
	function (array, i) {
		return A2(
			$elm$core$Maybe$withDefault,
			$elm$html$Html$text(''),
			A2(
				$elm$core$Array$get,
				A2(
					$elm$core$Basics$modBy,
					$elm$core$Array$length(array),
					i),
				array));
	});
var $elm$core$Elm$JsArray$map = _JsArray_map;
var $elm$core$Array$map = F2(
	function (func, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = function (node) {
			if (node.$ === 'SubTree') {
				var subTree = node.a;
				return $elm$core$Array$SubTree(
					A2($elm$core$Elm$JsArray$map, helper, subTree));
			} else {
				var values = node.a;
				return $elm$core$Array$Leaf(
					A2($elm$core$Elm$JsArray$map, func, values));
			}
		};
		return A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A2($elm$core$Elm$JsArray$map, helper, tree),
			A2($elm$core$Elm$JsArray$map, func, tail));
	});
var $author$project$View$Animation$animate = F2(
	function (widthOverHeight, animation) {
		return $author$project$View$Animation$fromArray(
			A2(
				$elm$core$Array$map,
				$author$project$View$Screen$animation(
					{height: animation.height, width: animation.width, widthOverHeight: widthOverHeight}),
				animation.frames));
	});
var $Orasund$elm_layout$Layout$centerContent = A2($elm$html$Html$Attributes$style, 'justify-content', 'center');
var $author$project$View$Title$coal = '\n       CCCCCCC  OOOOOOOOOO  AA  LLLL\n      C#####C  O########O  A##A  L##L\n     C##CCCC  O##OOOO##O  A####A  L##L\n    C##C     O##O  O##O  A##AA##A  L##L\n   C##C     O##O  O##O  A##A  A##A  L##L\n  C##CCCC  O##OOOO##O  A##AAAAAA##A  L##LLLL\n C#####C  O########O  A##A      A##A  L#####L\nCCCCCCC  OOOOOOOOOO  AAAA        AAAA  LLLLLLLL\n    ';
var $author$project$View$Title$crawl = '\n       CCCCCCC  RRRRRRRRRR  AA  WWWW            WWWW  LLLL\n      C.....C  R........R  A..A  W..W          W..W  L..L\n     C..CCCC  R..RRRR..R  A....A  W..W        W..W  L..L\n    C..C     R..R  R..R  A..AA..A  W..W  WW  W..W  L..L\n   C..C     R..RRRR..R  A..A  A..A  W..WW..WW..W  L..L\n  C..CCCC  R......RRR  A..AAAAAA..A  W........W  L..LLLL\n C.....C  R..RRR..R   A..A      A..A  W..WW..W  L......L\nCCCCCCC  RRRR  RRRR  AAAA        AAAA  WW  WW  LLLLLLLLL\n    ';
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $Orasund$elm_layout$Layout$heading2 = F2(
	function (attrs, content) {
		return A2(
			$elm$html$Html$h2,
			A2(
				$elm$core$List$cons,
				A2($elm$html$Html$Attributes$style, 'display', 'flex'),
				attrs),
			_List_fromArray(
				[content]));
	});
var $author$project$Config$maxLevel = 9;
var $elm$html$Html$pre = _VirtualDom_node('pre');
var $author$project$Data$Improvement$toString = function (improvement) {
	switch (improvement.$) {
		case 'GetOneGoldEachLevel':
			return 'Get one gold each level';
		case 'MinecartCanCollect':
			return 'Minecart can collect items';
		default:
			return 'Train can collect items';
	}
};
var $author$project$Data$Animation$emptyWorld = function (args) {
	return $author$project$Data$World$fromList(
		A2(
			$elm$core$List$concatMap,
			function (y) {
				return A2(
					$elm$core$List$map,
					function (x) {
						return _Utils_Tuple2(
							_Utils_Tuple2(x, y),
							$author$project$Data$Block$FloorBlock($author$project$Data$Floor$Ground));
					},
					A2($elm$core$List$range, 0, args.width - 1));
			},
			A2($elm$core$List$range, 0, args.height - 1)));
};
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $author$project$Data$Animation$tutorial = function () {
	var width = 4;
	var train = _Utils_Tuple2(3, 1);
	var selections = $elm$core$Dict$fromList(
		$elm$core$List$reverse(
			A3(
				$elm$core$List$foldl,
				F2(
					function (_v3, _v4) {
						var i = _v3.a;
						var p = _v3.b;
						var l = _v4.a;
						var time = _v4.b;
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(i + time, p),
								l),
							i + time);
					}),
				_Utils_Tuple2(_List_Nil, 0),
				_List_fromArray(
					[
						_Utils_Tuple2(
						0,
						_Utils_Tuple2(0, 1)),
						_Utils_Tuple2(5, train),
						_Utils_Tuple2(
						5,
						_Utils_Tuple2(1, 0)),
						_Utils_Tuple2(5, train),
						_Utils_Tuple2(
						5,
						_Utils_Tuple2(1, 2)),
						_Utils_Tuple2(5, train)
					])).a));
	var player = _Utils_Tuple2(2, 1);
	var height = 3;
	var initGame = {
		improvements: _List_Nil,
		player: $author$project$Data$Player$fromPos(player),
		selected: player,
		trainId: 0,
		world: A3(
			$author$project$Data$World$insertActor,
			$author$project$Data$Actor$Train(
				$author$project$Data$Train$fromPos(train)),
			train,
			A3(
				$author$project$Data$World$insertEntity,
				$author$project$Data$Entity$Vein($author$project$Data$Item$Coal),
				_Utils_Tuple2(1, 2),
				A3(
					$author$project$Data$World$insertEntity,
					$author$project$Data$Entity$Vein($author$project$Data$Item$Coal),
					_Utils_Tuple2(0, 1),
					A3(
						$author$project$Data$World$insertEntity,
						$author$project$Data$Entity$Vein($author$project$Data$Item$Coal),
						_Utils_Tuple2(1, 0),
						$author$project$Data$Animation$emptyWorld(
							{height: height, width: width})))))
	};
	return {
		frames: $elm$core$Array$fromList(
			$elm$core$List$reverse(
				A3(
					$elm$core$List$foldl,
					F2(
						function (_int, _v0) {
							var _v1 = _v0.a;
							var game = _v1.a;
							var seed = _v1.b;
							var l = _v0.b;
							return _Utils_Tuple2(
								A2(
									$elm$core$Tuple$mapFirst,
									function (_v2) {
										var g = _v2.a;
										return A2(
											$elm$core$Maybe$withDefault,
											g,
											A2(
												$elm$core$Maybe$map,
												function (pos) {
													return A2($author$project$Data$Game$select, pos, g);
												},
												A2($elm$core$Dict$get, _int, selections)));
									},
									A2(
										$elm$random$Random$step,
										$author$project$Data$Behavior$passTime(game),
										seed)),
								A2($elm$core$List$cons, game, l));
						}),
					_Utils_Tuple2(
						_Utils_Tuple2(
							initGame,
							$elm$random$Random$initialSeed(42)),
						_List_Nil),
					A2($elm$core$List$range, 0, 30)).b)),
		height: height,
		width: width
	};
}();
var $author$project$View$Modal$toHtml = F5(
	function (widthOverHeight, closeModal, game, m, level) {
		return A2(
			$Orasund$elm_layout$Layout$column,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'padding', '16px'),
					A2($elm$html$Html$Attributes$style, 'width', '300px'),
					A2($elm$html$Html$Attributes$style, 'background-color', 'white'),
					A2($elm$html$Html$Attributes$style, 'border-radius', '16px'),
					A2($elm$html$Html$Attributes$style, 'border', 'solid 1px black')
				]),
			function () {
				if (m.$ === 'TitleModal') {
					var modal = m.a;
					return _List_fromArray(
						[
							A2(
							$Orasund$elm_layout$Layout$row,
							_List_fromArray(
								[$Orasund$elm_layout$Layout$centerContent]),
							A2(
								$elm$core$List$map,
								function (string) {
									return A2(
										$elm$html$Html$pre,
										_List_fromArray(
											[
												A2($elm$html$Html$Attributes$style, 'font-family', 'monospace'),
												A2($elm$html$Html$Attributes$style, 'font-size', '8px')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(string)
											]));
								},
								_List_fromArray(
									[$author$project$View$Title$coal, $author$project$View$Title$crawl]))),
							A2(
							$Orasund$elm_layout$Layout$el,
							$Orasund$elm_layout$Layout$centered,
							A3($author$project$View$Animation$animate, widthOverHeight, $author$project$Data$Animation$tutorial, modal.animationFrame)),
							A2(
							$Orasund$elm_layout$Layout$paragraph,
							_List_Nil,
							$elm$html$Html$text('Reach the bottom of the cave. Collect coal to progress.')),
							A2(
							$author$project$View$Button$toHtml,
							$elm$core$Maybe$Just(
								closeModal($elm$core$Maybe$Nothing)),
							'Continue')
						]);
				} else {
					var improvements = m.a;
					return (_Utils_cmp(level, $author$project$Config$maxLevel) < 0) ? _Utils_ap(
						_List_fromArray(
							[
								A2(
								$Orasund$elm_layout$Layout$heading2,
								_List_fromArray(
									[$Orasund$elm_layout$Layout$contentCentered]),
								$elm$html$Html$text('Level Completed')),
								A2(
								$Orasund$elm_layout$Layout$el,
								_List_Nil,
								$elm$html$Html$text(
									'Items collected sofar: ' + A2(
										$elm$core$String$join,
										', ',
										A2(
											$elm$core$List$map,
											function (_v1) {
												var k = _v1.a;
												var n = _v1.b;
												return $elm$core$String$fromInt(n) + ('x ' + ($elm$core$String$fromChar(
													$author$project$Data$Item$toChar(k)) + $author$project$Data$Item$toString(k)));
											},
											$author$project$Data$Game$getTrain(game).items))))
							]),
						$elm$core$List$isEmpty(improvements) ? $elm$core$List$singleton(
							A2(
								$author$project$View$Button$toHtml,
								$elm$core$Maybe$Just(
									closeModal($elm$core$Maybe$Nothing)),
								'Continue')) : _List_fromArray(
							[
								A2(
								$Orasund$elm_layout$Layout$heading2,
								_List_Nil,
								$elm$html$Html$text('Choose an improvement')),
								A2(
								$Orasund$elm_layout$Layout$row,
								_List_fromArray(
									[
										$Orasund$elm_layout$Layout$spacing(8)
									]),
								A2(
									$elm$core$List$map,
									function (improvement) {
										return A2(
											$author$project$View$Button$toHtml,
											$elm$core$Maybe$Just(
												closeModal(
													$elm$core$Maybe$Just(improvement))),
											$author$project$Data$Improvement$toString(improvement));
									},
									improvements))
							])) : _List_fromArray(
						[
							A2(
							$Orasund$elm_layout$Layout$heading2,
							_List_fromArray(
								[$Orasund$elm_layout$Layout$contentCentered]),
							$elm$html$Html$text('GAME WON!')),
							A2(
							$Orasund$elm_layout$Layout$el,
							_List_Nil,
							$elm$html$Html$text(
								'Items collected sofar: ' + A2(
									$elm$core$String$join,
									', ',
									A2(
										$elm$core$List$map,
										function (_v2) {
											var k = _v2.a;
											var n = _v2.b;
											return $elm$core$String$fromInt(n) + ('x ' + ($elm$core$String$fromChar(
												$author$project$Data$Item$toChar(k)) + $author$project$Data$Item$toString(k)));
										},
										$author$project$Data$Game$getTrain(game).items)))),
							A2(
							$Orasund$elm_layout$Layout$heading2,
							_List_Nil,
							$elm$html$Html$text('You reached the end of the cave')),
							A2(
							$author$project$View$Button$toHtml,
							$elm$core$Maybe$Just(
								closeModal($elm$core$Maybe$Nothing)),
							'Continue')
						]);
				}
			}());
	});
var $Orasund$elm_layout$Layout$withStack = F3(
	function (attrs, list, base) {
		return A2(
			$elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'flex'),
						A2($elm$html$Html$Attributes$style, 'position', 'relative')
					]),
				attrs),
			A2(
				$elm$core$List$cons,
				base,
				A2(
					$elm$core$List$map,
					function (_v0) {
						var attr = _v0.a;
						var content = _v0.b;
						return A2(
							$elm$html$Html$div,
							A2(
								$elm$core$List$cons,
								A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
								attr),
							_List_fromArray(
								[content]));
					},
					list)));
	});
var $author$project$Main$view = function (model) {
	var train = $author$project$Data$Game$getTrain(model.game);
	return {
		body: _List_fromArray(
			[
				A3(
				$elm$html$Html$node,
				'link',
				_List_fromArray(
					[
						$elm$html$Html$Attributes$rel('stylesheet'),
						$elm$html$Html$Attributes$href('https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css')
					]),
				_List_Nil),
				A2(
				$Orasund$elm_layout$Layout$container,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'background-color', 'black')
					]),
				A3(
					$Orasund$elm_layout$Layout$withStack,
					_List_Nil,
					_List_fromArray(
						[
							_Utils_Tuple2(
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'top', '50%'),
									A2($elm$html$Html$Attributes$style, 'left', '50%'),
									A2($elm$html$Html$Attributes$style, 'transform', 'translate(-50%,-50%)')
								]),
							function () {
								var _v7 = model.modal;
								if (_v7.$ === 'Just') {
									var modal = _v7.a;
									return A5($author$project$View$Modal$toHtml, model.widthOverHeight, $author$project$Main$CloseModal, model.game, modal, model.level);
								} else {
									return $Orasund$elm_layout$Layout$none;
								}
							}())
						]),
					A3(
						$Orasund$elm_layout$Layout$withStack,
						_Utils_ap(
							(!_Utils_eq(model.modal, $elm$core$Maybe$Nothing)) ? _List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'backdrop-filter', 'brightness(0.5)')
								]) : _List_Nil,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'position', 'relative')
								])),
						_List_fromArray(
							[
								_Utils_Tuple2(
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'top', '8px'),
										A2($elm$html$Html$Attributes$style, 'left', '8px')
									]),
								function () {
									var _v0 = model.building;
									if (_v0.$ === 'Just') {
										var block = _v0.a;
										return A2(
											$Orasund$elm_layout$Layout$column,
											_List_fromArray(
												[
													$Orasund$elm_layout$Layout$spacing(8)
												]),
											_List_fromArray(
												[
													A2(
													$author$project$View$Button$toHtml,
													$elm$core$Maybe$Just($author$project$Main$StopBuilding),
													'Stop Building'),
													A2(
													$Orasund$elm_layout$Layout$el,
													_List_fromArray(
														[
															A2($elm$html$Html$Attributes$style, 'background-color', 'white'),
															A2($elm$html$Html$Attributes$style, 'padding', '8px'),
															A2($elm$html$Html$Attributes$style, 'border-radius', '8px'),
															A2($elm$html$Html$Attributes$style, 'border', 'solid 1px black')
														]),
													$elm$html$Html$text(
														function () {
															switch (block.$) {
																case 'BuildingBlock':
																	var _v2 = block.a;
																	var b = _v2.b;
																	return A2($author$project$Data$Info$fromBlock, model.game, b).title;
																case 'BuildingActor':
																	var _v3 = block.a;
																	var a = _v3.b;
																	return $author$project$Data$Info$fromActor(a).title;
																default:
																	return 'Removing';
															}
														}()))
												]));
									} else {
										return A2(
											$author$project$View$Tab$sidebar,
											{
												buildActor: function (_v4) {
													var cost = _v4.cost;
													var actor = _v4.actor;
													return $author$project$Main$StartBuilding(
														$author$project$Main$BuildingActor(
															_Utils_Tuple2(cost, actor)));
												},
												buildBlock: function (_v5) {
													var cost = _v5.cost;
													var block = _v5.block;
													return $author$project$Main$StartBuilding(
														$author$project$Main$BuildingBlock(
															_Utils_Tuple2(cost, block)));
												},
												destroyBlock: $author$project$Main$StartBuilding($author$project$Main$RemovingBlock),
												restart: $author$project$Main$Restart(model.seed),
												setTab: $author$project$Main$SetTab,
												setVolume: $author$project$Main$SetVolume,
												setZoom: $author$project$Main$SetZoom,
												tab: model.sidebarTab,
												volume: model.volume,
												zoom: model.zoomPercent
											},
											model.game);
									}
								}()),
								_Utils_Tuple2(
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'top', '8px'),
										A2($elm$html$Html$Attributes$style, 'right', '8px')
									]),
								A2(
									$Orasund$elm_layout$Layout$column,
									_List_fromArray(
										[
											A2($elm$html$Html$Attributes$style, 'background-color', 'white'),
											A2($elm$html$Html$Attributes$style, 'padding', '8px'),
											A2($elm$html$Html$Attributes$style, 'border-radius', '8px'),
											A2($elm$html$Html$Attributes$style, 'border', 'solid 1px black')
										]),
									_List_fromArray(
										[
											A2(
											$Orasund$elm_layout$Layout$el,
											_List_Nil,
											$elm$html$Html$text(
												function (content) {
													return A2($elm$core$String$join, ', ', content);
												}(
													A2(
														$elm$core$List$map,
														function (_v6) {
															var k = _v6.a;
															var n = _v6.b;
															return $elm$core$String$fromInt(n) + ('x ' + k);
														},
														A2(
															$elm$core$List$cons,
															_Utils_Tuple2('Tracks', train.tracks),
															A2(
																$elm$core$List$map,
																$elm$core$Tuple$mapFirst(
																	function (k) {
																		return _Utils_ap(
																			$elm$core$String$fromChar(
																				$author$project$Data$Item$toChar(k)),
																			$author$project$Data$Item$toString(k));
																	}),
																train.items)))))),
											A2(
											$Orasund$elm_layout$Layout$el,
											_List_Nil,
											$elm$html$Html$text(
												'Needs ' + ($elm$core$String$fromInt(
													$author$project$Data$Train$coalNeeded(train) - A2($author$project$ListBag$count, $author$project$Data$Item$Coal, train.items)) + ' for the next Level')))
										])))
							]),
						A2(
							$author$project$View$Screen$fromGame,
							{
								camera: model.camera,
								onPress: $author$project$Main$TileClicked,
								widthOverHeight: model.widthOverHeight,
								zoom: $author$project$Data$Zoom$fromPercent(model.zoomPercent)
							},
							model.game))))
			]),
		title: '⛏Coal Crawl'
	};
};
var $author$project$Main$main = $elm$browser$Browser$document(
	{init: $author$project$Main$init, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));