#reader"../lang/reader.ss"

export *;

var global = this;

var Array = global.Array;
var Object = global.Object;
var String = global.String;
var Function = global.Function;
var Math = global.Math;
var Number = global.Number;
var Boolean = global.Boolean;
var Trace = global.Trace;
var Name = global.Name;

var NaN = global.NaN;
var Infinity = global.Infinity;
var undefined = global.undefined;

var eval = global.eval;
var parseInt = global.parseInt;
var parseFloat = global.parseFloat;
var isNaN = global.isNaN;
var isFinite = global.isFinite;
var decodeURI = global.decodeURI;
var decodeURIComponent = global.decodeURIComponent;
var encodeURI = global.encodeURI;
var encodeURIComponent = global.encodeURIComponent;

var print = global.print || function() { throw 'no console available' };
var alert = global.alert || function() { throw 'no GUI available' };
