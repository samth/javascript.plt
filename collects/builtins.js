#reader"../lang/reader.ss"

import file("../private/runtime/value.ss").{ 'object-class': objectClass,
                                             'has-own-property?' : hasOwnProperty,
                                             'object-proto' : objectProto,
                                             'object?' : isObject,
                                             'object-get-attributes' : objectGetAttributes,
                                             'bit-flag-set?' : isBitFlagSet,
                                             'DONT-ENUM?' : DONT_ENUM,
                                             'proto:Function' : proto_Function,
                                             'proto:Array' : proto_Array,
                                             'new-Array' : newArray,
                                             'object-get' : get,
                                             'object-put!' : put,
                                             'object-delete!' : del,
                                             'any->uint32' : ToUint32,
                                             'any->string' : ToString,
                                             'any->integer' : ToInteger,
                                             'any->property-name' : ToName,
                                             'any->object' : ToObject,
                                             'has-property?' : HasProperty };
import scheme/base.{ 'eq?' : schemeEq,
                     'display' : schemeDisplay,
                     'write' : schemeWrite,
                     'void' : schemeVoid,
                     'build-list' : schemeBuildList,
                     'values' : schemeValues,
                     'map' : schemeMap,
                     'floor' : schemeFloor,
                     'max' : schemeMax,
                     'min' : schemeMin,
                     'string<?' : schemeStringLT };
import scheme/string.{ 'string-join' : stringJoin };

/* private */ var undefined = void(0);

/* private */ function ensureInstance(object, proto, expected) {
    if (!proto.isPrototypeOf(object))
        throw ("expected " + expected + ", receieved object");
}

/* private */ function arrayLength(a) {
    return get(this, 'length', {|| => 0 }, ToUint32);
}

/* private */ function mapN(len, f) {
    schemeMap(f, schemeBuildList(len, schemeValues));
}

// =============================================================================
// GLOBAL OBJECT
// =============================================================================

var global = {

};

// =============================================================================
// OBJECT PROTOTYPE
// =============================================================================

var Object = {
    toString : function() {
        return "[object " + objectClass(this) + "]";
    },
    toLocaleString : function() {
        return this.toString();
    },
    valueOf : function() { return this },
    hasOwnProperty : function(x) { return hasOwnProperty(this, ToName(x)) },
    isPrototypeOf : function(x) {
        if (!isObject(x)) return false;
        do {
            if (schemeEq(this, x)) return true;
            x = objectProto(x);
        } while (x);
        return false;
    },
    propertyIsEnumerable : function(key) {
        var attributes = objectGetAttributes(this, key);
        return attributes && isBitFlagSet(attributes, DONT_ENUM);
    }
};

// =============================================================================
// FUNCTION PROTOTYPE
// =============================================================================

var Function = {
    toString : function() {
        ensureInstance(this, proto_Function, "function");
        return "[object Function]";
    },
    apply : function(obj, args) { throw "not yet implemented" },
    call : function(obj) { throw "not yet implemented" }
};

// =============================================================================
// ARRAY PROTOTYPE
// =============================================================================

var Array = {
    write : function(out) {
        var length = arrayLength(this);
        for (var i = 0; i < length; i++) {
            if (i > 0)
                schemeDisplay(',', out);
            get(this, ToName(i), schemeVoid, {|x| schemeWrite(x, out) });
        }
    },
    display : function(out) {
        var length = arrayLength(this);
        for (var i = 0; i < length; i++) {
            if (i > 0)
                schemeDisplay(',', out);
            get(this, ToName(i), schemeVoid, {|x| schemeDisplay(x, out) });
        }
    },
    // 15.4.4.2
    toString : function() {
        var length = arrayLength(this);
        var result = "";
        for (var i = 0; i < length; i++) {
            if (i > 0)
                result += ",";
            result += get(this, ToName(i), {|| => ""}, ToString);
        }
        return result;
    },
    // 15.4.4.3
    toLocaleString : function() {
        ensureInstance(this, proto_Array, "array");
        var length = arrayLength(this);
        var result = "";
        for (var i = 0; i < length; i++) {
            if (i > 0)
                result += ",";
            result += get(this, ToName(i), {|| => ""}, {|v| => ToObject(v).toLocaleString() });
        }
        return result;
    },
    // 15.4.4.4
    concat : function(a1) {
        var A = newArray();
        var k, n = 0;
        for (k = 0; k < this.length; k++, n++) {
            var Result8 = ToName(k);
            if (HasProperty(this, Result8))
                put(A, ToName(n), this[Result8]);
        }
        for (var i = 0; i < arguments.length; i++) {
            var E = arguments[i];
            for (var k = 0; k < E.length; k++, n++) {
                var Result8 = ToName(k);
                if (HasProperty(this, Result8))
                    put(A, ToName(n), E[Result8]);
            }
        }
        return A;
    },
    // 15.4.4.5
    join : function(separator) {
        if (separator === undefined || separator === null)
            separator = ',';
        var self = this;
        return stringJoin(mapN(this.length, {|i| get(self,
                                                     ToName(i),
                                                     {|| => ""},
                                                     {|v| => (v === undefined || v === null) ? "" : ToString(v) }) }),
                               separator);
    },
    // 15.4.4.6
    pop : function() {
        var length = arrayLength(this);
        if (length === 0) {
            this.length = length;
            return undefined;
        }
        var key = ToName(length - 1);
        var val = get(this, key, schemeVoid);
        del(this, key);
        put(this, 'length', length - 1);
        return val;
    },
    // 15.4.4.7
    push : function(first) {
        var length = arrayLength(this);
        var newLength = length + arguments.length;
        for (var i = 0; i < arguments.length; i++) {
            put(this, ToName(i + length), arguments[i]);
        }
        put(this, 'length', newLength);
        return newLength;
    },
    // 15.4.4.8
    reverse : function() {
        var length = arrayLength(this);
        var half = schemeFloor(length / 2);
        for (var left = 0; left < half; left++) {
            var right = length - left - 1;
            var leftKey = ToName(left);
            var rightKey = ToName(right);
            get(this,
                leftKey,
                {|| get(this,
                        rightKey,
                        {|| del(this, leftKey); del(this, rightKey) },
                        {|rightVal| del(this, rightKey); put(this, leftKey, rightVal) })},
                {|leftVal| get(this,
                               rightKey,
                               {|| put(this, rightKey, leftVal); del(this, rightKey) },
                               {|rightVal| put(this, leftKey, rightVal); put(this, rightKey, leftVal) }) });
        }
    },
    // 15.4.4.9
    shift : function() {
        var length = arrayLength(this);
        if (length === 0) {
            put(this, 'length', length);
            return undefined;
        }
        var removed = get(this, '0');
        for (var k = 0; k < length; k++) {
            var kKey = ToName(k);
            var kMinus1Key = ToName(k - 1);
            get(this,
                kKey,
                {|| del(this, kMinus1Key)},
                {|val| put(this, kMinus1Key, val)});
        }
        del(this, ToName(length - 1));
        put(this, 'length', length - 1);
        return removed;
    },
    // 15.4.4.10
    slice : function(start, end) {
        var length = arrayLength(this);
        var A = newArray();
        var ToIndex = {|x| let i = ToInteger(x); => (i < 0) ? schemeMax(i + length, 0) : schemeMin(i, length)};
        start = ToIndex(start);
        end = ToIndex(end);
        var k, n;
        for (k = start, n = 0; k < end; k++, n++) {
            var kKey = ToName(k);
            if (HasProperty(this, kKey)) {
                put(A, ToName(n), get(this, kKey));
            }
        }
        put(A, 'length', n);
        return A;
    },
    // 15.4.4.11
    sort : function(comparefn) {
        var self = this;
        function SortCompare(j, k) {
            var jKey = ToName(j);
            var kKey = ToName(k);
            var hasJ = HasProperty(self, jKey);
            var hasK = HasProperty(self, kKey);
            if (!hasJ && !hasK) return 0;
            else if (hasJ) return 1;
            else if (hasK) return -1;
            else {
                let x = get(self, jKey);
                let y = get(self, kKey);
                if (x === undefined && y === undefined) return 0;
                else if (x === undefined) return 1;
                else if (y === undefined) return -1;
                else if (comparefn === undefined) {
                    let xStr = ToString(x);
                    let yStr = ToString(y);
                    if (schemeStringLT(xStr, yStr)) return 1;
                    else if (schemeStringLT(yStr, xStr)) return -1;
                    else return 0;
                }
                else return comparefn(x, y);
            }
        }
        function quicksort(p, r) {
            if (p < r) {
                let q = partition(p, r);
                quicksort(p, q);
                quicksort(q + 1, r);
            }
        }
        function partition(p, r) {
            let i = p - 1;
            let j = r + 1;
            do {
                while (ToInteger(SortCompare(j, p)) > 0) j--;
                while (ToInteger(SortCompare(i, p)) < 0) i++;
                if (i < j) swap(i, j);
            } while (i < j);
            return j;
        }
        function swap(i, j) {
            let iKey = ToName(i);
            let jKey = ToName(j);
            let hasI = HasProperty(this, iKey);
            let hasJ = HasProperty(this, jKey);
            if (!hasI && !hasJ) return;
            else if (!hasI) {
                let jVal = get(this, jKey);
                del(this, jKey);
                put(this, iKey, jVal);
            }
            else if (!hasJ) {
                let iVal = get(this, iKey);
                del(this, iKey);
                put(this, jKey, iVal);
            }
            else {
                let iVal = get(this, iKey);
                let jVal = get(this, jKey);
                put(this, iKey, jVal);
                put(this, jKey, iVal);
            }
        }
        quicksort(0, arrayLength(this) - 1);
        return this;
    },
    // 15.4.4.12
    splice : function(start, deleteCount) {
        var length = arrayLength(this);
        var A = newArray();
        start = (start === undefined) ? 0 : ToInteger(start);
        deleteCount = (deleteCount === undefined) ? 0 : ToInteger(deleteCount);
        start = (start < 0) ? schemeMax(length + start, 0) : schemeMin(start, length); // 5
        deleteCount = schemeMin(schemeMax(deleteCount, 0), length - start); // 6
        // 7 - 15
        for (let k = 0; k < deleteCount; k++) {
            let key = ToName(start + k);
            if (HasProperty(this, key))
                put(A, ToName(k), get(this, key));
        }
        // 16
        put(A, 'length', deleteCount);
        // 17
        let insertCount = schemeMax(arguments.length - 2, 0);
        let newLength = (length - deleteCount) + insertCount;
        // 18 - 19
        if (insertCount < deleteCount) {
            // 20 - 30
            for (let k = start; k < (length - deleteCount); k++) {
                let fromKey = ToName(k + deleteCount);
                let toKey = ToName(k + insertCount);
                if (HasProperty(this, fromKey))
                    put(this, toKey, get(this, fromKey));
                else
                    del(this, toKey);
            }
            // 31 - 36
            for (let k = length; k < newLength; k++) {
                del(this, ToName(k - 1));
            }
        }
        else if (insertCount > deleteCount) {
            // 37 - 47
            for (let k = length - deleteCount; k > start; k--) {
                let fromKey = ToName(k + deleteCount - 1);
                let toKey = ToName(k + insertCount - 1);
                if (HasProperty(this, fromKey))
                    put(this, toKey, get(this, fromKey));
                else
                    del(this, toKey);
            }
        }
        // 48 - 52
        for (let i = 0; i < insertCount; i++) {
            put(this, ToName(start + i), arguments[i]);
        }
        // 53
        put(this, 'length', newLength);
        // 54
        return A;
    },
    // 15.4.4.13
    unshift : function(first) {
        var length = arrayLength(this); // 2
        var count = arguments.length; // 3
        // 4 - 14
        for (let k = length; k > 0; k--) {
            let fromKey = ToName(k - 1);
            let toKey = ToName(count + k - 1);
            if (HasProperty(this, fromKey))
                put(this, toKey, get(this, fromKey));
            else
                del(this, toKey);
        }
        // 15 - 20
        for (let k = 0; k < count; k++) {
            put(this, ToName(k), arguments[k]);
        }
        let newLength = length + count;
        // 21
        put(this, 'length', newLength);
        return newLength;
    }
};

// =============================================================================
// STRING PROTOTYPE
// =============================================================================

var String = {
};

// =============================================================================
// BOOLEAN PROTOTYPE
// =============================================================================

var Boolean = {
};

// =============================================================================
// NUMBER PROTOTYPE
// =============================================================================

var Number = {
};

// =============================================================================
// MATH OBJECT
// =============================================================================

var Math = {
};
