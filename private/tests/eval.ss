#lang scheme/base

(require rackunit
         "../../runtime.ss"
         "../../eval.ss"
         "../../config.ss"
         "util.ss")

(provide eval-tests)

(enable-let-expressions? #t)

(define binding-tests
  (test-suite "binding tests"
    (test-case "top-level binding"
      (check-output '("true")
                    "var a = true;"
                    "print(a);"))
    (test-case "non-with lexical binding"
      (check-output '("true")
                    "(function(a){print(a)})(true)"))
    (test-case "non-with catch binding"
      (check-output '("true")
                    "try{throw true}catch(a){print(a)}"))
    (test-case "non-with let binding"
      (check-output '("true")
                    "let (a = true){print(a)}"))
    (test-case "non-with lexical shadowing of top-level"
      (check-output '("true")
                    "var a = false;"
                    "(function(a){print(a)})(true);"))
    (test-case "non-with lexical shadowing of lexical"
      (check-output '("true")
                    "(function(a){(function(a){print(a)})(true)})(false);"))
    (test-case "non-with lexical shadowing of catch"
      (check-output '("true")
                    "try{throw false}catch(a){(function(a){print(a)})(true)}"))
    (test-case "non-with lexical shadowing of let"
      (check-output '("true")
                    "let (a = false){(function(a){print(a)})(true)}"))
    (test-case "non-with catch shadowing of top-level"
      (check-output '("true")
                    "var a = false"
                    "try{throw true}catch(a){print(a)}"))
    (test-case "non-with catch shadowing of lexical"
      (check-output '("true")
                    "(function(a){try{throw true}catch(a){print(a)}})(false)"))
    (test-case "non-with catch shadowing of catch"
      (check-output '("true")
                    "try{throw false}catch(a){try{throw true}catch(a){print(a)}}"))
    (test-case "non-with catch shadowing of let"
      (check-output '("true")
                    "let (a = false){try{throw true}catch(a){print(a)}}"))
    #;(test-case "non-with let shadowing of top-level"
        (check-output '("true")
                      ))
    #;(test-case "non-with let shadowing of lexical"
        (check-output '("true")
                      ))
    #;(test-case "non-with let shadowing of catch"
        (check-output '("true")
                      ))
    #;(test-case "non-with let shadowing of let"
        (check-output '("true")
                      ))
    (test-case "with binding"
      (check-output '("true")
                    "var o = {a:true}"
                    "with(o){print(a)}"))
    (test-case "with mutation"
      (check-output '("true")
                    "(function(){var a = false;with({}){a = true};print(a)})()"))
    (test-case "nested with-mutation of lexical"
      (check-output '("true")
                    "var a = false"
                    "with({}){with({}){a = true}}print(a)"))
    (test-case "nested with-mutation of with"
      (check-output '("true")
                    "var obj = {a:false}"
                    "with(obj){with({}){a = true}print(a)}"))
    (test-case "with shadowing of top-level"
      (check-output '("true")
                    "var o = {a:true}"
                    "var a = false"
                    "with(o){print(a)}"))
    (test-case "with shadowing of lexical"
      (check-output '("true")
                    "var o = {a:true};"
                    "(function(a){with(o){print(a)}})(false);"))
    (test-case "with shadowing of with"
      (check-output '("true")
                    "var o1 = {a:false};"
                    "var o2 = {a:true};"
                    "with(o1){with(o2){print(a)}}"))
    (test-case "with shadowing of catch"
      (check-output '("true")
                    "var o = {a:true}"
                    "try{throw false}catch(a){with(o){print(a)}}"))
    #;(test-case "with shadowing of let"
        (check-output '("true")
                      ))
    (test-case "lexical shadowing of with"
      (check-output '("true")
                    "var o = {a:false}"
                    "with(o) {(function(a){print(a)})(true)}"))
    (test-case "catch shadowing of with"
      (check-output '("true")
                    "var o = {a:false}"
                    "with(o){try{throw true}catch(a){print(a)}}"))
    #;(test-case "let shadowing of with"
        (check-output '("true")
                      ))
    (test-case "with shadowing of lexical shadowing of with"
      (check-output '("true")
                    "var o1 = {a:1}"
                    "var o2 = {a:true}"
                    "with(o1){(function(a){with(o2){print(a)}})(2)}"))
    (test-case "with shadowing of catch shadowing of with"
      (check-output '("true")
                    "var o1 = {a:1}"
                    "var o2 = {a:true}"
                    "with(o1){try{throw 2}catch(a){with(o2){print(a)}}}"))
    #;(test-case "with shadowing of let shadowing of with"
        (check-output '("true")
                      ))
    (test-case "with shadowing of catch shadowing of lexical"
      (check-output '("true")
                    "var o = {a:true};"
                    "(function(a){try{throw 1}catch(a){with(o){print(a)}}})(2)"))
    (test-case "lexical shadowing of catch shadowing of with"
      (check-output '("true")
                    "var o = {a:1};"
                    "try{throw 2}catch(a){(function(a){print(a)})(true)}"))
    #;(test-case "with shadowing of catch shadowing of let"
        (check-output '("true")
                      ))
    (test-case "lexical shadowing of with shadowing of catch"
      (check-output '("true")
                    "var o = {a:1};"
                    "try{throw 2}catch(a){with(o){(function(a){print(a)})(true)}}"))
    (test-case "catch shadowing of with shadowing of lexical"
      (check-output '("true")
                    "var o = {a:1};"
                    "(function(a){with(o){try{throw true}catch(a){print(a)}}})(2);"))
    #;(test-case "let shadowing of with shadowing of catch"
        (check-output '("true")
                      ))
    #;(test-case "let shadowing of with shadowing of lexical"
        (check-output '("true")
                      ))
    #;(test-case "lexical shadowing of with shadowing of let"
        (check-output '("true")
                      ))
    #;(test-case "catch shadowing of with shadowing of let"
        (check-output '("true")
                      ))
    (test-case "with shadowing of lexical shadowing of catch"
      (check-output '("true")
                    "var o = {a:true};"
                    "try{throw 1}catch(a){(function(a){with(o){print(a)}})(2)}"))
    #;(test-case "with shadowing of lexical shadowing of let"
        (check-output '("true")
                      ))
    (test-case "catch shadowing of lexical shadowing of with"
      (check-output '("true")
                    "var o = {a:1};"
                    "with(o){(function(a){try{throw true}catch(a){print(a)}})(2)}"))
    #;(test-case "let shadowing of lexical shadowing of with"
        (check-output '("true")
                      ))
    (test-case "mutation of with-bound variable"
      (check-output '("true")
                    "var o = {a:false}"
                    "with(o) {o.a=true;print(a)}"))
    (test-case "temporary with shadowing of top-level"
      (check-output '("true")
                    "var a = true;"
                    "var o = {a:false}"
                    "with(o) {delete o.a;print(a)}"))
    (test-case "temporary with shadowing of lexical"
      (check-output '("true")
                    "var o = {a:false};"
                    "(function(a){with(o){delete o.a;print(a)}})(true);"))
    (test-case "temporary with shadowing of with"
      (check-output '("true")
                    "var o1 = {a:true};"
                    "var o2 = {a:false};"
                    "with(o1){with(o2){delete o2.a;print(a)}}"))
    (test-case "temporary with shadowing of catch"
      (check-output '("true")
                    "var o = {a:false};"
                    "try{throw true}catch(a){with(o){delete o.a;print(a)}}"))
    #;(test-case "temporary with shadowing of let"
        (check-output '("true")
                      ))
    (test-case "lexical shadowing of temporary with"
      (check-output '("true")
                    "var o = {a:false};"
                    "with(o){(function(a){delete o.a;print(a)})(true)}"))
    (test-case "catch shadowing of temporary with"
      (check-output '("true")
                    "var o = {a:false};"
                    "with(o){try{throw true}catch(a){delete o.a;print(a)}}"))
    #;(test-case "let shadowing of temporary with"
        (check-output '("true")
                      ))
    (test-case "temporary with shadowing of lexical shadowing of with"
      (check-output '("true")
                    "var o1 = {a:1};"
                    "var o2 = {a:2};"
                    "with(o1){(function(a){with(o2){delete o2.a;print(a)}})(true)}"))
    (test-case "temporary with shadowing of catch shadowing of with"
      (check-output '("true")
                    "var o1 = {a:1};"
                    "var o2 = {a:2};"
                    "with(o1){try{throw true}catch(a){with(o2){delete o2.a;print(a)}}}"))
    (test-case "with non-shadowing of lexical shadowing of with"
      (check-output '("true")
                    "var o1 = {a:false};"
                    "var o2 = {};"
                    "with(o1){(function(a){with(o2){print(a)}})(true)}"))
    (test-case "with non-shadowing of catch shadowing of with"
      (check-output '("true")
                    "var o1 = {a:false};"
                    "var o2 = {};"
                    "with(o1){try{throw true}catch(a){with(o2){print(a)}}}"))
    (test-case "with non-shadowing of let shadowing of with"
      (check-output '("true")
                    "var o1 = {a:false};"
                    "var o2 = {};"
                    "with(o1){let(a=true){with(o2){print(a)}}}"))
    (test-case "var and arguments share the same frame"
      (check-output '("true")
                    "function foo(x) { var x = true; print(arguments[0]); }"
                    "foo(false)"))
    (test-case "superseding arguments"
      (check-output '("true")
                    "function foo(arguments) { print(arguments) }"
                    "foo(true)"))
    (test-case "blocks close over this"
      (check-output '("true")
                    "var obj1 = { prop: false, m: function() { } }"
                    "var obj2 = { prop: true, m: function() { return ({|| obj1.m(); => this.prop }()) } }"
                    "print(obj2.m())"))
    ))

(define dynamic-eval-tests
  (test-suite "eval tests"
    (test-case "eval inherits environment - lookup"
      (check-output '("true")
                    "function foo(x) { print(eval('x')) }"
                    "foo(true)"))
    (test-case "eval inherits environment - assignment"
      (check-output '("true")
                    "function foo(x) { eval('x = true'); print(x) }"
                    "foo(false)"))
    (test-case "eval inherits variable object"
      (check-output '("true")
                    "function foo() { var x = false; (function() { eval('var x = true'); print(x) })() }"
                    "foo()"))
    (test-case "eval inherits current this 1"
      (check-output '("true")
                    "var obj = { x: true, m: function() { print(eval('this.x')) } }"
                    "obj.m()"))
    (test-case "eval inherits current this 2"
      (check-output '("true")
                    "({foo:function(){eval('print(this.bar)')},bar:true}).foo()"))
    (test-case "indirect eval"
      (check-output '("true")
                    "try { var f = eval; f('print(false)') } catch (e) { print('true') }"))
    (test-case "eval inherits global object"
      (check-output '("true")
                    "var a = true;"
                    "eval('print(this.a)');"))
    (test-case "direct eval after mutation"
      (check-output '("hello")
                    "eval = print; eval('hello')"))
    (test-case "direct eval after mutating away and back again"
      (check-output '("hello")
                    "saved = eval;"
                    "eval = print;"
                    "eval = saved;"
                    "eval('print(\"hello\")')"))
    (test-case "direct eval in global code"
      (check-output '("true")
                    "var x = true"
                    "eval('print(x)')"))
    ))

(define prototype-tests
  (test-suite "prototype tests"
    (test-case "global object toString"
      (before
       (reset-js-namespace! test-ns)
       (check-equal? (eval-script "this.toString()" test-ns) "[object DrScheme]")))
    (test-case "vanilla object toString"
      (before
       (reset-js-namespace! test-ns)
       (check-equal? (eval-script "({}).toString()" test-ns) "[object Object]")))
    (test-case "dot method call sets up current this"
      (check-output '("true")
                    "({foo:function(){print(this.bar)},bar:true}).foo()"))
    ))

(define library-tests
  (test-suite "library tests"
    (test-case "String called as a function"
      (before
       (reset-js-namespace! test-ns)
       (check-equal? (eval-script "String(44)" test-ns) "44")))
    (test-case "Number called as a function"
      (before
       (reset-js-namespace! test-ns)
       (check-equal? (eval-script "Number('44')" test-ns) 44)))
    (test-case "Boolean called as a function"
      (before
       (reset-js-namespace! test-ns)
       (check-equal? (eval-script "Boolean(0)" test-ns) #f)))
    (test-case "String.fromCharCode"
      (before
       (reset-js-namespace! test-ns)
       (check-equal? (eval-script "String.fromCharCode(104,101,108,108,111)" test-ns) "hello")))
    (test-case "Array with 0 args"
      (check-output '("")
                    "var a = new Array(); print(a)"))
    (test-case "Array with 1 arg - number"
      (check-output '(",,,,")
                    "var a = new Array(5); print(a)"))
    (test-case "Array with 1 arg - non-number"
      (check-output '("true")
                    "var a = new Array(true); print(a)"))
    (test-case "Array with two args"
      (check-output '("1,2")
                    "var a = new Array(1,2); print(a)"))
    (test-case "Array with three args"
      (check-output '("1,2,3")
                    "var a = new Array(1,2,3); print(a)"))
    ))

(define tail-tests
  (test-suite "tail tests"
    (test-case "tail calls"
      (check-output '("1")
                    "var stack = new Trace;"
                    "var loop = {|n| => (n == 0) ? stack.toArray() : stack.trace(n, {|| => loop(n-1)})};"
                    "print(loop(10))"))
    (test-case "non-tail calls"
      (check-output '("1,2,3,4,5,6,7,8,9,10")
                    "var stack = new Trace;"
                    "function loop(n) { return (n == 0) ? stack.toArray() : stack.trace(n, {|| => loop(n-1)}); }"
                    "print(loop(10))"))
    ))

(define block-tests
  (test-suite "block tests"
    (test-case "do expression"
      (check-output '("true")
                    "var x = 0"
                    "print(do { for (let i = 0; i < 10; i++) { x = i; } => x } === 9)"))
    (test-case "do expression with no tail"
      (check-output '("true")
                    "print(do { } === void(0))"))
    ))

(define object-tests
  (test-suite "object tests"
    (test-case "literal with numeric property name"
      (check-output '("true")
                    "var obj = { 8: true }"
                    "print(obj[8])"))
    (test-case "literal with string literal property name"
      (check-output '("true")
                    "var obj = { \"foo\": true }"
                    "print(obj.foo)"))
    (test-case "literal with property name containing pipes"
      (check-output '("true")
                    "var obj = { \"|foo|\": true }"
                    "print(obj[\"|foo|\"])"))
    ))

(define eval-tests
  (test-suite "eval tests"
    binding-tests
    dynamic-eval-tests
    prototype-tests
    library-tests
    tail-tests
    block-tests
    object-tests
    ))
