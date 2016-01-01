#lang scheme/base

(require scheme/string
         rackunit
         (only-in srfi/13/string string-trim-both)
         "../../runtime.ss"
         "../../eval.ss"
         "../../config.ss"
         "util.ss")

(provide array-tests)

(define property-tests
  (test-suite "property tests"
    (test-case "deleting numeric properties"
      (check-result (void) "var a = [0,1,2]; delete a[2]; a[2]"))
    (test-case "deleted property is not defined"
      (check-result #f "var a = [0,1,2]; delete a[2]; 2 in a"))
    (test-case "numeric `in'"
      (check-result #t "2 in [0,1,2]"))
    (test-case "string `in'"
      (check-result #t "'2' in [0,1,2]"))
    (test-case "skipped property is not defined (11.1.4)"
      (check-result #f "2 in [0,1,,3]"))
    (test-case "skipped property evaluates to void(0)"
      (check-result (void) "[0,1,,3][2]"))
    (test-case "non-numeric hasOwnProperty"
      (check-result #t "var a = [1,2,3]; a.foo = 42; a.hasOwnProperty('foo')"))
    (test-case "numeric hasOwnProperty"
      (check-result #t "[1,2,3].hasOwnProperty('0')"))
    (test-case "hasOwnProperty('length')"
      (check-result #t "[1,2,3].hasOwnProperty('length')"))
    ))

(define length-tests
  (test-suite "length tests"
    (test-case "empty array : length 0"
      (check-result 0 "[].length"))
    (test-case "indirectly extending length"
      (check-result 10 "var a = []; a[9] = 'hello'; a.length"))
    (test-case "deleting doesn't truncate"
      (check-result 3 "var a = [1,2,3]; delete a[2]; a.length"))
    (test-case "truncation"
      (check-result (void) "var a = [1,2,3]; a.length = 0; a[0]"))
    ;; TODO: test range error (set length to something other than [0..2^32))
    ))

(define array-tests
  (test-suite "array tests"
    property-tests
    length-tests
    ))
