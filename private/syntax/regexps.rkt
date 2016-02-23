#lang scheme/base

(require parser-tools/lex
         "abstract-regexps.ss"
         "../../private/config.ss")

(provide (all-defined-out))

(define-abstract-regexps
  [octal (range #\0 #\7)]
  [decimal (range #\0 #\9)]
  [positive (range #\1 #\9)]
  [hex (union (range #\0 #\9) (range #\A #\F) (range #\a #\f))]
  [exponent (union #\e #\E)]
  [sign (union #\- #\+)]
  [floatA (sequence (kleene+ decimal)
                    #\.
                    (kleene* decimal)
                    (maybe exponent sign (kleene+ decimal)))]
  [floatB (sequence (kleene+ decimal)
                    (maybe #\. (kleene* decimal))
                    exponent (maybe sign) (kleene+ decimal))]
  [floatC (sequence #\. (kleene+ decimal)
                    (maybe exponent (maybe sign) (kleene+ decimal)))]
  [float (union floatA floatB floatC)]
  [intH (sequence #\0 (union #\x #\X) (kleene+ hex))]
  [intO (sequence #\0 (kleene+ octal))]
  [intD (union #\0 (sequence positive (kleene* decimal)))]
  [integer (union intH intO intD)]
  [letter (union (range #\a #\z) (range #\A #\Z))]
  [unicode-escape (sequence #\\ #\u hex hex hex hex)]
  [identifier-start (union #\$ #\_ letter unicode-escape)]
  [identifier (sequence identifier-start (kleene* (union identifier-start decimal)))]
  [single-string (sequence #\'
                           (save (kleene* (union (sequence #\\ (any))
                                                 (complement #\'))))
                           #\')]
  [double-string (sequence #\"
                           (save (kleene* (union (sequence #\\ (any))
                                                 (complement #\"))))
                           #\")]
  [string-literal (union single-string double-string)]
  [re-pattern (kleene* (union (sequence #\\ #\/)
                              (complement #\/ #\return #\newline)))]
  [regexp-literal (sequence #\/ (save re-pattern) #\/ (maybe (save (union (sequence #\g (maybe #\i))
                                                                          (sequence #\i (maybe #\g))))))]
  ;; NOTE: prefixes must go AFTER longer patterns
  [assignment-operator (union "|=" "^=" "&=" "<<=" ">>=" ">>>=" "+=" "-=" "*=" "/=" "%=" "=")]
  [operator (union ";" "," "?" ":" "||" "&&" "|" "^" "&" "=>" "==="
                   "==" "=" "!==" "!=" "<<" "<=" "<" ">>>" ">>"
                   ">=" ">" "++" "--" "+" "-" "*" "/" "%" "!"
                   "~" "." "[" "]" "{" "}" "(" ")")]
  [line-comment (sequence "//" (kleene* (complement #\return #\newline)))]
  [block-comment (sequence "/*"
                           (kleene* (union (complement #\*)
                                           (sequence #\* (complement #\/))))
                           "*/")]
  [comment (union line-comment block-comment)]
  [ws (kleene+ (union #\tab #\return #\newline #\space #\vtab))]
  [empty (kleene+ (union ws comment))])

(define-lex-abbrevs
  [lex:float (make-lex float)]
  [lex:integer (make-lex integer)]
  [lex:regexp (make-lex regexp-literal)]
  [lex:string (make-lex string-literal)]
  [lex:assignment-operator (make-lex assignment-operator)]
  [lex:operator (make-lex operator)]
  [lex:identifier (make-lex identifier)]
  [lex:line-comment (make-lex line-comment)]
  [lex:block-comment (make-lex block-comment)]
  [lex:comment (make-lex comment)]
  [lex:whitespace (make-lex ws)]
  [lex:empty (make-lex empty)])

(define rx:float (make-rx float))
(define rx:integer (make-rx integer))
(define rx:regexp (make-rx regexp-literal))
(define rx:string (make-rx string-literal))
(define rx:operator (make-rx operator))
(define rx:assignment-operator (make-rx assignment-operator))
(define rx:identifier (make-rx identifier))
(define rx:line-comment (make-rx line-comment))
(define rx:block-comment (make-rx block-comment))
(define rx:comment (make-rx comment))
(define rx:whitespace (make-rx ws))
(define rx:empty (make-rx empty))
