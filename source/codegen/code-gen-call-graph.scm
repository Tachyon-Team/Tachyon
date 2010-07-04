#! /usr/bin/env gsi

;;; Extract the call graph from code gen for asm and rtl functions
;;; and produce a dot file for visualization using Graphiz


; Globals to old information
(define fct-def-table (make-table))
(define fct-def-prefixes '("asm" "rtl" "gen" "x86"))

;; Helper functions

; Allow iteration of sexpr in file
(define (for-each-sexpr-in-port proc port)
   (let loop ()
     (let ((sexpr (read port)))
       (if (not (eof-object? sexpr))
           (begin
             (proc sexpr)
             (loop))))))

; Flatten nested list
(define (list-flatten list)
    (cond ((null? list) '())
          ((list? (car list)) (append (list-flatten (car list)) (list-flatten (cdr list))))
          (else (cons (car list) (list-flatten (cdr list))))))
;(pp "list-flatten")
;(pp (list-flatten '()))
;(pp (list-flatten '(1 (2 3))))
;(pp (list-flatten '(1 (((2)) 3))))
;(pp (list-flatten (quote (define (list-flatten list)
;    (cond ((null? list) '())
;          ((list? (car list)) (append (list-flatten (car list)) (list-flatten (cdr list))))
;          (else (cons (car list) (list-flatten (cdr list)))))))))

(define (list-foldl list op init)
    (if (equal? list '())
        init
        (list-foldl (cdr list) op (op (car list) init))))
;(pp "list-foldl")
;(pp (list-foldl '(1 2 3) (lambda (x acc) (+ x acc)) 0))

; Remove the fist item found in the list equal to elem
(define (list-remove list elem)
    (cond ((null? list) '()) 
          ((equal? (car list) elem) (cdr list))
          (else (cons (car list) (list-remove (cdr list) elem)))))
;(pp "list-remove")
;(pp (list-remove '(1 2 3) 2))
;(pp (list-remove '(1 2 3) 4))
;(pp (list-remove '(3 1 2 3) 3))

; Test for a prefix in a symbol
(define (symbol-prefix? symbol prefix)
    (define sym-str (symbol->string symbol))
    (and (>= (string-length sym-str) (string-length prefix))
         (equal? (substring sym-str 0 (string-length prefix))
                 prefix)))
;(pp "symbol-prefix?")
;(pp (symbol-prefix? 'asm-test "asm"))
;(pp (symbol-prefix? 'foo-asm-test "asm"))

; Test for a function definition in a define
(define (fct-def? sexpr) 
    (and (>= (length sexpr) 3) 
         (equal? (car sexpr) 'define) 
         (list? (cadr sexpr))))
;(pp "fct-def")
;(pp (fct-def? '()))
;(pp (fct-def? '(define foo 42)))
;(pp (fct-def? '(define (foo) ())))

; Extract the symbol name from a function definition
(define (fct-def-name sexpr) 
    (if (fct-def? sexpr) (caadr sexpr) #f))
;(pp "fct-def-name")
;(pp (fct-def? '(define foo 42)))
;(pp (fct-def-name '(define (foo) ())))

; Test for an accepted prefix on a symbol
(define (fct-def-name-accepted-prefix? symbol)
    (list-foldl fct-def-prefixes 
                (lambda (prefix acc) (or (symbol-prefix? symbol prefix) acc)) 
                #f))
;(pp "fct-def-name-accepted-prefix?")
;(pp (fct-def-name-accepted-prefix? 'foo-bar))
;(pp (fct-def-name-accepted-prefix? 'asm-bar))

; Returns the set union of operand-set and union-set
(define (lset-union union-set operand-set) 
    (append union-set
            (list-foldl union-set 
                        (lambda (item acc) (list-remove acc item)) 
                        operand-set)))
;(pp "lset-union")
;(pp (lset-union '(1 2 3) '(2)))
;(pp (lset-union '(1 2 3) '(4)))
;(pp (lset-union '(1 2 3) '(4 5 6)))

; Adds symbol to called functions of fct-name if it contains one of the
; fct-def-prefixes
(define (process-fct-def-body fct-name)
    (lambda (sexpr)
        (if (and (symbol? sexpr) (fct-def-name-accepted-prefix? sexpr))
            (table-set! fct-def-table 
                        fct-name 
                        (lset-union (table-ref fct-def-table fct-name '()) (list sexpr))))))
        

;; Main algorithm
(define (extract-called-functions sexpr)
    (if (fct-def? sexpr)
        (for-each (process-fct-def-body (fct-def-name sexpr)) (cddr (list-flatten sexpr)))))
;(pp "extract-called-functions")
;(extract-called-functions (quote
;(define (asm-origin cb address #!optional (fill 0))
;  (asm-at-assembly
;   cb
;   (lambda (self)
;     (- address self))
;   (lambda (self)
;     (let ((len (- address self)))
;       (if (< len 0)
;           (compiler-internal-error "asm-origin, can't move back")
;           (let loop ((n len))
;             (if (> n 0)
;                 (begin
;                   (asm-8 cb fill)
;                   (loop (- n 1))))))))))))
(define (fct-def-entry key)
    (table-ref fct-def-table key))
;(pp (fct-def-entry 'asm-origin))


(define (analyze filenames)
    (for-each
        (lambda (filename)
            (call-with-input-file filename (lambda (port) (for-each-sexpr-in-port extract-called-functions port)))) filenames))
;(pp "analyse asm.scm")
(analyze '("asm-x86.scm" "asm.scm"))

(define (report)
    (print "digraph untitled {\n")
    (table-for-each 
        (lambda (k v) 
            (for-each 
                (lambda (v)
                    (print "\"" k "\" -> \"" v "\";\n"))
             v))
         fct-def-table)
     (fill-translated)
     (print "}\n"))

(define (report2)
    (define visited '())
    (define tovisit '())
    (define (print-node k c) 
        (print "\"" k "\" -> \"" c "\";\n"))
    (define (loop)
        (if (not (null? tovisit))
            (let ((k (car tovisit))
                  (children (table-ref fct-def-table (car tovisit) '())))
                 ; Remove k from the tovisit set
                 (set! tovisit (cdr tovisit))
                 (set! visited (cons k visited))
                 ; Append all the children not visited yet
                 (for-each (lambda (c) 
                            (if (not (member c visited))
                                (set! tovisit (cons c tovisit)))
                            (print-node k c))
                           children)
                 (loop))))
    (print "digraph untitled {\n")
    (for-each 
        (lambda (k)
            (set! tovisit (cons (string->symbol k) tovisit)))
        (cdr (command-line)))
    (loop)
    (fill-translated)
    (print "}\n"))

(define (fill-translated)
    (for-each 
        (lambda (c)
            (print "\"" c "\"[color=grey, style=filled];\n"))
        translated))
(define translated
    '(asm-8
      asm-16
      asm-32
      asm-code-extend
      asm-code-block-stream
      gen-8
      gen-16
      gen-32
      gen-64
      gen-imm-num
      x86-opnd-size-override-prefix
      x86-mem
      x86-mem?
      x86-mem-offset
      x86-mem-reg1
      x86-mem-reg2
      x86-mem-scale
      x86-glo
      x86-glo?
      x86-glo-name
      x86-glo-offset
      x86-reg
      x86-reg-field
      x86-reg-width
      x86-reg-name
      x86-reg?
      x86-r8?
      x86-r8-h?
      x86-xmm?
      x86-mm?
      x86-fpu?
      x86-r16?
      x86-r32?
      x86-r64?
      x86-r8
      x86-r16
      x86-r32
      x86-r64
      rtl-code-gen-context-code-block))

(if (null? (cdr (command-line))) (report) (report2))




