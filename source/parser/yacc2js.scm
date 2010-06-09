#! /usr/bin/env gsi

;;;============================================================================

;;; File: "yacc2js.scm", Time-stamp: <2010-06-09 08:06:36 feeley>

;;; Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(define (yacc-convert filename output-filename)
  (call-with-input-file filename
    (lambda (in-port)
      (convert filename output-filename in-port))))

(define (convert filename output-filename in-port)

  (define rev-defs '())

  (define start-nt #f)
  (define rev-left-toks '())
  (define rev-right-toks '())
  (define rev-nonassoc-toks '())
  (define rev-explicit-toks '())
  (define implicit-toks (make-table))

  (define (peek)
    (encode (peek-char in-port)))

  (define (read)
    (encode (read-char in-port)))

  (define (encode c)
    (if (eof-object? c)
        #f
        c))

  (define (parse)
    (let loop ((state 1))
      (if (<= state 2)
          (let ((s (parse-in state)))
            (if s (loop s))))))

  (define (parse-in state)
    (let ((c (skip-whitespace-not-eol)))
      (case c
        ((#f)
         state)
        ((#\%)
         (let ((d (parse-yacc-directive)))
           (if (equal? (car d) "%")
               (+ state 1)
               (begin
                 (cond ((equal? (car d) "start")
                        (let ((tok (convert-token (cadr d))))
                          (set! start-nt tok)))
                       ((equal? (car d) "left")
                        (let ((toks (map convert-token (cdr d))))
                          (set! rev-left-toks
                                (cons toks rev-left-toks))))
                       ((equal? (car d) "right")
                        (let ((toks (map convert-token (cdr d))))
                          (set! rev-right-toks
                                (cons toks rev-right-toks))))
                       ((equal? (car d) "nonassoc")
                        (let ((toks (map convert-token (cdr d))))
                          (set! rev-nonassoc-toks
                                (cons toks rev-nonassoc-toks))))
                       ((equal? (car d) "token")
                        (for-each (lambda (form)
                                    (if (not (char=? #\< (string-ref form 0)))
                                        (let ((tok (convert-token form)))
                                          (set! rev-explicit-toks
                                                (cons tok
                                                      rev-explicit-toks)))))
                                  (cdr d))))
                 state))))
        (else
         (parse-other c state)
         state))))

  (define (parse-yacc-directive)
    (let loop ((rev-forms '()))
      (let ((x (parse-form #f)))
        (if x
            (loop (cons x rev-forms))
            (reverse rev-forms)))))

  (define (parse-form c)
    (let ((c (or c (skip-whitespace-not-eol))))
      (case c
        ((#f)
         #f)
        ((#\{)
         (parse-curly c))
        (else
         (parse-token c)))))

  (define (skip-whitespace-not-eol)
    (let loop ()
      (let ((c (read)))
        (case c
          ((#\space)
           (loop))
          ((#\newline)
           #f)
          ((#\/)
           (case (peek)
             ((#\*)
              (read)
              (let skip ()
                (let ((c (read)))
                  (if (and (eqv? c #\*)
                           (eqv? (peek) #\/))
                      (begin
                        (read)
                        (loop))
                      (skip)))))
             ((#\/)
              (skip-line)
              #f)
             (else
              c)))
          (else
           c)))))

  (define (parse-curly c)
    (let loop ((rev-str (list c)) (n 1))
      (if (= n 0)
          (list->string (reverse rev-str))
          (let ((x (read)))
            (loop (cons x rev-str)
                  (case x
                    ((#\{) (+ n 1))
                    ((#\}) (- n 1))
                    (else  n)))))))

  (define (parse-token c)
    (let loop ((rev-str (list c)))
      (let ((x (peek)))
        (if (char-whitespace? x)
            (list->string (reverse rev-str))
            (loop (cons (read) rev-str))))))

  (define (parse-other c state)
    (case state
      ((2)
       (let loop ((rev-def '()) (c c))
         (let ((f (parse-form c)))
           (if (equal? f ";")
               (convert-def (reverse rev-def))
               (loop (if f
                         (cons f rev-def)
                         rev-def)
                     #f)))))
      (else
       (skip-line))))

  (define (skip-line)
    (let loop ()
      (case (read)
        ((#\newline)
         #t)
        (else
         (loop)))))

  (define (convert-def def)
    (let ((head (car def)))
      (if (not (char=? #\: (string-ref head (- (string-length head) 1))))
          (error "def head does not end in ':'")
          (let ((nt
                 (convert-token (substring head 0 (- (string-length head) 1)))))
            (let loop ((rev-rhss '(())) (lst (cdr def)))
              (if (pair? lst)
                  (let ((x (car lst)))
                    (cond ((string=? x "|")
                           (loop (cons '() rev-rhss)
                                 (cdr lst)))
                          ((string=? x "error")
                           (loop (cons (cons 'AUTOSEMICOLON (car rev-rhss))
                                       (cdr rev-rhss))
                                 (cdr lst)))
                          ((char=? (string-ref x 0) #\{)
                           (loop rev-rhss
                                 (cdr lst)))
                          (else
                           (loop (cons (cons (convert-token x) (car rev-rhss))
                                       (cdr rev-rhss))
                                 (cdr lst)))))
                  (set! rev-defs
                        (cons (cons nt
                                    (reverse (map convert-rule rev-rhss)))
                              rev-defs))))))))

  (define (convert-rule rev-rule)
    (let loop ((lst rev-rule) (rule '()))
      (if (pair? lst)
          (loop (cdr lst)
                (let ((x (car lst)))
                  (if (eq? x '%prec)
                      (list (cons 'prec: rule))
                      (cons x rule))))
          rule)))

  (define (convert-token str)
    (let ((len (string-length str)))
      (if (and (>= len 3)
               (char=? #\' (string-ref str 0))
               (char=? #\' (string-ref str (- len 1))))
          (let ((tok (string->symbol (substring str 1 (- len 1)))))
            (table-set! implicit-toks tok tok)
            tok)
          (string->symbol str))))

  (define (remove x lst)
    (if (equal? x (car (car lst)))
        (cdr lst)
        (cons (car lst)
              (remove x (cdr lst)))))

  (include "keywords.scm");;;;;;;;;;;;;;;;;;;;;;

  (parse)

  (let* ((start
          (or start-nt (car (car defs))))
         (defs
           (reverse rev-defs))
         (reordered-defs
          (cons (assoc start defs)
                (remove start defs)))
         (all-toks
          (append rev-explicit-toks
                  (map car (table->list implicit-toks))
                  (apply append rev-left-toks)
                  (apply append rev-right-toks)
                  (apply append rev-nonassoc-toks))))

    (define (generate-javascript-constructors)

      (define (token? x)
        (memq x all-toks))

      (for-each
       (lambda (rule)
         (let ((nt (car rule))
               (rhss (cdr rule)))
           (define count 0)
           (for-each
            (lambda (rhs)
              (let ((rev-forms '()))
                (for-each
                 (lambda (x)
                   (if (not (list? x))
                       (set!
                        rev-forms
                        (cons
                         (cons x
                               (list->string
                                (map (lambda (c) c) ;; char-downcase
                                     (string->list
                                      (if (token? x)
                                          (let* ((t (assq x token-remap))
                                                 (t2 (if t (cdr t) x)))
                                            (string-append (symbol->string t2)
                                                           #;"_tok"))
                                          (symbol->string x))))))
                         rev-forms))))
                 rhs)
                (set! count (+ count 1))
                (print "function " nt "_" count "(p")
                (for-each (lambda (x) (print ", " (cdr x))) (reverse rev-forms))
                (print ")\n")
                (print "{\n")
                (print "    return { type: \"" nt "_" count "\"\n")
                (print "           , loc: ")
                (if (null? rev-forms)
                    (print "p.current_loc()")
                    (let ((last (car rev-forms)))
                      (if (null? (cdr rev-forms))
                          (print (cdr last) ".loc")
                          (let ((first (car (reverse rev-forms))))
                            (print (cdr first) ".loc.join("
                                   (cdr last) ".loc)")))))
                (print "\n")
                (for-each (lambda (x)
                            (if (not (token? (car x)))
                                (begin
                                  (print "           , ")
                                  (print (cdr x) ": " (cdr x) "\n"))))
                          (reverse rev-forms))
                (print "           };\n")
                (print "}\n")
                (print "\n")))
            rhss)))
       reordered-defs))

    ;; (generate-javascript-constructors)

    `(lalr-parser
      ;;(out-table: "foo")
      ;;(output: parser "out")
      (output-javascript: ,output-filename)
      ,(append '(AUTOSEMICOLON)
               (reverse rev-explicit-toks)
               (apply append
                      (map (lambda (kw-str)
                             (let ((sym
                                    (string->symbol
                                     (list->string
                                      (map char-upcase (string->list kw-str))))))
                               (if (or (memq sym all-toks)
                                       (memq sym (map cdr token-remap)))
                                   '()
                                   (list sym))))
                           keywords))
               (map car (table->list implicit-toks))
               (map (lambda (lst) (cons 'left: lst)) rev-left-toks)
               (map (lambda (lst) (cons 'right: lst)) rev-right-toks)
               (map (lambda (lst) (cons 'nonassoc: lst)) rev-nonassoc-toks))
      ,@reordered-defs)))

(define (main yacc-grammar-filename
              #!optional
              (output-filename (string-append yacc-grammar-filename ".js")))
  (let ((lalr-grammar (yacc-convert yacc-grammar-filename output-filename)))
;;    (pretty-print lalr-grammar)
;;    (eval '(include "lalr-scm/lalr.scm"))
    (eval '(include "lalr-2.2.0.scm"))
    (eval lalr-grammar)))

;;;============================================================================
