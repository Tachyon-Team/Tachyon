;;; File: "asm.scm"
;;;
;;; This module implements the generic assembler.

;(##declare (standard-bindings) (fixnum) (block))

;(define compiler-internal-error error)

;; (asm-make-code-block start-pos big-endian? listing?) starts a new
;; empty code block at address start-pos and returns it.  The
;; parameter big-endian? indicates the byte ordering to use for 16, 32
;; and 64 bit values.  The parameter listing? indicates if a listing
;; is to be generated.  After a call to asm-make-code-block the code
;; block is built by calling the following procedures:
;;
;;  asm-8            adds an 8 bit integer to the code block
;;  asm-16-be        adds a 16 bit big-endian integer to the code block
;;  asm-16-le        adds a 16 bit little-endian integer to the code block
;;  asm-16           like asm-16-be or asm-16-le depending on endianness
;;  asm-32-be        adds a 32 bit big-endian integer to the code block
;;  asm-32-le        adds a 32 bit little-endian integer to the code block
;;  asm-32           like asm-32-be or asm-32-le depending on endianness
;;  asm-64-be        adds a 64 bit big-endian integer to the code block
;;  asm-64-le        adds a 64 bit little-endian integer to the code block
;;  asm-64           like asm-64-be or asm-64-le depending on endianness
;;  asm-f32          adds a 32 bit IEEE float to the code block
;;  asm-f64          adds a 64 bit IEEE float to the code block
;;  asm-UTF-8-string adds a null terminated UTF-8 string to the code block
;;  asm-label        sets a label to the current position in the code block
;;  asm-align        adds enough padding bytes to force alignment
;;  asm-origin       adds enough padding bytes to move to a particular address
;;  asm-at-assembly  defers code production to assembly time
;;  asm-listing      adds textual information to the listing

(define (asm-make-code-block start-pos big-endian? listing?)
  (vector 'code-block
          start-pos
          big-endian?
          listing?
          (asm-make-stream)))

(define (asm-code-block-start-pos cb)
  (vector-ref cb 1))

(define (asm-code-block-big-endian? cb)
  (vector-ref cb 2))

(define (asm-code-block-listing? cb)
  (vector-ref cb 3))

(define (asm-code-block-stream cb)
  (vector-ref cb 4))

(define (asm-code-block-stream-set! cb x)
  (vector-set! cb 4 x))

;; (asm-8 cb n) adds an 8 bit signed or unsigned integer to the code
;; block.

(define (asm-8 cb n)
  (asm-code-extend cb (asm-bits-0-to-7 n)))

;; (asm-16 cb n) adds a 16 bit signed or unsigned integer to the code
;; block.

(define (asm-16 cb n)
  (if (asm-code-block-big-endian? cb)
      (asm-16-be cb n)
      (asm-16-le cb n)))

(define (asm-16-be cb n)
  (asm-8 cb (asm-bits-8-and-up n))
  (asm-8 cb n))

(define (asm-16-le cb n)
  (asm-8 cb n)
  (asm-8 cb (asm-bits-8-and-up n)))

;; (asm-32 cb n) adds a 32 bit signed or unsigned integer to the code
;; block.

(define (asm-32 cb n)
  (if (asm-code-block-big-endian? cb)
      (asm-32-be cb n)
      (asm-32-le cb n)))

(define (asm-32-be cb n)
  (asm-16-be cb (asm-bits-16-and-up n))
  (asm-16-be cb n))

(define (asm-32-le cb n)
  (asm-16-le cb n)
  (asm-16-le cb (asm-bits-16-and-up n)))

;; (asm-64 cb n) adds a 64 bit signed or unsigned integer to the code
;; block.

(define (asm-64 cb n)
  (if (asm-code-block-big-endian? cb)
      (asm-64-be cb n)
      (asm-64-le cb n)))

(define (asm-64-be cb n)
  (asm-32-be cb (asm-bits-32-and-up n))
  (asm-32-be cb n))

(define (asm-64-le cb n)
  (asm-32-le cb n)
  (asm-32-le cb (asm-bits-32-and-up n)))

;; (asm-f32 cb n) adds a 32 bit IEEE floating point number to the code
;; block.

(define (asm-f32 cb n)
  (asm-32 cb (asm-float->bits n #f)))

;; (asm-f64 cb n) adds a 64 bit IEEE floating point number to the code
;; block.

(define (asm-f64 cb n)
  (asm-64 cb (asm-float->bits n #t)))

;; (asm-UTF-8-string cb str) adds a null terminated UTF-8 string to
;; the code block.

;;TODO: actually encode UTF-8!

(define (asm-UTF-8-string cb str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (asm-8 cb (char->integer (string-ref str i)))
            (loop (+ i 1)))
          (asm-8 cb 0)))))

;; (asm-make-label cb id) creates a new label object.  A label can be
;; queried with asm-label-pos to obtain the label's position relative
;; to the start of the code block (i.e. start-pos).  The argument id
;; gives a name (not necessarily unique) to the label and is only
;; needed for debugging purposes.

(define (asm-make-label cb id #!optional (pos #f))
  (vector 'label
          pos
          id))

;; (asm-label cb label-obj) sets the label to the current position in
;; the code block.

(define (asm-label cb label-obj)
  (if (vector-ref label-obj 1)
      (compiler-internal-error
       "asm-label, label multiply defined"
       (asm-label-id label-obj))
      (begin
        (vector-set! label-obj 1 0)
        (asm-code-extend cb label-obj))))

;; (asm-label-id label-obj) returns the identifier of the label object.

(define (asm-label-id label-obj)
  (vector-ref label-obj 2))

(define (asm-label-name label-obj)
  (let ((id (asm-label-id label-obj)))
    (cond ((string? id) id)
          ((symbol? id) (symbol->string id))
          ((number? id) (string-append "_" (number->string id)))
          (else
           (compiler-internal-error
            "asm-label-name, this type of label id is not supported" id)))))

;; (asm-label-pos label-obj) returns the position of the label
;; relative to the start of the code block (i.e. start-pos).
;; This procedure can only be called at assembly time (i.e.
;; within the call to asm-assemble) or after assembly time
;; for labels declared prior to assembly time with asm-label.
;; A label declared at assembly time can only be queried after
;; assembly time.  Moreover, at assembly time the position of a
;; label may vary from one call to the next due to the actions
;; of the assembler.

(define (asm-label-pos label-obj)
  (let ((pos (vector-ref label-obj 1)))
    (if pos
        pos
        (compiler-internal-error
         "asm-label-pos, undefined label"
         (asm-label-id label-obj)))))

;; (asm-align cb multiple offset fill) adds enough zero bytes to the code
;; block to force alignment to the next address congruent to
;; offset modulo multiple.

(define (asm-align cb multiple #!optional (offset 0) (fill 0))
  (asm-at-assembly
   cb
   (lambda (cb self)
     (modulo (- multiple (- self offset)) multiple))
   (lambda (cb self)
     (let ((n (modulo (- multiple (- self offset)) multiple)))
       (let loop ((i n))
         (if (> i 0)
             (begin
               (asm-8 cb fill)
               (loop (- i 1)))))))))

;; (asm-origin cb address) adds enough zero bytes to the code block to
;; move to the given address.

(define (asm-origin cb address #!optional (fill 0))
  (asm-at-assembly
   cb
   (lambda (self)
     (- address self))
   (lambda (self)
     (let ((len (- address self)))
       (if (< len 0)
           (compiler-internal-error "asm-origin, can't move back")
           (let loop ((n len))
             (if (> n 0)
                 (begin
                   (asm-8 cb fill)
                   (loop (- n 1))))))))))

;; (asm-at-assembly cb . procs) makes it possible to defer code
;; production to assembly time.  A useful application is to generate
;; position dependent and span dependent code sequences.  This
;; procedure must be passed an even number of procedures.  All odd
;; indexed procedures (including the first procedure) are called
;; "check" procedures.  The even indexed procedures are the
;; "production" procedures which, when called, produce a particular
;; code sequence.  A check procedure decides if, given the current
;; state of assembly (in particular the current positioning of the
;; labels), the code produced by the corresponding production
;; procedure is valid.  If the code is not valid, the check procedure
;; must return #f.  If the code is valid, the check procedure must
;; return the length of the code sequence in bytes.  The assembler
;; will try each check procedure in order until it finds one that does
;; not return #f (the last check procedure must never return #f).  For
;; convenience, the code block and current position in the code
;; sequence is passed as the two arguments of check and production
;; procedures.
;;
;; Here is a sample call of asm-at-assembly to produce the
;; shortest branch instruction to branch to label x for a
;; hypothetical processor:
;;
;;  (asm-at-assembly
;;
;;    cb
;;
;;    (lambda (cb self) ;; first check procedure
;;      (let ((dist (- (asm-label-pos x) self)))
;;        (if (and (>= dist -128) (<= dist 127)) ;; short branch possible?
;;            2
;;            #f)))
;;
;;    (lambda (cb self) ;; first production procedure
;;      (asm-8 cb #x34) ;; branch opcode for 8 bit displacement
;;      (asm-8 cb (- (asm-label-pos x) self)))
;;
;;    (lambda (cb self) 5) ;; second check procedure
;;
;;    (lambda (cb self) ;; second production procedure
;;      (asm-8 cb #x35) ;; branch opcode for 32 bit displacement
;;      (asm-32 cb (- (asm-label-pos x) self))))

(define (asm-at-assembly cb . procs)
  (asm-code-extend cb (vector 'deferred procs 0)))

;; (asm-listing cb text) adds text to the right side of the listing.
;; The atoms in text will be output using the display procedure.  The
;; listing is generated by calling asm-display-listing.

(define (asm-listing cb text)
  (if (asm-code-block-listing? cb)
      (asm-code-extend cb (vector 'listing text))))

;; (asm-assemble cb) assembles the code block.  After assembly, the
;; label objects will be set to their final position and the
;; alignment bytes and the deferred code will have been produced.  It
;; is possible to extend the code block after assembly.  However, if
;; any of the procedures "asm-label", "asm-align", and
;; "asm-at-assembly" are called, the code block will have to be
;; assembled once more.

(define (asm-assemble cb)

  (define (pass1)

    ;; construct fixup list and make first label assignment

    (let loop ((curr (cdr (asm-code-block-stream cb)))
               (fixup-lst '())
               (span 0)
               (pos (asm-code-block-start-pos cb)))
      (if (pair? curr)
          (let ((x (car curr)))
            (if (vector? x)
                (let ((kind (vector-ref x 0)))
                  (cond ((eq? kind 'label)
                         ;; make first approximation of label's position
                         (vector-set! x 1 pos)
                         (loop (cdr curr)
                               (cons (cons span curr) fixup-lst)
                               0
                               pos))
                        ((eq? kind 'deferred)
                         (loop (cdr curr)
                               (cons (cons span curr) fixup-lst)
                               0
                               pos))
                        (else
                         (loop (cdr curr)
                               fixup-lst
                               span
                               pos))))
                (loop (cdr curr)
                      fixup-lst
                      (+ span 1)
                      (+ pos 1))))
          (reverse fixup-lst))))

  (let ((fixup-lst (pass1)))

    (let loop1 ()

      ;; determine size of deferred code given current label positions

      (let loop2 ((lst fixup-lst)
                  (pos (asm-code-block-start-pos cb))
                  (changed? #f))
        (if (pair? lst)
            (let* ((fixup (car lst))
                   (pos (+ pos (car fixup)))
                   (curr (cdr fixup))
                   (x (car curr)))
              (if (eq? (vector-ref x 0) 'label)
                  ;; label
                  (loop2 (cdr lst) pos changed?)
                  ;; deferred
                  (let ((old-size (vector-ref x 2)))
                    (let loop3 ()
                      (let* ((check (car (vector-ref x 1)))
                             (new-size (check cb pos)))
                        (if new-size
                            (if (= old-size new-size)
                                (loop2 (cdr lst) (+ pos old-size) changed?)
                                (begin
                                  ;; set the new size of the deferred code
                                  (vector-set! x 2 new-size)
                                  ;; position must advance according to old size
                                  (loop2 (cdr lst) (+ pos old-size) #t)))
                            (begin
                              ;; discard current check/production procedures
                              (vector-set! x 1 (cddr (vector-ref x 1)))
                              (loop3))))))))

            ;; determine label positions given new size of deferred code

            (let loop4 ((lst fixup-lst)
                        (pos (asm-code-block-start-pos cb))
                        (changed? changed?))
              (if (pair? lst)
                  (let* ((fixup (car lst))
                         (pos (+ pos (car fixup)))
                         (curr (cdr fixup))
                         (x (car curr)))
                    (if (eq? (vector-ref x 0) 'label)
                        ;; label
                        (if (= (vector-ref x 1) pos)
                            (loop4 (cdr lst) pos changed?)
                            (begin
                              (vector-set! x 1 pos)
                              (loop4 (cdr lst) pos #t)))
                        ;; deferred
                        (let ((new-size (vector-ref x 2)))
                          (loop4 (cdr lst) (+ pos new-size) changed?))))

                  ;; repeat if one or more labels changed position

                  (if changed?
                      (loop1)))))))

    ;; generate deferred code by calling production procedures

    (let loop5 ((prev (asm-code-block-stream cb))
                (curr (cdr (asm-code-block-stream cb)))
                (pos (asm-code-block-start-pos cb)))
      (if (pair? curr)
          (let ((x (car curr))
                (next (cdr curr)))
            (if (vector? x)
                (let ((kind (vector-ref x 0)))
                  (cond ((eq? kind 'label)
                         (let ((final-pos (vector-ref x 1)))
                           (if final-pos
                               (if (not (= pos final-pos))
                                   (compiler-internal-error
                                    "asm-assemble, inconsistency detected"))
                               (vector-set! x 1 pos))
                           ;; remove label
                           (set-cdr! prev next)
                           (loop5 prev next pos)))
                        ((eq? kind 'deferred)
                         (let ((temp (asm-code-block-stream cb)))
                           (asm-code-block-stream-set! cb (asm-make-stream))
                           (let ((production (cadr (vector-ref x 1))))
                             (production cb pos))
                           (let ((tail (car (asm-code-block-stream cb))))
                             (set-cdr! tail next))
                           (let ((head (cdr (asm-code-block-stream cb))))
                             (set-cdr! prev head)
                             (asm-code-block-stream-set! cb temp)
                             (loop5 prev head pos))))
                        (else
                         (loop5 curr next pos))))
                (loop5 curr next (+ pos 1))))
          (begin
            (set-car! (asm-code-block-stream cb) prev)
            pos)))))

;; (asm-display-listing cb port) produces a listing of the code block
;; on the given output port.  The bytes generated are shown in
;; hexadecimal on the left side of the listing and the right side of
;; the listing contains the text inserted by asm-listing.

(define (asm-display-listing cb port)

  (define text-col 24)
  (define pos-width 6)
  (define byte-width 2)
  (define pos-radix 16)

  (define (output text)
    (cond ((null? text))
          ((pair? text)
           (output (car text))
           (output (cdr text)))
          (else
           (display text port))))

  (define (print-digit n)
    (display (string-ref "0123456789abcdef" n) port))

  (define (print-byte n)
    (print-digit (quotient n 16))
    (print-digit (modulo n 16)))

  (define (print-pos n)

    (define (p n i)
      (if (> i 0)
          (if (< n 0)
              (begin
                (p n (- i 1))
                (output " "))
              (begin
                (p (quotient n pos-radix) (- i 1))
                (print-digit (modulo n pos-radix))))))

    (p n pos-width))

  (let loop1 ((lst (cdr (asm-code-block-stream cb)))
              (pos (asm-code-block-start-pos cb))
              (col 0))
    (if (pair? lst)
        (let ((x (car lst)))
          (if (vector? x)
              (let ((kind (vector-ref x 0)))
                (cond ((eq? kind 'listing)
                       (let ((col
                              (if (= col 0)
                                  (begin
                                    (print-pos pos)
                                    pos-width)
                                  col)))
                         (let loop2 ((col col))
                           (if (< col text-col)
                               (begin
                                 (display (integer->char 9) port)
                                 (loop2 (* 8 (+ (quotient col 8) 1)))))))
                       (output (vector-ref x 1))
                       (newline port)
                       (loop1 (cdr lst) pos 0))
                      (else
                       (loop1 (cdr lst) pos col)
                       (compiler-internal-error
                        "asm-display-listing, code stream not assembled"))))
              (if (or (= col 0) (>= col (- text-col byte-width)))
                  (begin
                    (if (not (= col 0)) (newline port))
                    (print-pos pos)
                    (display " " port)
                    (print-byte x)
                    (loop1 (cdr lst) (+ pos 1) (+ (+ pos-width 1) byte-width)))
                  (begin
                    (print-byte x)
                    (loop1 (cdr lst) (+ pos 1) (+ col byte-width))))))
        (if (> col 0)
            (newline port)))))

;; (asm-assemble-to-file cb filename) assembles the code block and
;; writes it to a file.

(define (asm-assemble-to-file cb filename)
  (asm-assemble cb)
  (with-output-to-file filename
    (lambda ()
      (let loop ((lst (cdr (asm-code-block-stream cb))))
        (if (pair? lst)
            (let ((x (car lst)))
              (if (vector? x)
                  (let ((kind (vector-ref x 0)))
                    (if (not (eq? kind 'listing))
                        (compiler-internal-error
                         "asm-write-code, code stream not assembled"))
                    (loop (cdr lst)))
                  (begin
                    (write-char (integer->char x))
                    (loop (cdr lst))))))))))

;; (asm-assemble-to-u8vector cb) assembles the code block and converts
;; it to a u8vector containing the sequence of bytes.

(define (asm-assemble-to-u8vector cb)
  (let* ((len (asm-assemble cb))
         (u8v (make-u8vector len 0)))
    (let loop ((lst (cdr (asm-code-block-stream cb)))
               (pos 0))
        (if (pair? lst)
            (let ((x (car lst)))
              (if (vector? x)
                  (let ((kind (vector-ref x 0)))
                    (if (not (eq? kind 'listing))
                        (compiler-internal-error
                         "asm-write-code, code stream not assembled"))
                    (loop (cdr lst) pos))
                  (begin
                    (u8vector-set! u8v pos x)
                    (loop (cdr lst) (+ pos 1)))))
            u8v))))

(define (asm-write-hex-file cb filename)
  (with-output-to-file filename
    (lambda ()

      (define (print-digit n)
        (display (string-ref "0123456789ABCDEF" n)))

      (define (print-byte n)
        (print-digit (quotient n 16))
        (print-digit (modulo n 16)))

      (define (print-line type addr bytes)
        (let ((n (length bytes))
              (addr-hi (quotient addr 256))
              (addr-lo (modulo addr 256)))
          (display ":")
          (print-byte n)
          (print-byte addr-hi)
          (print-byte addr-lo)
          (print-byte type)
          (for-each print-byte bytes)
          (let ((sum
                 (modulo (- (apply + n addr-hi addr-lo type bytes)) 256)))
            (print-byte sum)
            (newline))))

      (let loop ((lst (cdr (asm-code-block-stream cb)))
                 (pos (asm-code-block-start-pos cb))
                 (rev-bytes '()))
        (if (pair? lst)
            (let ((x (car lst)))
              (if (vector? x)
                  (let ((kind (vector-ref x 0)))
                    (if (not (eq? kind 'listing))
                        (compiler-internal-error
                         "asm-write-hex-file, code stream not assembled"))
                    (loop (cdr lst)
                          pos
                          rev-bytes))
                  (let ((new-pos
                         (+ pos 1))
                        (new-rev-bytes
                         (cons x
                               (if (= (modulo pos 16) 0)
                                   (begin
                                     (print-line 0
                                                 (- pos (length rev-bytes))
                                                 (reverse rev-bytes))
                                     '())
                                   rev-bytes))))
                    (loop (cdr lst)
                          new-pos
                          new-rev-bytes))))
            (begin
              (if (pair? rev-bytes)
                  (print-line 0
                              (- pos (length rev-bytes))
                              (reverse rev-bytes)))
              (print-line 1 0 '())
              (if #t
                  (begin
                    (display pos ##stderr-port)
                    (display " ROM bytes\n" ##stderr-port)))))))))

;; Utilities.

(define (asm-make-stream) ;; create an empty stream
  (let ((x (cons '() '())))
    (set-car! x x)
    x))
     
(define (asm-code-extend cb item) ;; add an item at the end of current code stream
  (let* ((stream (asm-code-block-stream cb))
         (tail (car stream))
         (cell (cons item '())))
    (set-cdr! tail cell)
    (set-car! stream cell)))

(declare (generic))

(define (asm-bits-0-to-7 n) ;; return bits 0 to 7 of a signed integer
  (modulo n #x100))

(define (asm-bits-8-and-up n) ;; return bits 8 and up of a signed integer
  (if (>= n 0)
      (quotient n #x100)
      (- (quotient (+ n 1) #x100) 1)))

(define (asm-bits-16-and-up n) ;; return bits 16 and up of a signed integer
  (if (>= n 0)
      (quotient n #x10000)
      (- (quotient (+ n 1) #x10000) 1)))

(define (asm-bits-32-and-up n) ;; return bits 32 and up of a signed integer
  (if (>= n 0)
      (quotient n #x100000000)
      (- (quotient (+ n 1) #x100000000) 1)))

;; The following procedures convert floating point numbers into their
;; ANSI-IEEE Std 754-1985 representation (32 bit and 64 bit floats).
;; They perform bignum and flonum arithmetic.

(define asm-inexact-+2   (exact->inexact 2))
(define asm-inexact--2   (exact->inexact -2))
(define asm-inexact-+1   (exact->inexact 1))
(define asm-inexact-+1/2 (exact->inexact (/ 1 2)))
(define asm-inexact-+0   (exact->inexact 0))

(define (asm-float->inexact-exponential-format x f64?)
  (let* ((e-bits (if f64? 11 8))
         (e-bias (- (expt 2 (- e-bits 1)) 1)))

    (define (float-copysign x y)
      (if (negative? y)
          (- x)
          x))

    (define (exp-form-pos x y i)
      (let ((i*2 (+ i i)))
        (let ((z (if (and (not (< e-bias i*2))
                          (not (< x y)))
                     (exp-form-pos x (* y y) i*2)
                     (vector x 0 1))))
          (let ((a (vector-ref z 0)) (b (vector-ref z 1)))
            (let ((i+b (+ i b)))
              (if (and (not (< e-bias i+b))
                       (not (< a y)))
                  (begin
                    (vector-set! z 0 (/ a y))
                    (vector-set! z 1 i+b)))
              z)))))

    (define (exp-form-neg x y i)
      (let ((i*2 (+ i i)))
        (let ((z (if (and (< i*2 (- e-bias 1))
                          (< x y))
                     (exp-form-neg x (* y y) i*2)
                     (vector x 0 1))))
          (let ((a (vector-ref z 0)) (b (vector-ref z 1)))
            (let ((i+b (+ i b)))
              (if (and (< i+b (- e-bias 1))
                       (< a y))
                  (begin
                    (vector-set! z 0 (/ a y))
                    (vector-set! z 1 i+b)))
              z)))))

    (define (exp-form x)
      (if (< x asm-inexact-+1)
          (let ((z (exp-form-neg x asm-inexact-+1/2 1)))
            (vector-set! z 0 (* asm-inexact-+2 (vector-ref z 0)))
            (vector-set! z 1 (- -1 (vector-ref z 1)))
            z)
          (exp-form-pos x asm-inexact-+2 1)))

    (if (negative? (float-copysign asm-inexact-+1 x))
        (let ((z (exp-form (float-copysign x asm-inexact-+1))))
          (vector-set! z 2 -1)
          z)
        (exp-form x))))

(define (asm-float->exact-exponential-format x f64?)
  (let* ((z      (asm-float->inexact-exponential-format x f64?))
         (m-bits (if f64? 52 23))
         (e-bits (if f64? 11 8)))

    (let ((y (vector-ref z 0)))
      (if (not (< y asm-inexact-+2)) ;; +inf.0 or +nan.0?
          (begin
            (if (< asm-inexact-+0 y)
                (vector-set! z 0 (expt 2 m-bits))              ;; +inf.0
                (vector-set! z 0 (- (* (expt 2 m-bits) 2) 1))) ;; +nan.0
            (vector-set! z 1 (expt 2 (- e-bits 1))))
          (vector-set! z 0
                       (truncate
                        (inexact->exact
                         (* (vector-ref z 0)
                            (exact->inexact (expt 2 m-bits)))))))
      (vector-set! z 1 (- (vector-ref z 1) m-bits))
      z)))

(define (asm-float->bits x f64?)
  (let ((m-bits (if f64? 52 23))
        (e-bits (if f64? 11 8)))

    (define (bits a b)
      (let ((m-min (expt 2 m-bits)))
        (if (< a m-min)
          a
          (+ (- a m-min)
             (* (+ (+ b m-bits) (- (expt 2 (- e-bits 1)) 1))
                m-min)))))

    (let* ((z (asm-float->exact-exponential-format x f64?))
           (y (bits (vector-ref z 0) (vector-ref z 1))))
      (if (negative? (vector-ref z 2))
        (+ (expt 2 (+ e-bits m-bits)) y)
        y))))
