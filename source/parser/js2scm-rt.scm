;;;============================================================================

;;; File: "js2scm-rt.scm", Time-stamp: <2011-03-01 23:46:57 feeley>

;;; Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(define cmd-args (cdr (command-line)))

(define stats?
  (if (and (pair? cmd-args) (equal? (car cmd-args) "-stats"))
      (begin
        (set! cmd-args (cdr cmd-args))
        #t)
      #f))

(include "js2scm-rt#.scm")

(define (sort l <?)

  (define (mergesort l)

    (define (merge l1 l2)
      (cond ((null? l1) l2)
            ((null? l2) l1)
            (else
             (let ((e1 (car l1)) (e2 (car l2)))
               (if (<? e1 e2)
                 (cons e1 (merge (cdr l1) l2))
                 (cons e2 (merge l1 (cdr l2))))))))

    (define (split l)
      (if (or (null? l) (null? (cdr l)))
        l
        (cons (car l) (split (cddr l)))))

    (if (or (null? l) (null? (cdr l)))
      l
      (let* ((l1 (mergesort (split l)))
             (l2 (mergesort (split (cdr l)))))
        (merge l1 l2))))

  (mergesort l))

(define stats-ref #f)
(define stats-set #f)

(define (init-stats)
  (set! stats-ref (make-table)) 
  (set! stats-set (make-table)))


(if stats?
    (begin
      (init-stats)
      (##add-exit-job!
       (lambda ()
         (define (sort-stats stats)
           (sort (table->list stats)
                 (lambda (x y)
                   (> (cdr x) (cdr y)))))
         (pp (list 'stats-ref (sort-stats stats-ref)))
         (pp (list 'stats-set (sort-stats stats-set)))))))

;;;----------------------------------------------------------------------------

;; Representation of objects

#|
(define (make-assoc-table)
  (make-table))

(define (list->assoc-table lst)
  (list->table lst))

(define (assoc-table->list at)
  (table->list at))

(define (assoc-table-ref at key not-found)
  (if stats?
      (let ((len (table-length at)))
        (table-set! stats-ref len (+ 1 (table-ref stats-ref len 0)))))
  (table-ref at key not-found))

(define (assoc-table-set! at key val)
  (if stats?
      (let ((len (table-length at)))
        (table-set! stats-set len (+ 1 (table-ref stats-set len 0)))))
  (table-set! at key val))
|#

(define (make-assoc-table)
  (list #f))

(define (list->assoc-table lst)
  (cons #f lst))

(define (assoc-table->list at)
  (cdr at))

(define (assoc-table-ref at key not-found)
  (let ((x (assoc key (cdr at))))
    (if x
        (cdr x)
        not-found)))

(define (assoc-table-set! at key val)
  (let ((x (assoc key (cdr at))))
    (if x
        (set-cdr! x val)
        (let loop ((lst at))
          (let ((rest (cdr lst)))
            (if (pair? rest)
                (loop rest)
                (set-cdr! lst (list (cons key val)))))))))




(define prototypes (make-table test: eq? weak-keys: #t))

(define (get-prototype ctor)
  (if (not (procedure? ctor))
      (error "can only get the prototype property of a function" ctor)
      (let ((p (table-ref prototypes ctor #f)))
        (or p
            (let ((prototype (make-empty-Object)))
              (table-set! prototypes ctor prototype)
              prototype)))))

(define (set-prototype ctor value)
  (if (not (procedure? ctor))
      (error "can only set the prototype of a function" ctor)
      (begin
        (table-set! prototypes ctor value)
        value)))

(define obj-proxies (make-table test: eq? weak-keys: #t))

(define (get-obj-proxy val)
  (let ((x (table-ref obj-proxies val #f)))
        (or x
            (let ((obj
                   (cond ((procedure? val)
                          (make-Function _Function
                                         (get-prototype _Function)
                                         (make-assoc-table)
                                         val))
                         ((string? val)
                          (make-String _String
                                       (get-prototype _String)
                                       (make-assoc-table)
                                       val))
                         ((number? val)
                          (make-Number _Number
                                       (get-prototype _Number)
                                       (make-assoc-table)
                                       val))
                         (else
                          (error "get-obj-proxy unknown type" val)))))
              (table-set! obj-proxies val obj)
              obj))))

(define (has-own-prop? self name)
  (let ((x (assoc-table-ref (Object-at self) name assoc-table-ref)))
    (not (eq? x assoc-table-ref))))

(define (has-prop? self name)
  (not (eq? (js:index self name) (js.undefined))))

(define-type Object
  extender: define-type-of-Object
  ctor
  proto
  at ;; assoc-table
)

(define (make-obj ctor)
  (cond ((eq? ctor _Array)
         (make-empty-Array))
        (else
         (let ((proto (get-prototype ctor))
               (at (make-assoc-table)))
           (make-Object ctor
                        proto
                        at)))))

(define (_Object this . args)
  (if (pair? args)
      (error "Object with 1 argument not implemented")
      (make-empty-Object)))

(define (create-Object proto)
  (make-Object _Object
               proto
               (make-assoc-table)))

(define Object-prototype
  (create-Object '())) ;; prototype is null

(table-set! prototypes _Object Object-prototype)

(define (make-empty-Object)
  (make-Object _Object
               Object-prototype
               (make-assoc-table)))

(define-type-of-Object Function
  fn
)

(define-type-of-Object String
  str
)

(define-type-of-Object Number
  num
)

(define-type-of-Object Array
  len
  vect
)

(define-type-of-Object Error
)

;;;----------------------------------------------------------------------------

;;; Predefined variables in global object.

(define (_print this x)
  (println (to-string x)))

(define (_read this filename)
  (call-with-input-file filename
    (lambda (port)
      (read-line port #f))))

(define (_error this message)
  (println (to-string message))
  (exit))

(define (_quit this)
  (exit))

(define _undefined
  (js.undefined))

(define _Nan +nan.0)

(define _Infinity +inf.0)

(define (_Boolean this . args)
  (js.undefined))

(define (_Number this . args)
  (js.undefined))

(define (_Function this . args)
  (js.undefined))

(define (_Array this . args)
  (js.undefined))

(define (_Error this . args)
  (js:index-set! this "args" args)
  (js.undefined))

(define (_String this #!optional (value "") . args)
  (to-string value))

;;;----------------------------------------------------------------------------

(define memory (make-table))

(define memory-addr 50)

(define (_malloc this size)
  (let ((addr (list->Array (list memory-addr 0 0 0))))
    (set! memory-addr (+ 1 memory-addr))
    (table-set! memory addr (make-u8vector size))
    addr))

(define (_free this addr)
  (table-set! memory addr)
  (js.undefined))

(define (_exit this status)
  (exit status))

(define (_puts this str)
  (println str)
  (js.undefined))

(define (_writeFile this filename str)
  (call-with-output-file
      filename
    (lambda (port)
      (display str port)
      (js.undefined))))

(define (_readFile this filename)
  (call-with-input-file
      filename
    (lambda (port)
      (read-line port #f))))

(define (_shellCommand this command)
  (call-with-input-process
   (list path: "/bin/sh" arguments: (list "-c" command))
   (lambda (port)
     (read-line port #f))))

(define (_readConsole this)
  (read-line))

(define (_rawAllocMachineCodeBlock this size)
  (_malloc this size))

(define (_rawFreeMachineCodeBlock this addr)
  (_free this addr))

(define (_rawCallTachyonFFI this . args)
  (pp (cons '_rawCallTachyonFFI (cons this args)))
  (js.undefined))

(define (_getFuncAddr this fn)
  (let ((addr
         (cond ((equal? fn "malloc")
                0)
               ((equal? fn "free")
                1)
               ((equal? fn "exit")
                2)
               ((equal? fn "puts")
                3)
               ((equal? fn "printInt")
                4)
               ((equal? fn "sum2Ints")
                5)
               ((equal? fn "writeFile")
                6)
               ((equal? fn "readFile")
                7)
               ((equal? fn "shellCommand")
                8)
               ((equal? fn "readConsole")
                9)
               ((equal? fn "rawAllocMachineCodeBlock")
                10)
               ((equal? fn "rawFreeMachineCodeBlock")
                11)
               ((equal? fn "rawCallTachyonFFI")
                12)
               ((equal? fn "getFuncAddr")
                13)
               (else
                (error "getFuncAddr: unknown function" fn)))))
    (list->Array (list addr 0 0 0))))

;;;----------------------------------------------------------------------------

;; Property getter and setter

(define (js:index obj prop)

  (define (get-in-at)
    (let ((x (assoc-table-ref (Object-at obj) prop assoc-table-ref)))
      (if (eq? x assoc-table-ref)
          (let ((proto (Object-proto obj)))
            (if (null? proto)
                (js.undefined)
                (js:index proto prop)))
          x)))

  (cond ((equal? prop "prototype")
         (get-prototype obj))
        ((equal? prop "hasOwnProperty")
         has-own-prop?)
        ((string? obj) 
         (cond ((equal? prop "length")
                (string-length obj))
               (else
                (js:index (get-obj-proxy obj) prop))))
        ((Array? obj)
         (cond ((equal? prop "length")
                (Array-len obj))
               ((number? prop)
                (if (fx< prop (Array-len obj))
                    (vector-ref (Array-vect obj) prop)
                    (js.undefined)))
               (else
                (get-in-at))))
        ((Object? obj)
         (get-in-at))
        (else
         (js:index (get-obj-proxy obj) prop))))

(define (js:index-set! obj prop value)

  (define (set-in-at)
    (assoc-table-set! (Object-at obj) prop value))

  (cond ((equal? prop "prototype")
         (set-prototype obj value))
        ((Array? obj)
         (cond ((number? prop)
                (Array-resize-if-needed obj (fx+ prop 1))
                (vector-set! (Array-vect obj) prop value))
               (else
                (set-in-at))))
        ((Object? obj)
         (set-in-at))
        (else
         (js:index-set! (get-obj-proxy obj) prop value))))

;;;----------------------------------------------------------------------------

;; Object

(js:index-set!
 _Object
 "create"
 (lambda (self proto)
   (create-Object proto)))

;;;----------------------------------------------------------------------------

;; Function

(js:index-set!
 (js:index _Function "prototype")
 "apply"
 (lambda (self self2 args)
   (apply self (cons self2 (Array->list args)))))

(js:index-set!
 (js:index _Function "prototype")
 "call"
 (lambda (self self2 . args)
   (apply self (cons self2 args))))

;;;----------------------------------------------------------------------------

;; Array

(define (make-empty-Array)
  (make-Array _Array
              (get-prototype _Array)
              (make-assoc-table)
              0
              (vector (js.undefined))))

(define (Array-resize-if-needed arr len)
  (let ((v (Array-vect arr)))
    (if (fx< (vector-length v) len)
        (let* ((n (fx+ 1 (fx* 2 len)))
               (new-v (make-vector n (js.undefined))))
          (subvector-move! v 0 (Array-len arr) new-v 0)
          (Array-vect-set! arr new-v)))
    (if (fx< (Array-len arr) len)
        (Array-len-set! arr len))))

(define (Array-for-each-index arr fn)
  (let ((len (Array-len arr)))
    (let loop ((i 0))
      (if (fx< i len)
          (begin
            (fn i)
            (loop (fx+ i 1)))))))

(define (Array->vector arr)
  (subvector (Array-vect arr) 0 (Array-len arr)))

(define (Array->list arr)
  (vector->list (Array->vector arr)))

(define (vector->Array vect)
  (make-Array _Array
              (get-prototype _Array)
              (make-assoc-table)
              (vector-length vect)
              vect))

(define (list->Array lst)
  (vector->Array (list->vector lst)))

(define (Array-toString self)
  (Array-join self ","))

(define (Array-concat self . args)
  (let ((a (list->vector args)))
    (let loop1 ((i (fx- (vector-length a) 1)) (len (Array-len self)))
      (if (fx>= i 0)
          (let ((x (vector-ref a i)))
            (loop1 (fx- i 1)
                   (fx+ len (if (Array? x) (Array-len x) 1))))
          (let ((v (make-vector len)))
            (let loop2 ((i (fx- (vector-length a) 1)) (len len))
              (if (fx>= i 0)
                  (let ((x (vector-ref a i)))
                    (if (Array? x)
                        (let loop3 ((j (fx- (Array-len x) 1)) (len len))
                          (if (fx>= j 0)
                              (let ((len (fx- len 1)))
                                (vector-set! v len (vector-ref (Array-vect x) j))
                                (loop3 (fx- j 1)
                                       len))
                              (loop2 (fx- i 1)
                                     len)))
                        (let ((len (fx- len 1)))
                          (vector-set! v len x)
                          (loop2 (fx- i 1)
                                 len))))
                  (let loop4 ((j (fx- (Array-len self) 1)) (len len))
                    (if (fx>= j 0)
                        (let ((len (fx- len 1)))
                          (vector-set! v len (vector-ref (Array-vect self) j))
                          (loop4 (fx- j 1)
                                 len))
                        (vector->Array v))))))))))

(define (Array-join self #!optional (separator (js.undefined)))
  (let* ((sep (if (eq? separator (js.undefined)) "," (to-string separator)))
         (v (Array-vect self)))
    (let loop ((i 0) (str "") (s ""))
      (if (fx< i (vector-length v))
          (loop (fx+ i 1)
                (string-append str s (to-string (vector-ref v i)))
                sep)
          str))))

(define (Array-pop self)
  (let ((len (Array-len self)))
    (if (fx> len 0)
        (let* ((v (Array-vect self))
               (x (vector-ref v (fx- len 1))))
          (Array-len-set! self (fx- len 1))
          (Array-vect-set! self (subvector v 0 (fx- len 1)))
          x)
        (js.undefined))))

(define (Array-push self x)
  (let ((i (Array-len self))
        (v (Array-vect self)))
    (if (fx< i (vector-length v))
        (begin
          (vector-set! v i x)
          (Array-len-set! self (fx+ i 1)))
        (let* ((n (fx+ 1 (fx* 2 (vector-length v))))
               (new-v (make-vector n (js.undefined))))
          (subvector-move! v 0 i new-v 0)
          (vector-set! new-v i x)
          (Array-len-set! self (fx+ i 1))
          (Array-vect-set! self new-v)))))

(define (Array-reverse self)
  (list->Array
   (reverse
    (Array->list self))))

(define (Array-shift self)
  (let ((len (Array-len self)))
    (if (fx> len 0)
        (let* ((v (Array-vect self))
               (x (vector-ref v 0)))
          (Array-len-set! self (fx- len 1))
          (Array-vect-set! self (subvector v 1 len))
          x)
        (js.undefined))))

(define (Array-slice self start #!optional (end (js.undefined)))
  (let* ((len (Array-len self))
         (s (if (fx< start 0)
                (max 0 (fx+ len start))
                (min start len)))
         (e (if (eq? end (js.undefined))
                len
                (max s
                     (if (fx< end 0)
                         (fx+ len end)
                         (min end len))))))
    (vector->Array
     (subvector (Array-vect self) s e))))

(define (Array-sort self #!optional (comparefn (js.undefined)))
  (error "Array-sort unimplemented"))

(define (Array-splice self start delcount . items)
  (let* ((v (Array-vect self))
         (v0 (subvector v start (fx+ start delcount)))
         (v1 (subvector v 0 start))
         (v2 (list->vector items))
         (v3 (subvector v (fx+ start delcount) (Array-len self)))
         (new-v (vector-append v1 v2 v3)))
    (Array-vect-set! self new-v)
    (Array-len-set! self (vector-length new-v))
    (vector->Array v0)))

(define (Array-unshift self . args)
  (error "Array-unshift unimplemented"))

(define (Array-indexOf self #!optional (elem (js.undefined)) (from (js.undefined)))
  (let* ((len (Array-len self))
         (f (if (eq? from (js.undefined))
                0
                (if (fx< from 0)
                    (let ((f (fx+ len from)))
                      (if (fx< f 0)
                          0
                          f))
                    from))))
    (let loop ((i f))
      (if (fx< i len)
          (if (js:=== elem (vector-ref (Array-vect self) i))
              i
              (loop (fx+ i 1)))
          -1))))

(define (Array-lastIndexOf self #!optional (elem (js.undefined)) (from (js.undefined)))
  (let* ((len (Array-len self))
         (f (if (or (eq? from (js.undefined)) (fx>= from len))
                (fx- len 1)
                (if (fx< from 0)
                    (fx+ len from)
                    from))))
    (let loop ((i f))
      (if (fx>= i 0)
          (if (js:=== elem (vector-ref (Array-vect self) i))
              i
              (loop (fx- i 1)))
          -1))))

(define (Array-forEach self callback)
  (let loop ((i 0))
    (if (fx< i (Array-len self))
        (let ((value (vector-ref (Array-vect self) i)))
          (callback '() value i self)
          (loop (fx+ i 1))))))

(define (Array-map self callback)
  (let ((result (make-empty-Array)))
    (let loop ((i 0))
      (if (fx< i (Array-len self))
          (let ((value (vector-ref (Array-vect self) i)))
            (Array-push result (callback '() value i self))
            (loop (fx+ i 1)))
          result))))

(define (Array-filter self callback)
  (error "Array-filter unimplemented"))

(js:index-set! (js:index _Array "prototype") "toString"    Array-toString)
(js:index-set! (js:index _Array "prototype") "concat"      Array-concat)
(js:index-set! (js:index _Array "prototype") "join"        Array-join)
(js:index-set! (js:index _Array "prototype") "pop"         Array-pop)
(js:index-set! (js:index _Array "prototype") "push"        Array-push)
(js:index-set! (js:index _Array "prototype") "reverse"     Array-reverse)
(js:index-set! (js:index _Array "prototype") "shift"       Array-shift)
(js:index-set! (js:index _Array "prototype") "slice"       Array-slice)
(js:index-set! (js:index _Array "prototype") "sort"        Array-sort)
(js:index-set! (js:index _Array "prototype") "splice"      Array-splice)
(js:index-set! (js:index _Array "prototype") "unshift"     Array-unshift)
(js:index-set! (js:index _Array "prototype") "indexOf"     Array-indexOf)
(js:index-set! (js:index _Array "prototype") "lastIndexOf" Array-lastIndexOf)
(js:index-set! (js:index _Array "prototype") "forEach"     Array-forEach)
(js:index-set! (js:index _Array "prototype") "map"         Array-map)
(js:index-set! (js:index _Array "prototype") "filter"      Array-filter)

;;;----------------------------------------------------------------------------

;; String

(js:index-set!
 (js:index _String "prototype")
 "concat"
 (lambda (self . args)
   (apply string-append args)))

(js:index-set!
 (js:index _String "prototype")
 "toString"
 (lambda (self)
   self))

(js:index-set!
 (js:index _String "prototype")
 "charAt"
 (lambda (self i)
   (string (string-ref self i))))

(js:index-set!
 (js:index _String "prototype")
 "charCodeAt"
 (lambda (self i)
   (char->integer (string-ref self i))))

(js:index-set!
 _String
 "fromCharCode"
 (lambda (self . lst)
   (list->string (map integer->char lst))))

(js:index-set!
 (js:index _String "prototype")
 "split"
 (lambda (self separator)
   (if (not (and (string? separator)
                 (= 1 (string-length separator))))
       (error "String.split expects a separator of length 1")
       (list->Array
        (call-with-input-string
            self
          (lambda (p)
            (read-all p (lambda (p) (read-line p (string-ref separator 0))))))))))

(define (to-string obj)
  (cond ((and (Object? obj)
              (has-prop? obj "toString"))
         (js.call (js.index obj "toString")))
        ((number? obj) (number->string obj))
        ((string? obj) obj)
        ((boolean? obj) (if obj "true" "false"))
        ((eq? obj (js.undefined)) "undefined")
        (else (object->string obj))))

;;;----------------------------------------------------------------------------

;; Number

(js:index-set!
 (js:index _Number "prototype")
 "toString"
 (lambda (self)
   (number->string self)))

;;;----------------------------------------------------------------------------

;; Math

(define _Math (make-empty-Object))

(js:index-set!
 _Math
 "min"
 (lambda (self x y)
   (fxmin x y)))

(js:index-set!
 _Math
 "max"
 (lambda (self x y)
   (fxmax x y)))

(js:index-set!
 _Math
 "floor"
 (lambda (self x)
   (floor x)))

(js:index-set!
 _Math
 "pow"
 (lambda (self x y)
   (expt x y)))

;;;----------------------------------------------------------------------------

;; Command-line arguments

(define _arguments
  (list->Array cmd-args))

;;;----------------------------------------------------------------------------

;;; JavaScript operators.

(define (js:* x y)
  (if (and (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (* x y)
      (error "js:* expects numbers" x y)))

(define (js:+ x y)
  (if (and (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (+ x y)
      (string-append (to-string x) (to-string y))))

(define (js:- x y)
  (if (and (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (- x y)
      (error "js:- expects numbers" x y)))

(define (js:< x y)
  (if (and (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (< x y)
      (error "js:< expects numbers" x y)))

(define (js:> x y)
  (if (and (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (> x y)
      (error "js:> expects numbers" x y)))

(define (js:<= x y)
  (if (and (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (<= x y)
      (error "js:<= expects numbers" x y)))

(define (js:>= x y)
  (if (and (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (<= x y)
      (error "js:<= expects numbers" x y)))

(define (js:== x y)
  (js:=== x y)) ;; FIXME

(define (js:!= x y)
  (not (js:== x y)))

(define (js:=== x y)
  (cond ((and (number? x) (number? y))
         (= x y))
        ((and (string? x) (string? y))
         (string=? x y))
        (else
         (eq? x y))))

(define (js:!== x y)
  (not (js:=== x y)))

(define (js:array-lit . elems)
  (let ((obj (make-empty-Array)))
    (for-each
     (lambda (elem)
       (Array-push obj elem))
     elems)
    obj))

(define (js:obj-lit . props)
  (make-Object _Object
               Object-prototype
               (list->assoc-table props)))

(define (js:forin set iteration)
  (let ((keys (map car (assoc-table->list (Object-at set)))))
    (for-each iteration keys)
    (if (Array? set)
        (Array-for-each-index set iteration))))

(define (js:throw obj)
  (pp (assoc-table->list (Object-at obj)));;;;;;;;;;;;;;;
  (raise obj))

(define (js:instanceof x ctor)
  (cond ((Object? x)
         (or (eq? (Object-ctor x) ctor)
             (js:instanceof (Object-proto x) ctor)))
        ((procedure? x)
         (js:instanceof (get-obj-proxy x) ctor))
        (else
         #f)))

(define (js:typeof obj)
  (cond ((boolean? obj) "boolean")
        ((number? obj) "number")
        ((string? obj) "string")
        ((procedure? obj) "function")
        ((eq? obj (js.undefined)) "undefined")
        (else "object")))

(init-stats) ;; start stats from scratch

(define (main . lst)
  0)

;;;============================================================================
