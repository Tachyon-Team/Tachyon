;;;============================================================================

;;; File: "js2scm-rt.scm", Time-stamp: <2010-12-17 09:53:11 feeley>

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

(define (Array-push arr x)
  (let ((i (Array-len arr))
        (v (Array-vect arr)))
    (if (fx< i (vector-length v))
        (begin
          (vector-set! v i x)
          (Array-len-set! arr (fx+ i 1)))
        (let* ((n (fx* 2 (vector-length v)))
               (new-v (make-vector n (js.undefined))))
          (subvector-move! v 0 i new-v 0)
          (vector-set! new-v i x)
          (Array-len-set! arr (fx+ i 1))
          (Array-vect-set! arr new-v)))))

(define (Array-pop arr)
  (let ((len (Array-len arr)))
    (if (fx> len 0)
        (let* ((v (Array-vect arr))
               (x (vector-ref v (fx- len 1))))
          (Array-len-set! arr (fx- len 1))
          (Array-vect-set! arr (subvector v 0 (fx- len 1)))
          x)
        (js.undefined))))

(define (Array-shift arr)
  (let ((len (Array-len arr)))
    (if (fx> len 0)
        (let* ((v (Array-vect arr))
               (x (vector-ref v 0)))
          (Array-len-set! arr (fx- len 1))
          (Array-vect-set! arr (subvector v 1 len))
          x)
        (js.undefined))))

(define (Array-resize-if-needed arr len)
  (let ((v (Array-vect arr)))
    (if (fx< (vector-length v) len)
        (let* ((n (fx* 2 len))
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

(js:index-set!
 (js:index _Array "prototype")
 "concat"
 (lambda (self . args)
   (vector->Array
    (apply vector-append
           (map Array->vector (cons self args))))))

(js:index-set!
 (js:index _Array "prototype")
 "slice"
 (lambda (self start #!optional (end (js.undefined)))
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
      (subvector (Array-vect self) s e)))))

(js:index-set!
 (js:index _Array "prototype")
 "splice"
 (lambda (self start delcount . items)
   (let* ((v (Array-vect self))
          (v0 (subvector v start (fx+ start delcount)))
          (v1 (subvector v 0 start))
          (v2 (list->vector items))
          (v3 (subvector v (fx+ start delcount) (Array-len self)))
          (new-v (vector-append v1 v2 v3)))
     (Array-vect-set! self new-v)
     (Array-len-set! self (vector-length new-v))
     (vector->Array v0))))

(js:index-set!
 (js:index _Array "prototype")
 "push"
 Array-push)

(js:index-set!
 (js:index _Array "prototype")
 "pop"
 Array-pop)

(js:index-set!
 (js:index _Array "prototype")
 "shift"
 Array-shift)

(js:index-set!
 (js:index _Array "prototype")
 "forEach"
 (lambda (self callback)
   (let loop ((i 0))
     (if (fx< i (Array-len self))
         (let ((value (vector-ref (Array-vect self) i)))
           (callback '() value i self)
           (loop (fx+ i 1)))))))

(js:index-set!
 (js:index _Array "prototype")
 "map"
 (lambda (self callback)
   (let ((result (make-empty-Array)))
     (let loop ((i 0))
       (if (fx< i (Array-len self))
           (let ((value (vector-ref (Array-vect self) i)))
             (Array-push result (callback '() value i self))
             (loop (fx+ i 1)))
           result)))))

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

(define (js:instanceof x y)
  (cond ((Object? x)
         (or (eq? (Object-ctor x) y)
             (js:instanceof (Object-proto x) y)))
        (else
         #f)))

(define (js:forin set iteration)
  (let ((keys (map car (assoc-table->list (Object-at set)))))
    (for-each iteration keys)
    (if (Array? set)
        (Array-for-each-index set iteration))))

(define (js:throw obj)
  (pp (table->list (Object-at obj)));;;;;;;;;;;;;;;
  (raise obj))

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
