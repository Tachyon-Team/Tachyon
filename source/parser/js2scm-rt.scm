;;;============================================================================

;;; File: "js2scm-rt.scm", Time-stamp: <2010-06-23 22:25:23 feeley>

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

(define-macro (undefined)
  `(##type-cast -7 2)) ;; #!unbound object

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

(define-type Object
  extender: define-type-of-Object
  ctor
  proto
  at ;; assoc-table
)

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

(define prototypes (make-table test: eq? weak-keys: #t))

(define (get-prototype ctor)
  (if (not (procedure? ctor))
      (error "can only get the prototype of a function" ctor)
      (let ((p (table-ref prototypes ctor #f)))
        (or p
            (let ((prototype (make-Object #f #f (make-assoc-table))))
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

(define (make-empty-Array)
  (make-Array _Array (get-prototype _Array) (make-assoc-table) 0 (vector #f)))

(define (Array-push arr x)
  (let ((i (Array-len arr))
        (v (Array-vect arr)))
    (if (fx< i (vector-length v))
        (begin
          (vector-set! v i x)
          (Array-len-set! arr (fx+ i 1)))
        (let* ((n (fx* 2 (vector-length v)))
               (new-v (make-vector n #f)))
          (subvector-move! v 0 i new-v 0)
          (vector-set! new-v i x)
          (Array-len-set! arr (fx+ i 1))
          (Array-vect-set! arr new-v)))))

(define (Array-resize-if-needed arr len)
  (let ((v (Array-vect arr)))
    (if (fx< (vector-length v) len)
        (let* ((n (fx* 2 len))
               (new-v (make-vector n #f)))
          (subvector-move! v 0 (Array-len arr) new-v 0)
          (Array-vect-set! arr new-v)))
    (if (fx< (Array-len arr) len)
        (Array-len-set! arr len))))

(define (Array->vector arr)
  (subvector (Array-vect arr) 0 (Array-len arr)))

(define (Array->list arr)
  (vector->list (Array->vector arr)))

(define (vector->Array vect)
  (make-Array _Array (get-prototype _Array) (make-assoc-table) (vector-length vect) vect))

;;; Predefined functions.

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
  (undefined))

(define (_Boolean)
  #f);;;;;;;;;;;;;

(define (_Number)
  #f);;;;;;;;;;;;;

(define (_Function)
  #f);;;;;;;;;;;;;

(define (_Array . lst)
  #f);;;;;;;;;;;;;

(define (_String #!optional (value ""))
  (to-string value))

(define (js:index obj field)

  (define (get-in-at)
    (let ((x (assoc-table-ref (Object-at obj) field assoc-table-ref)))
      (if (eq? x assoc-table-ref)
          (let ((proto (Object-proto obj)))
            (if proto
                (js:index proto field)
                (undefined)))
          x)))

  (cond ((equal? field "prototype")
         (get-prototype obj))
        ((string? obj) 
         (cond ((equal? field "length")
                (string-length obj))
               (else
                (js:index (get-obj-proxy obj) field))))
        ((Array? obj)
         (cond ((equal? field "length")
                (Array-len obj))
               ((number? field)
                (Array-resize-if-needed obj (fx+ field 1))
                (vector-ref (Array-vect obj) field))
               (else
                (get-in-at))))
        ((Object? obj)
         (get-in-at))
        (else
         (js:index (get-obj-proxy obj) field))))

(define (js:index-set! obj field value)

  (define (set-in-at)
    (assoc-table-set! (Object-at obj) field value))

  (cond ((equal? field "prototype")
         (set-prototype obj value))
        ((Array? obj)
         (cond ((number? field)
                (Array-resize-if-needed obj (fx+ field 1))
                (vector-set! (Array-vect obj) field value))
               (else
                (set-in-at))))
        ((Object? obj)
         (set-in-at))
        (else
         (js:index-set! (get-obj-proxy obj) field value))))

(js:index-set!
 (js:index _Function "prototype")
 "apply"
 (lambda (self self2 args)
   (apply self (cons self2 (Array->list args)))))

(js:index-set!
 (js:index _Array "prototype")
 "concat"
 (lambda (self . args)
   (vector->Array
    (apply vector-append
           (map Array->vector (cons self args))))))

(js:index-set!
 (js:index _Array "prototype")
 "push"
 Array-push)

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

(js:index-set!
 (js:index _String "prototype")
 "concat"
 (lambda (self . args)
   (apply string-append
          (cons self args))))

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
 (js:index _Number "prototype")
 "toString"
 (lambda (self)
   (number->string self)))

(init-stats) ;; start stats from scratch

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

(define (to-string obj)
  (cond ((number? obj) (number->string obj))
        ((string? obj) obj)
        ((boolean? obj) (if obj "true" "false"))
        ((##unbound? obj) "undefined")
        (else (object->string obj))))

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
  (if (and (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (= x y)
      (if (and (string? x) (string? y))
          (string=? x y)
          (eq? x y))))

(define (js:!= x y)
  (not (js:== x y)))

(define (js:array-lit . elems)
  (let ((obj (make-empty-Array)))
    (for-each
     (lambda (elem)
       (Array-push obj elem))
     elems)
    obj))

(define (js:instanceof x y)
  (cond ((Object? x)
         (eq? (Object-ctor x) y))
        (else
         #f)))

(define (js:forin set iteration)
  (let ((keys (map car (assoc-table->list (Object-at set)))))
    (for-each iteration keys)))

(define _arguments
  (vector->Array (list->vector cmd-args)))

(define _Math
  (make-Object #f #f (list->assoc-table (list (cons "min" (lambda (self x y) (fxmin x y)))))))

(define (js:typeof obj)
  (cond ((boolean? obj) "boolean")
        ((number? obj) "number")
        ((string? obj) "string")
        ((procedure? obj) "function")
        ((eq? obj (undefined)) "undefined")
        (else "object")))

(define (main . lst)
  0)

;;;============================================================================
