;;;============================================================================

;;; File: "js2scm-rt.scm", Time-stamp: <2010-06-08 22:01:36 feeley>

;;; Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(include "js2scm-rt#.scm")

(define-macro (undefined)
  `(##type-cast -7 2)) ;; #!unbound object

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

(define (_exit this)
  (exit))

(define _undefined
  (undefined))

(define (_Boolean)
  #f);;;;;;;;;;;;;

(define (_Number)
  #f);;;;;;;;;;;;;

(define (_String #!optional (value ""))
  (to-string value))

(define (_Function)
  #f);;;;;;;;;;;;;

(define (_Array . lst)
  #f);;;;;;;;;;;;;

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

(define (js:index obj field)
  (cond ((string? obj) 
         (cond ((equal? field "length")
                (string-length obj))
               ((equal? field "charCodeAt")
                (lambda (self i)
                  (char->integer (string-ref self i))))
               (else
                (error "unknown string field" field))))
        ((procedure? obj)
         (cond ((equal? field "apply")
                (lambda (self self2 args)
                  (apply obj (cons self2 (Array->list args)))))
               ((eq? obj _String)
                (cond ((equal? field "fromCharCode")
                       (lambda (self . lst)
                         (list->string (map integer->char lst))))
                      ((equal? field "prototype")
                       'String-prototype)
                      (else
                       (error "unknown String function field" field))))
               ((eq? obj _Array)
                (cond ((equal? field "prototype")
                       'Array-prototype)
                      (else
                       (error "unknown Array function field" field))))
               (else
                (error "unknown function field" field))))
        ((eq? obj 'Array-prototype)
         (cond ((equal? field "concat")
                (lambda (self . args)
                  (vector->Array
                   (apply vector-append
                          (map Array->vector (cons self args))))))
               (else
                (error "unknown Array prototype field" field))))
        ((eq? obj 'String-prototype)
         (cond ((equal? field "concat")
                (lambda (self . args)
                  (apply string-append
                         (cons self args))))
               (else
                (error "unknown String prototype field" field))))
        ((Array? obj)
         (cond ((equal? field "length")
                (Array-len obj))
               ((equal? field "push")
                Array-push)
               ((equal? field "concat")
                (lambda lst
                  (vector->Array
                   (apply vector-append
                          (map Array->vector lst)))))
               ((equal? field "forEach")
                (lambda (self callback)
                  (let loop ((i 0))
                    (if (fx< i (Array-len self))
                        (let ((value (vector-ref (Array-vect self) i)))
                          (callback '() value i self)
                          (loop (fx+ i 1)))))))
               ((equal? field "map")
                (lambda (self callback)
                  (let ((result (make-empty-Array)))
                    (let loop ((i 0))
                      (if (fx< i (Array-len self))
                          (let ((value (vector-ref (Array-vect self) i)))
                            (Array-push result (callback '() value i self))
                            (loop (fx+ i 1)))
                          result)))))
               ((number? field)
                (Array-resize-if-needed obj (fx+ field 1))
                (vector-ref (Array-vect obj) field))
               (else
                (error "unknown Array field" field))))
        ((number? obj)
         (cond ((equal? field "toString")
                (lambda (self)
                  (number->string self)))
               (else
                (error "unknown number field" field))))
        ((Object? obj)
         (table-ref (Object-ht obj) field (undefined)))
        (else
         (error "unknown field" obj field))))

(define (js:index-set! obj field value)
  (cond ((string? obj) 
         (cond (else
                (error "unknown string field" field))))
        ((eq? obj _String)
         (cond (else
                (error "unknown String field" field))))
        ((procedure? obj)
         (cond (else
                (error "unknown function field" field))))
        ((Array? obj)
         (cond ((number? field)
                (Array-resize-if-needed obj (fx+ field 1))
                (vector-set! (Array-vect obj) field value))
               (else
                (error "unknown Array field" field))))
        ((Object? obj)
         (table-set! (Object-ht obj) field value))
        (else
         (error "unknown field" obj field))))

(define (js:array-lit . elems)
  (let ((obj (make-empty-Array)))
    (for-each
     (lambda (elem)
       (Array-push obj elem))
     elems)
    obj))

(define-type Object
  extender: define-type-of-Object
  ctor
  ht
)

(define-type-of-Object Array
  len
  vect
)

(define (make-empty-Array)
  (make-Array _Array (make-table) 0 (vector #f)))

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
  (make-Array _Array (make-table) (vector-length vect) vect))

(define (js:instanceof x y)
  (cond ((Object? x)
         (eq? (Object-ctor x) y))
        (else
         #f)))

(define (js:forin set iteration)
  (let ((keys (map car (table->list (Object-ht set)))))
    (for-each iteration keys)))

(define _arguments
  (vector->Array (list->vector (cdr (command-line)))))

(define _Math
  (make-Object #f (list->table (list (cons "min" (lambda (self x y) (fxmin x y)))))))

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
