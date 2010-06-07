;;;============================================================================

;;; File: "js2scm-rt.scm", Time-stamp: <2010-06-06 19:52:15 feeley>

;;; Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(include "js2scm-rt#.scm")

;;; Predefined functions.

(define (_print this x)
  (println (to-string x)))

(define (_read this filename)
  (call-with-input-file filename
    (lambda (port)
      (read-line port #f))))

(define (_String #!optional (value ""))
  (to-string value))

;;; JavaScript operators.

(define (js:* x y)
  (if (and #f ;; assume the only numerical type is fixnum
           (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (* x y)
      (error "js:* expects numbers" x y)))

(define (js:+ x y)
  (if (and #f ;; assume the only numerical type is fixnum
           (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (+ x y)
      (string-append (to-string x) (to-string y))))

(define (to-string obj)
  (cond ((number? obj) (number->string obj))
        ((string? obj) obj)
        ((boolean? obj) (if obj "true" "false"))
        ((##unbound? obj) "undefined")
        (else (error "to-string cannot handle" obj))))

(define (js:- x y)
  (if (and #f ;; assume the only numerical type is fixnum
           (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (- x y)
      (error "js:- expects numbers" x y)))

(define (js:< x y)
  (if (and #f ;; assume the only numerical type is fixnum
           (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (< x y)
      (error "js:< expects numbers" x y)))

(define (js:> x y)
  (if (and #f ;; assume the only numerical type is fixnum
           (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (> x y)
      (error "js:> expects numbers" x y)))

(define (js:<= x y)
  (if (and #f ;; assume the only numerical type is fixnum
           (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (<= x y)
      (error "js:<= expects numbers" x y)))

(define (js:>= x y)
  (if (and #f ;; assume the only numerical type is fixnum
           (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (<= x y)
      (error "js:<= expects numbers" x y)))

(define (js:== x y)
  (if (and #f ;; assume the only numerical type is fixnum
           (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (= x y)
      (error "js:= expects numbers" x y)))

(define (js:!= x y)
  (if (and #f ;; assume the only numerical type is fixnum
           (or (##fixnum? x) (##flonum? x))
           (or (##fixnum? y) (##flonum? y)))
      (not (= x y))
      (error "js:!= expects numbers" x y)))

(define (js:index obj field)
  (cond ((string? obj) 
         (cond ((equal? field "length")
                (string-length obj))
               ((equal? field "charCodeAt")
                (lambda (self i)
                  (char->integer (string-ref self i))))
               (else
                (error "unknown string field" field))))
        ((eq? obj _String)
         (cond ((equal? field "fromCharCode")
                (lambda lst
                  (list->string (map integer->char lst))))
               (else
                (error "unknown String field" field))))
        ((procedure? obj)
         (cond ((equal? field "apply")
                (lambda (self args)
                  (apply obj (cons self (array->list args)))))
               (else
                (error "unknown function field" field))))
        ((array? obj)
         (cond ((equal? field "length")
                (array-len obj))
               ((equal? field "push")
                array-push)
               ((number? field)
                (vector-ref (array-vect obj) field))
               (else
                (error "unknown array field" field))))
        (else
         (table-ref obj field))))

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
        ((array? obj)
         (cond ((number? field)
                (vector-set! (array-vect obj) field value))
               (else
                (error "unknown array field" field))))
        (else
         (table-set! obj field value))))

(define (js:array-lit . elems)
  (let ((obj (make-empty-array)))
    (for-each
     (lambda (elem)
       (array-push obj elem))
     elems)))

(define-type array len vect)

(define (make-empty-array)
  (make-array 0 (vector #f)))

(define (array-push arr x)
  (let ((i (vector-ref arr 0))
        (v (vector-ref arr 1)))
    (if (fx< i (vector-length v))
        (begin
          (vector-set! v i x)
          (vector-set! arr 0 (fx+ i 1)))
        (let* ((n (fx* 2 (vector-length v)))
               (new-v (make-vector n #f)))
          (subvector-move! v 0 i new-v 0)
          (vector-set! new-v i x)
          (vector-set! arr 0 (fx+ i 1))
          (vector-set! arr 1 new-v)))))

(define (array->list arr)
  (vector->list (subvector (array-vect arr) 0 (array-len arr))))

(define (vector->array vect)
  (make-array (vector-length vect) vect))

(define _arguments
  (vector->array (list->vector (cdr (command-line)))))

(define (main . lst)
  0)

;;;============================================================================
