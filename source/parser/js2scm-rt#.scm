;;;============================================================================

;;; File: "js2scm-rt#.scm", Time-stamp: <2010-06-06 19:59:06 feeley>

;;; Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(declare
 (standard-bindings)
 (extended-bindings)
 (not safe)
 (block)
 (not inline)
)

;;; JavaScript forms.

(define-macro (js.var variable value)
  `(define ,variable ,value))

(define-macro (js.function params body)
  `(lambda (this ,@params) ,body))

(define-macro (js.this)
  `this)

(define-macro (js.if test consequent alternative)
  `(if ,test ,consequent ,alternative))

(define-macro (js.call fn . args)
  (if (and (pair? fn)
           (eq? (car fn) 'js.index))
      `(let* ((self ,(cadr fn)) (fn (js.index self ,(caddr fn))))
         (fn self ,@args))
      `(,fn '() ,@args)))

(define-macro (js.new ctor . args)
  `(let* ((self (make-table)) (ctor ,ctor))
     (ctor self ,@args)
     self))

(define-macro (js.index obj field)
  `(js:index ,obj ,field))

(define-macro (js.index-set! obj field value)
  `(js:index-set! ,obj ,field ,value))

(define-macro (js.array-lit . elems)
  `(js:array-lit ,@elems))

(define-macro (js.obj-lit . props)
  `(list->table (list ,@props)))

(define-macro (js.prop name value)
  `(cons ,name ,value))

;;; JavaScript operators.

(define-macro (js.delete x)
  `())

(define-macro (js.void x)
  `())

(define-macro (js.typeof x)
  `())

(define-macro (js.++x x)
  (if (and (pair? x)
           (eq? (car x) 'js.index))
      `(let* ((self ,(cadr x)) (key ,(caddr x)) (res (js.+ (js.index self key) 1)))
         (js.index-set! self key res)
         res)
      `(let ((res (js.+ ,x 1)))
         (set! ,x res)
         res)))

(define-macro (js.auto++x x)
  `())

(define-macro (js.--x x)
  (if (and (pair? x)
           (eq? (car x) 'js.index))
      `(let* ((self ,(cadr x)) (key ,(caddr x)) (res (js.- (js.index self key) 1)))
         (js.index-set! self key res)
         res)
      `(let ((res (js.- ,x 1)))
         (set! ,x res)
         res)))

(define-macro (js.auto--x x)
  `())

(define-macro (js.~ x)
  `())

(define-macro (js.! x)
  `(let ((x ,x))
     (not x))) ;; is this correct?

(define-macro (js.x++ x)
  (if (and (pair? x)
           (eq? (car x) 'js.index))
      `(let* ((self ,(cadr x)) (key ,(caddr x)) (res (js.index self key)))
         (js.index-set! self key (js.+ res 1))
         res)
      `(let ((res ,x))
         (set! ,x (js.+ res 1))
         res)))

(define-macro (js.x-- x)
  (if (and (pair? x)
           (eq? (car x) 'js.index))
      `(let* ((self ,(cadr x)) (key ,(caddr x)) (res (js.index self key)))
         (js.index-set! self key (js.- res 1))
         res)
      `(let ((res ,x))
         (set! ,x (js.- res 1))
         res)))

(define-macro (js.* x y)
  (if #f ;; assume only numerical type is fixnum and no overflow
      `(let* ((x ,x) (y ,y))
         (if (##fixnum? x)
             (if (##fixnum? y)
                 (let ((r (##fx*? x y)))
                   (or r (js:* x y)))
                 (js:* x y))
             (js:* x y)))
      `(##fx* ,x ,y)))

(define-macro (js./ x y)
  `())

(define-macro (js.% x y)
  `())

(define-macro (js.+ x . y)
  (if (null? y)
      `(js.+ 0 ,x)
      (if #f ;; assume only numerical type is fixnum and no overflow
          `(let* ((x ,x) (y ,(car y)))
             (if (##fixnum? x)
                 (if (##fixnum? y)
                     (let ((r (##fx+? x y)))
                       (or r (js:+ x y)))
                     (js:+ x y))
                 (js:+ x y)))
          `(##fx+ ,x ,(car y)))))

(define-macro (js.- x . y)
  (if (null? y)
      `(js.- 0 ,x)
      (if #f ;; assume only numerical type is fixnum and no overflow
          `(let* ((x ,x) (y ,(car y)))
             (if (##fixnum? x)
                 (if (##fixnum? y)
                     (let ((r (##fx-? x y)))
                       (or r (js:- x y)))
                     (js:- x y))
                 (js:- x y)))
          `(##fx- ,x ,(car y)))))

(define-macro (js.<< x y)
  `())

(define-macro (js.>> x y)
  `())

(define-macro (js.>>> x y)
  `())

(define-macro (js.< x y)
  (if #f ;; assume only numerical type is fixnum and no overflow
      `(let* ((x ,x) (y ,y))
         (if (##fixnum? x)
             (if (##fixnum? y)
                 (##fx< x y)
                 (js:< x y))
             (js:< x y)))
      `(##fx< ,x ,y)))

(define-macro (js.> x y)
  (if #f ;; assume only numerical type is fixnum and no overflow
      `(let* ((x ,x) (y ,y))
         (if (##fixnum? x)
             (if (##fixnum? y)
                 (##fx> x y)
                 (js:> x y))
             (js:> x y)))
      `(##fx> ,x ,y)))

(define-macro (js.<= x y)
  (if #f ;; assume only numerical type is fixnum and no overflow
      `(let* ((x ,x) (y ,y))
         (if (##fixnum? x)
             (if (##fixnum? y)
                 (##fx<= x y)
                 (js:<= x y))
             (js:<= x y)))
      `(##fx<= ,x ,y)))

(define-macro (js.>= x y)
  (if #f ;; assume only numerical type is fixnum and no overflow
      `(let* ((x ,x) (y ,y))
         (if (##fixnum? x)
             (if (##fixnum? y)
                 (##fx>= x y)
                 (js:>= x y))
             (js:>= x y)))
      `(##fx>= ,x ,y)))

(define-macro (js.instanceof x y)
  `())

(define-macro (js.in x y)
  `())

(define-macro (js.== x y)
  (if #f ;; assume only numerical type is fixnum and no overflow
      `(let* ((x ,x) (y ,y))
         (if (##fixnum? x)
             (if (##fixnum? y)
                 (##fx= x y)
                 #f)
             (js:== x y)))
      `(##fx= ,x ,y)))

(define-macro (js.!= x y)
  (if #f ;; assume only numerical type is fixnum and no overflow
      `(let* ((x ,x) (y ,y))
         (if (##fixnum? x)
             (if (##fixnum? y)
                 (not (##fx= x y))
                 #t)
             (js:!= x y)))
      `(not (##fx= ,x ,y))))

(define-macro (js.=== x y)
  `(let* ((x ,x) (y ,y))
     (eq? x y))) ;;; is this correct?

(define-macro (js.!== x y)
  `(let* ((x ,x) (y ,y))
     (not (eq? x y)))) ;;; is this correct?

(define-macro (js.& x y)
  `())

(define-macro (js.^ x y)
  `())

(define-macro (|js.\|| x y)
  `())

(define-macro (js.&& x y)
  `())

(define-macro (|js.\|\|| x y)
  `())

(define-macro (|js.,| x y)
  `())

(define-macro (js.= x y)
  (if (and (pair? x)
           (eq? (car x) 'js.index))
      `(let* ((self ,(cadr x)) (key ,(caddr x)))
         (js.index-set! self key ,y))
      `(set! ,x ,y)))

(define-macro (js.+= x y)
  `())

(define-macro (js.-= x y)
  `())

(define-macro (js.*= x y)
  `())

(define-macro (js./= x y)
  `())

(define-macro (js.<<= x y)
  `())

(define-macro (js.>>= x y)
  `())

(define-macro (js.>>>= x y)
  `())

(define-macro (js.&= x y)
  `())

(define-macro (js.^= x y)
  `())

(define-macro (|js.\|=| x y)
  `())

(define-macro (js.%= x y)
  `())

;;;============================================================================
