;; _________________________________________________________________________
;;
;;             Tachyon : A Self-Hosted JavaScript Virtual Machine
;;
;;
;;  This file is part of the Tachyon JavaScript project. Tachyon is
;;  distributed at:
;;  http://github.com/Tachyon-Team/Tachyon
;;
;;
;;  Copyright (c) 2011, Universite de Montreal
;;  All rights reserved.
;;
;;  This software is licensed under the following license (Modified BSD
;;  License):
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions are
;;  met:
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in the
;;      documentation and/or other materials provided with the distribution.
;;    * Neither the name of the Universite de Montreal nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;;  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;;  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;;  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
;;  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;; _________________________________________________________________________

;;;============================================================================

;;; File: "build-keyword-ht.scm", Time-stamp: <2010-06-21 14:45:43 feeley>

;;; Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Script to build the perfect hastable for the keywords of
;;; ECMAScript 5 (ECMA-262 5th edition / September 2009).  The result
;;; is ECMAScript code that is to be pasted into "scanner.js".

;;; It is best to compile this file, as it can be time consuming to
;;; compute a perfect hashtable.  For example:
;;;
;;; % gsc -prelude '(include "keywords.scm")' build-keyword-ht.scm
;;; % gsi build-keyword-ht
;;; var HASH_MOD = 147;
;;; var HASH_MULT = 17;
;;; 
;;; var keyword_hashtable =
;;; [
;;;  { id: "const", cat: CONST_CAT }
;;; ...

;;;----------------------------------------------------------------------------

;; This code assumes that the variable "keywords" is bound to the
;; list of all keywords (strings).

(declare (standard-bindings) (not safe) (fixnum))

(define (scanner-hash-string str)
  (let ((len (string-length str)))
    (let loop ((h 0) (i 0))
      (if (< i len)
          (let ((n (modulo (+ (* h scanner-hash-mult)
                              (char->integer (string-ref str i)))
                           scanner-hash-mod)))
            (loop n
                  (+ i 1)))
          h))))

(define scanner-hash-mod  #f)
(define scanner-hash-mult #f)

(define (sort l)

  (define (mergesort l)

    (define (merge l1 l2)
      (cond ((null? l1) l2)
            ((null? l2) l1)
            (else
             (let ((e1 (car l1)) (e2 (car l2)))
               (if (< e1 e2)
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

(define (duplicates? lst)
  (let loop ((l (sort lst)))
    (if (or (null? l) (null? (cdr l)))
      #f
      (if (= (car l) (cadr l))
        #t
        (loop (cdr l))))))

(define (found)

  (define v (make-vector scanner-hash-mod #f))

  (for-each
   (lambda (keyword)
     (vector-set! v (scanner-hash-string keyword) keyword))
   keywords)

  (print "var HASH_MOD = " scanner-hash-mod ";\n")
  (print "var HASH_MULT = " scanner-hash-mult ";\n")
  (print "\n")
  (print "var keyword_hashtable =\n")
  (print "[\n")
  (let loop ((i 0))
    (if (< i (vector-length v))
        (let ((word (vector-ref v i)))
          (print (if (= i 0) " " ","))
          (if word
              (let* ((name
                      word)
                     (cat
                      (string-append
                       (list->string (map char-upcase (string->list name)))
                       "_CAT")))
                (print "{ id: \"" name "\", cat: " cat " }"))
              (begin
                (print "null")))
          (print "\n")
          (loop (+ i 1)))))
  (print "];\n"))

(call-with-current-continuation
 (lambda (abort)
   (let loop1 ((i (length keywords)))
     (if (<= i 1024)
         (begin
           (set! scanner-hash-mod i)
           (let loop2 ((j 1))
             (if (<= j 16384)
                 (begin
                   (set! scanner-hash-mult j)
                   (if (duplicates? (map scanner-hash-string keywords))
                       (loop2 (+ j 1))
                       (begin
                         (found)
                         (abort #f))))))
           (loop1 (+ i 1)))))))

;;;============================================================================
