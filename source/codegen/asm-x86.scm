(define (rtl-make-code-gen-context target fixup-list code-block lbls fs lbl-counter globals deferred-code)
  (vector 'code-gen-context
          target
          fixup-list
          code-block
          lbls
          fs
          lbl-counter
          globals
          deferred-code))

(define (rtl-code-gen-context-target cgc)               (vector-ref cgc 1))
(define (rtl-code-gen-context-target-set! cgc x)        (vector-set! cgc 1 x))
(define (rtl-code-gen-context-fixup-list cgc)           (vector-ref cgc 2))
(define (rtl-code-gen-context-fixup-list-set! cgc x)    (vector-set! cgc 2 x))
(define (rtl-code-gen-context-code-block cgc)           (vector-ref cgc 3))
(define (rtl-code-gen-context-code-block-set! cgc x)    (vector-set! cgc 3 x))
(define (rtl-code-gen-context-lbls cgc)                 (vector-ref cgc 4))
(define (rtl-code-gen-context-lbls-set! cgc x)          (vector-set! cgc 4 x))
(define (rtl-code-gen-context-fs cgc)                   (vector-ref cgc 5))
(define (rtl-code-gen-context-fs-set! cgc x)            (vector-set! cgc 5 x))
(define (rtl-code-gen-context-lbl-counter cgc)          (vector-ref cgc 6))
(define (rtl-code-gen-context-lbl-counter-set! cgc x)   (vector-set! cgc 6 x))
(define (rtl-code-gen-context-globals cgc)              (vector-ref cgc 7))
(define (rtl-code-gen-context-globals-set! cgc x)       (vector-set! cgc 7 x))
(define (rtl-code-gen-context-deferred-code cgc)        (vector-ref cgc 8))
(define (rtl-code-gen-context-deferred-code-set! cgc x) (vector-set! cgc 8 x))

(define (gen-8 cgc n)  (asm-8 (rtl-code-gen-context-code-block cgc) n))
(define (gen-16 cgc n) (asm-16 (rtl-code-gen-context-code-block cgc) n))
(define (gen-32 cgc n) (asm-32 (rtl-code-gen-context-code-block cgc) n))
(define (gen-64 cgc n) (asm-64 (rtl-code-gen-context-code-block cgc) n))

(define (gen-imm-num cgc k width)
  (cond ((fx= width 8)
         (let ((n (signed-lo8 k)))
           (gen-8 cgc n)
           n))
        ((fx= width 16)
         (let ((n (signed-lo16 k)))
           (gen-16 cgc n)
           n))
        ((fx= width 32)
         (let ((n (signed-lo32 k)))
           (gen-32 cgc n)
           n))
        (else
         (let ((n (signed-lo64 k)))
           (gen-64 cgc n)
           n))))

(define (gen-imm-lbl cgc label offset width)

  (assert (fx= width 32)
          "gen-imm-lbl expects width=32" width)

  (let ((lbl (asm-make-label (rtl-code-gen-context-code-block cgc) 'fixup)))
    (rtl-code-gen-context-fixup-list-set!
     cgc
     (cons lbl
           (rtl-code-gen-context-fixup-list cgc)))
    (asm-label (rtl-code-gen-context-code-block cgc) lbl)
    (asm-at-assembly

     (rtl-code-gen-context-code-block cgc)

     (lambda (cb self)
       4)
     (lambda (cb self)
       (asm-32 cb (fx+ (asm-label-pos label) offset))))))

(define (gen-listing? cgc)
  (asm-code-block-listing? (rtl-code-gen-context-code-block cgc)))

(define (gen-listing cgc text)
  (asm-listing (rtl-code-gen-context-code-block cgc) text))

(define (x86-offset->string offset)
  (cond ((fx= offset 0) "")
        ((fx< offset 0) (number->string offset))
        (else           (string-append "+" (number->string offset)))))

(define (x86-opnd-format-gnu opnd) ;; GNU syntax
  (cond ((x86-reg? opnd)
         (list "%" (x86-reg-name opnd)))
        ((x86-imm? opnd)
         (list "$"
               (if (x86-imm-num? opnd)
                   (x86-imm-num-value opnd)
                   (list (asm-label-name (x86-imm-lbl-label opnd))
                         (x86-offset->string (x86-imm-lbl-offset opnd))))))
        ((x86-glo? opnd)
         (let ((name (x86-glo-name opnd))
               (offset (x86-glo-offset opnd)))
           (list name
                 (x86-offset->string offset))))
        ((x86-mem? opnd)
         (let ((reg1 (x86-mem-reg1 opnd))
               (reg2 (x86-mem-reg2 opnd))
               (scale (x86-mem-scale opnd))
               (offset (x86-mem-offset opnd)))
           (if reg1
               (let ((x
                      (cons "("
                            (cons (x86-opnd-format-gnu reg1)
                                  (if reg2
                                      (cons ","
                                            (cons (x86-opnd-format-gnu reg2)
                                                  (if (fx= scale 0)
                                                      '(")")
                                                      (list ","
                                                            (fxarithmetic-shift-left
                                                             1
                                                             scale)
                                                            ")"))))
                                      '(")"))))))
                 (if (fx= offset 0) x (cons offset x)))
               offset)))
        (else
         opnd)))

(define (x86-opnd-format opnd)
  (x86-opnd-format-gnu opnd))

(define (x86-imm? x) (pair? x))

(define (x86-imm-num value) (cons value '()))
(define (x86-imm-num? x) (and (pair? x) (null? (cdr x))))
(define (x86-imm-num-value x) (car x))

(define (x86-imm-lbl label offset) (cons label offset))
(define (x86-imm-lbl? x) (and (pair? x) (not (null? (cdr x)))))
(define (x86-imm-lbl-label x) (car x))
(define (x86-imm-lbl-offset x) (cdr x))

(define (x86-glo name #!optional (offset 0))
  (vector name offset))

(define (x86-glo? x) (and (vector? x) (fx= (vector-length x) 2)))
(define (x86-glo-name x) (vector-ref x 0))
(define (x86-glo-offset x) (vector-ref x 1))

(define (x86-mem offset #!optional (reg1 #f) (reg2 #f) (scale 0))
  (vector offset reg1 reg2 scale))

(define (x86-mem? x) (and (vector? x) (fx= (vector-length x) 4)))
(define (x86-mem-offset x) (vector-ref x 0))
(define (x86-mem-reg1 x) (vector-ref x 1))
(define (x86-mem-reg2 x) (vector-ref x 2))
(define (x86-mem-scale x) (vector-ref x 3))

(define (x86-instr-format-gnu mnemonic size-suffix opnds)
  (let ((operands
         (if (pair? opnds)
             (let ((rest (cdr opnds)))
               (if (pair? rest)
                   (list (x86-opnd-format (car rest))
                         ","
                         (x86-opnd-format (car opnds)))
                   (list (x86-opnd-format (car opnds)))))
             '())))
    (cons #\tab
          (cons mnemonic
                (if size-suffix
                    (cons size-suffix
                          (if (pair? operands)
                              (cons #\tab
                                    operands)
                              '()))
                    (if (pair? operands)
                        (cons #\tab
                              (cons "*"
                                    operands))
                        '()))))))

(define (x86-instr-format mnemonic size-suffix . opnds)
  (x86-instr-format-gnu mnemonic size-suffix opnds))

(define (x86-32/64bit-mode-suffix cgc)
  (if (x86-64bit-mode? cgc) "q" "l"))

(define (x86-jump-label-suffix cgc short?)
  (if short? "" (x86-32/64bit-mode-suffix cgc)))

(define (x86-reg-width-suffix reg)
  (x86-width-suffix (x86-reg-width reg)))

(define (x86-width-suffix width)
  (cond ((fx= width 64) "q")
        ((fx= width 32) "l")
        ((fx= width 16) "w")
        ((fx= width 8)  "b")
        (else           "")))

(define (x86-xmm-width-suffix width)
  (cond ((fx= width 64) "d")
        (else           "s")))

(define (x86-gen-imm-num cgc k width)
  (gen-imm-num cgc k (fxmin 32 width)))

(define (x86-64bit-mode? cgc)
  (let ((targ (rtl-code-gen-context-target cgc)))
    (eq? (rtl-target-arch targ) 'x86-64)))

(define (x86-assert-64bit-mode cgc)
  (if (not (x86-64bit-mode? cgc))
      (error "instruction only valid for x86-64")))

(define (x86-assert-32bit-mode cgc)
  (if (x86-64bit-mode? cgc)
      (error "instruction only valid for x86")))

(define (assert x . msg)
  (if (not x) (apply error msg)))

(define (x86-opnd-prefix-reg-opnd cgc reg opnd)
  (let* ((width
          (x86-reg-width reg))
         (field
          (x86-reg-field reg))
         (ext-lo8-reg?
          (and (fx= width 8)
               (fx>= field 4)
               (not (x86-r8-h? reg)))))
    (if (x86-reg? opnd)
        (begin
          (assert (or (fx= width (x86-reg-width opnd))
                      (x86-xmm? reg)) ;; for cvtsi2ss/cvtsi2sd instructions
                  "registers are not of the same width" reg opnd)
          (let* ((field2
                  (x86-reg-field opnd))
                 (ext-lo8-reg2?
                  (and (fx= width 8)
                       (fx>= field2 4)
                       (not (x86-r8-h? opnd))))
                 (rex?
                  (x86-opnd-prefix cgc
                                   width
                                   field
                                   opnd
                                   (or ext-lo8-reg? ext-lo8-reg2?))))
            (assert (not (and rex?
                              (or (x86-r8-h? reg)
                                  (x86-r8-h? opnd))))
                    "cannot use high 8 bit register here" reg opnd)
            rex?))
        (x86-opnd-prefix cgc
                         width
                         field
                         opnd
                         ext-lo8-reg?))))

(define (x86-opnd-prefix-opnd cgc width opnd)
  (if (x86-reg? opnd)
      (let* ((field
              (x86-reg-field opnd))
             (ext-lo8-reg?
              (and (fx= width 8)
                   (fx>= field 4)
                   (not (x86-r8-h? opnd)))))
        (x86-opnd-prefix cgc width 0 opnd ext-lo8-reg?))
      (x86-opnd-prefix cgc width 0 opnd #f)))

(define (x86-opnd-modrm/sib-reg-opnd cgc reg opnd)
  (x86-opnd-modrm/sib cgc (x86-reg-field reg) opnd))

(define (x86-opnd-prefix cgc width field opnd force-rex?)
  (let ((rex*
         (fx+ ;; if needed emit REX.W (64 bit operand size)
              (if (or (fx= width 64)
                      (and (x86-reg? opnd) (x86-r64? opnd)))
                  8
                  0)
              ;; if needed emit REX.R (Extension of the ModR/M reg field)
              (fxarithmetic-shift-left
               (fxarithmetic-shift-right
                field
                3)
               2)
              (cond ((x86-reg? opnd)
                     ;; if needed emit REX.B (Extension of
                     ;; the ModR/M r/m field, SIB base field,
                     ;; or Opcode reg field)
                     (fxarithmetic-shift-right
                      (x86-reg-field opnd)
                      3))
                    ((x86-glo? opnd)
                     0)
                    ((x86-mem? opnd)
                     (let ((reg1 (x86-mem-reg1 opnd)))
                       (if reg1
                           (begin
                             (assert (or (x86-r32? reg1)
                                         (and (x86-r64? reg1)
                                              (x86-64bit-mode? cgc)))
                                     "invalid width base register" reg1)
                             (fx+ ;; if needed emit REX.B (Extension of
                                  ;; the ModR/M r/m field, SIB base field,
                                  ;; or Opcode reg field)
                                  (fxarithmetic-shift-right
                                   (x86-reg-field reg1)
                                   3)
                                  (let ((reg2 (x86-mem-reg2 opnd)))
                                    (if reg2
                                        (begin
                                          (assert (if (x86-r32? reg1)
                                                      (x86-r32? reg2)
                                                      (x86-r64? reg2))
                                                  "index register must have same width as base" reg2)
                                          ;; if needed emit REX.X (Extension
                                          ;; of the SIB index field)
                                          (fxarithmetic-shift-left
                                           (fxarithmetic-shift-right
                                            (x86-reg-field reg2)
                                            3)
                                           1))
                                        0))))
                           0)))
                    (else
                     (error "unknown operand" opnd))))))
    (x86-opnd-size-override-prefix cgc width)
    (x86-addr-size-override-prefix cgc opnd)
    (if (or force-rex?
            (not (fx= rex* 0)))
        (begin
          (x86-assert-64bit-mode cgc)
          (gen-8 cgc (fx+ #x40 rex*)) ;; REX
          #t)
        #f)))

(define (x86-opnd-size-override-prefix cgc width)
  (if (fx= width 16)
      (gen-8 cgc #x66))) ;; operand size override prefix

(define (x86-addr-size-override-prefix cgc opnd)
  (if (and (x86-mem? opnd)
           (let ((reg1 (x86-mem-reg1 opnd)))
             (and reg1
                  (eq? (x86-64bit-mode? cgc)
                       (not (x86-r64? reg1))))))
      (gen-8 cgc #x67))) ;; address size override prefix

(define (x86-opnd-modrm/sib cgc field opnd)
  (let ((modrm-rf
         (fxarithmetic-shift-left (fxand 7 field) 3)))

    (define (abs-addr)
      (if (x86-64bit-mode? cgc) ;; avoid RIP relative encoding?
          (begin
            (gen-8 cgc (fx+ modrm-rf 4)) ;; ModR/M
            (gen-8 cgc #x25))            ;; SIB
          (gen-8 cgc (fx+ modrm-rf 5)))) ;; ModR/M

    (cond ((x86-reg? opnd)
           (let ((modrm*
                  (fx+ modrm-rf (fxand 7 (x86-reg-field opnd)))))
             (gen-8 cgc (fx+ #xc0 modrm*)))) ;; ModR/M

          ((x86-glo? opnd)
           (abs-addr)
           (let ((name (x86-glo-name opnd))
                 (offset (x86-glo-offset opnd)))
             (gen-imm-lbl cgc (rtl-global-lookup cgc name) offset 32)))

          ((x86-mem? opnd)
           (let ((offset (x86-mem-offset opnd))
                 (reg1   (x86-mem-reg1 opnd)))

             (if reg1

                 (let* ((field1    (x86-reg-field reg1))
                        (field1-lo (fxand 7 field1))
                        (reg2      (x86-mem-reg2 opnd)))

                   (if (or reg2 ;; need a SIB when using an index
                           (fx= field1-lo 4)) ;; register or base = RSP/R12

                       ;; SIB needed

                       (let ((modrm*
                              (fx+ modrm-rf 4))
                             (sib
                              (fx+ field1-lo
                                   (if reg2
                                       (let ((field2 (x86-reg-field reg2)))
                                         (assert (not (fx= field2 4))
                                                 "SP not allowed as index" reg2)
                                         (fx+ (fxarithmetic-shift-left
                                               (fxand 7 field2)
                                               3)
                                              (fxarithmetic-shift-left
                                               (x86-mem-scale opnd)
                                               6)))
                                       #x20)))) ;; no index and no scaling

                         (if (signed8? offset)
                             (if (or (not (fx= offset 0)) ;; non-null offset?
                                     (fx= field1 5))      ;; or RBP
                                 (begin ;; use 8 bit displacement
                                   (gen-8 cgc (fx+ #x40 modrm*)) ;; ModR/M
                                   (gen-8 cgc sib) ;; SIB
                                   (gen-8 cgc offset))
                                 (begin
                                   (gen-8 cgc (fx+ #x00 modrm*)) ;; ModR/M
                                   (gen-8 cgc sib))) ;; SIB
                             (begin ;; use 32 bit displacement
                               (gen-8 cgc (fx+ #x80 modrm*)) ;; ModR/M
                               (gen-8 cgc sib)               ;; SIB
                               (gen-32 cgc offset))))

                       ;; SIB not needed

                       (let ((modrm*
                              (fx+ modrm-rf field1-lo)))
                         (if (signed8? offset)
                             (if (or (not (fx= offset 0)) ;; non-null offset?
                                     (fx= field1-lo 5)) ;; or RBP/R13
                                 (begin ;; use 8 bit displacement
                                   (gen-8 cgc (fx+ #x40 modrm*)) ;; ModR/M
                                   (gen-8 cgc offset))
                                 (gen-8 cgc (fx+ #x00 modrm*))) ;; ModR/M
                             (begin ;; use 32 bit displacement
                               (gen-8 cgc (fx+ #x80 modrm*)) ;; ModR/M
                               (gen-32 cgc offset))))))

                 (begin ;; absolute address, use disp32 ModR/M
                   (abs-addr)
                   (gen-32 cgc offset)))))

          (else
           (error "unknown operand" opnd)))))

(define (x86-add-imm cgc opnd k #!optional (width #f))
  (x86-op-imm cgc 0 "add" opnd k width))

(define (x86-or-imm cgc opnd k #!optional (width #f))
  (x86-op-imm cgc 1 "or" opnd k width))

(define (x86-adc-imm cgc opnd k #!optional (width #f))
 (x86-op-imm cgc 2 "adc" opnd k width))

(define (x86-sbb-imm cgc opnd k #!optional (width #f))
  (x86-op-imm cgc 3 "sbb" opnd k width))

(define (x86-and-imm cgc opnd k #!optional (width #f))
  (x86-op-imm cgc 4 "and" opnd k width))

(define (x86-sub-imm cgc opnd k #!optional (width #f))
  (x86-op-imm cgc 5 "sub" opnd k width))

(define (x86-xor-imm cgc opnd k #!optional (width #f))
  (x86-op-imm cgc 6 "xor" opnd k width))

(define (x86-cmp-imm cgc opnd k #!optional (width #f))
  (x86-op-imm cgc 7 "cmp" opnd k width))

(define (x86-add cgc opnd1 opnd2 #!optional (width #f))
  (x86-op cgc 0 "add" opnd1 opnd2 width))

(define (x86-or cgc opnd1 opnd2 #!optional (width #f))
  (x86-op cgc 1 "or" opnd1 opnd2 width))

(define (x86-adc cgc opnd1 opnd2 #!optional (width #f))
  (x86-op cgc 2 "adc" opnd1 opnd2 width))

(define (x86-sbb cgc opnd1 opnd2 #!optional (width #f))
  (x86-op cgc 3 "sbb" opnd1 opnd2 width))

(define (x86-and cgc opnd1 opnd2 #!optional (width #f))
  (x86-op cgc 4 "and" opnd1 opnd2 width))

(define (x86-sub cgc opnd1 opnd2 #!optional (width #f))
  (x86-op cgc 5 "sub" opnd1 opnd2 width))

(define (x86-xor cgc opnd1 opnd2 #!optional (width #f))
  (x86-op cgc 6 "xor" opnd1 opnd2 width))

(define (x86-cmp cgc opnd1 opnd2 #!optional (width #f))
  (x86-op cgc 7 "cmp" opnd1 opnd2 width))

(define (x86-mov cgc opnd1 opnd2 #!optional (width #f))
  (if (x86-imm-lbl? opnd2)
      (x86-mov-label cgc opnd1 (x86-imm-lbl-label opnd2) (x86-imm-lbl-offset opnd2));;;;;;;; should not be a special case, move logic to x86-op
      (x86-op cgc 17 "mov" opnd1 opnd2 width)))

(define (x86-op cgc op mnemonic opnd1 opnd2 width)

  ;; opnd1 = destination, opnd2 = source

  (define (gen-op reg opnd swapped?)

    (assert (or (not width)
                (fx= (x86-reg-width reg) width))
            "inconsistent operand width" width)

    (x86-opnd-prefix-reg-opnd cgc reg opnd)
    (gen-8 cgc (fx+ (fxarithmetic-shift-left op 3) ;; opcode
                    (if swapped? 0 2)
                    (if (x86-r8? reg) 0 1)))
    (x86-opnd-modrm/sib-reg-opnd cgc reg opnd)

    (if (gen-listing? cgc)
        (gen-listing
         cgc
         (x86-instr-format mnemonic
                           (x86-reg-width-suffix reg)
                           (if swapped? opnd reg)
                           (if swapped? reg opnd)))))

  (cond ((x86-imm-num? opnd2)
         (let ((k (x86-imm-num-value opnd2)))
           (if (fx= op 17) ;; move?
               (x86-mov-imm cgc opnd1 k width)
               (x86-op-imm cgc op mnemonic opnd1 k width))))
        ((x86-reg? opnd2)
         (gen-op opnd2 opnd1 #t))
        ((x86-reg? opnd1)
         (gen-op opnd1 opnd2 #f))
        (else
         (error "invalid operand combination" opnd1 opnd2))))

(define (x86-op-imm cgc op mnemonic opnd k width)

  ;; opnd = destination

  (define (listing width n)
    (if (gen-listing? cgc)
        (gen-listing
         cgc
         (x86-instr-format mnemonic
                           (x86-width-suffix width)
                           opnd
                           (x86-imm-num n)))))

  (define (accumulator width)
    (x86-opnd-size-override-prefix cgc width)
    ;; opcode = #x04, #x0c, #x14, ..., #x3c (for AL)
    ;;       or #x05, #x0d, #x15, ..., #x3d (for AX/EAX/RAX)
    (gen-8 cgc (fx+ (if (fx= width 8) #x04 #x05) ;; opcode
                    (fxarithmetic-shift-left op 3)))
    (listing width (x86-gen-imm-num cgc k width)))

  (define (general width)
    (x86-opnd-prefix-opnd cgc width opnd) ;; prefix
    (cond ((fx= width 8)
           (gen-8 cgc #x80) ;; opcode = 8 bit operation
           (x86-opnd-modrm/sib cgc op opnd) ;; ModR/M
           (listing width (x86-gen-imm-num cgc k 8)))
          ((signed8? k)
           (gen-8 cgc #x83) ;; opcode = sign extended 8 bit imm
           (x86-opnd-modrm/sib cgc op opnd) ;; ModR/M
           (listing width (x86-gen-imm-num cgc k 8)))
          (else
           (gen-8 cgc #x81) ;; opcode = sign extended 16/32 bit imm
           (x86-opnd-modrm/sib cgc op opnd) ;; ModR/M
           (listing width (x86-gen-imm-num cgc k width)))))

  (assert (if (x86-reg? opnd)
              (or (not width)
                  (fx= (x86-reg-width opnd) width))
              width)
          "missing or inconsistent operand width" width)

  (if (x86-reg? opnd)
      (let ((width (x86-reg-width opnd))
            (field (x86-reg-field opnd)))
        (if (and (fx= field 0)
                 (or (fx= width 8)
                     (not (signed8? k))))
            (accumulator width)
            (general width)))
      (general width)))

(define (x86-mov-imm cgc opnd k #!optional (width #f))

  ;; opnd = destination

  (define (listing width n)
    (if (gen-listing? cgc)
        (gen-listing
         cgc
         (x86-instr-format "mov"
                           (x86-width-suffix width)
                           opnd
                           (x86-imm-num n)))))

  (define (register width)
    (x86-opnd-prefix-opnd cgc width opnd) ;; prefix
    ;; opcode = #xb0-#xb7 (for 8 bit registers)
    ;;       or #xb8-#xbf (for 16/32/64 bit registers)
    (gen-8 cgc (fx+ (if (fx= width 8) #xb0 #xb8) ;; opcode
                    (fxand 7 (x86-reg-field opnd))))
    (listing width (gen-imm-num cgc k width)))

  (define (general width)
    (x86-opnd-prefix-opnd cgc width opnd) ;; prefix
    (gen-8 cgc (if (fx= width 8) #xc6 #xc7)) ;; opcode
    (x86-opnd-modrm/sib cgc 0 opnd) ;; ModR/M
    (listing width (x86-gen-imm-num cgc k width)))

  (assert (if (x86-reg? opnd)
              (or (not width)
                  (fx= (x86-reg-width opnd) width))
              width)
          "missing or inconsistent operand width" width)

  (if (x86-reg? opnd)
      (let ((width (x86-reg-width opnd)))
        (if (and (fx= width 64)
                 (signed32? k))
            (general width)
            (register width)))
      (general width)))

(define (x86-xmm-movss cgc opnd1 opnd2)
  (x86-xmm-movs cgc #x10 #f "movss" opnd1 opnd2))

(define (x86-xmm-movsd cgc opnd1 opnd2)
  (x86-xmm-movs cgc #x10 #t "movsd" opnd1 opnd2))

(define (x86-xmm-sqrtss cgc reg opnd)
  (x86-xmm-op cgc #x51 #f "sqrtss" reg opnd))

(define (x86-xmm-sqrtsd cgc reg opnd)
  (x86-xmm-op cgc #x51 #t "sqrtsd" reg opnd))

(define (x86-xmm-addss cgc reg opnd)
  (x86-xmm-op cgc #x58 #f "addss" reg opnd))

(define (x86-xmm-addsd cgc reg opnd)
  (x86-xmm-op cgc #x58 #t "addsd" reg opnd))

(define (x86-xmm-mulss cgc reg opnd)
  (x86-xmm-op cgc #x59 #f "mulss" reg opnd))

(define (x86-xmm-mulsd cgc reg opnd)
  (x86-xmm-op cgc #x59 #t "mulss" reg opnd))

(define (x86-xmm-cvtss2sd cgc reg opnd)
  (x86-xmm-op cgc #x5a #f "cvtss2sd" reg opnd))

(define (x86-xmm-cvtsd2ss cgc reg opnd)
  (x86-xmm-op cgc #x5a #t "cvtsd2ss" reg opnd))

(define (x86-xmm-cvtsi2ss cgc reg opnd #!optional (width #f))
  (x86-xmm-cvtsi cgc #x2a #f "cvtsi2ss" reg opnd width))

(define (x86-xmm-cvtsi2sd cgc reg opnd #!optional (width #f))
  (x86-xmm-cvtsi cgc #x2a #t "cvtsi2sd" reg opnd width))

(define (x86-xmm-cvttss2si cgc reg opnd)
  (x86-xmm-op cgc #x2c #f "cvttss2si" reg opnd))

(define (x86-xmm-cvttsd2si cgc reg opnd)
  (x86-xmm-op cgc #x2c #t "cvttsd2si" reg opnd))

(define (x86-xmm-cvtss2si cgc reg opnd)
  (x86-xmm-op cgc #x2d #f "cvtss2si" reg opnd))

(define (x86-xmm-cvtsd2si cgc reg opnd)
  (x86-xmm-op cgc #x2d #t "cvtsd2si" reg opnd))

(define (x86-xmm-subss cgc reg opnd)
  (x86-xmm-op cgc #x5c #f "subss" reg opnd))

(define (x86-xmm-subsd cgc reg opnd)
  (x86-xmm-op cgc #x5c #t "subsd" reg opnd))

(define (x86-xmm-minss cgc reg opnd)
  (x86-xmm-op cgc #x5d #f "minss" reg opnd))

(define (x86-xmm-minsd cgc reg opnd)
  (x86-xmm-op cgc #x5d #t "minsd" reg opnd))

(define (x86-xmm-divss cgc reg opnd)
  (x86-xmm-op cgc #x5e #f "divss" reg opnd))

(define (x86-xmm-divsd cgc reg opnd)
  (x86-xmm-op cgc #x5e #t "divsd" reg opnd))

(define (x86-xmm-maxss cgc reg opnd)
  (x86-xmm-op cgc #x5f #f "maxss" reg opnd))

(define (x86-xmm-maxsd cgc reg opnd)
  (x86-xmm-op cgc #x5f #t "maxsd" reg opnd))

(define (x86-xmm-movs cgc op double? mnemonic opnd1 opnd2)
  (cond ((x86-reg? opnd2)
         (x86-xmm-op-aux cgc op double? mnemonic opnd2 opnd1 #t #f))
        ((x86-reg? opnd1)
         (x86-xmm-op-aux cgc op double? mnemonic opnd1 opnd2 #f #f))
        (else
         (error "invalid operand combination" opnd1 opnd2))))

(define (x86-xmm-op cgc op double? mnemonic reg opnd)
  (x86-xmm-op-aux cgc op double? mnemonic reg opnd #f #f))

(define (x86-xmm-cvtsi cgc op double? mnemonic reg opnd width)
  (x86-xmm-op-aux cgc op double? mnemonic reg opnd #f width))

(define (x86-xmm-op-aux cgc op double? mnemonic reg opnd swapped? width)

  ;; reg = destination, opnd = source

  (let ((width
         (or width
             (if (x86-reg? opnd)
                 (fxmin (x86-reg-width reg)
                        (x86-reg-width opnd))
                 (x86-reg-width reg)))))

    (gen-8 cgc (if double? #xf2 #xf3)) ;; prefix
    (x86-opnd-prefix-opnd cgc width opnd) ;; prefix
    (gen-8 cgc #x0f) ;; opcode
    (gen-8 cgc (fx+ op ;; opcode
                    (if swapped? 1 0)))
    (x86-opnd-modrm/sib-reg-opnd cgc reg opnd)

    (if (gen-listing? cgc)
        (gen-listing
         cgc
         (x86-instr-format mnemonic
                           (x86-width-suffix width)
                           (if swapped? opnd reg)
                           (if swapped? reg opnd))))))

(define (x86-fpu-reg i)
  (vector-ref '#("%st"    "%st(1)" "%st(2)" "%st(3)"
                 "%st(4)" "%st(5)" "%st(6)" "%st(7)")
              i))

(define (x86-fnop cgc)    (x86-fpu-op0 cgc #xd0 "fnop"))
(define (x86-fchs cgc)    (x86-fpu-op0 cgc #xe0 "fchs"))
(define (x86-fabs cgc)    (x86-fpu-op0 cgc #xe1 "fabs"))
(define (x86-ftst cgc)    (x86-fpu-op0 cgc #xe4 "ftst"))
(define (x86-fxam cgc)    (x86-fpu-op0 cgc #xe5 "fxam"))
(define (x86-fld1 cgc)    (x86-fpu-op0 cgc #xe8 "fld1"))
(define (x86-fldl2t cgc)  (x86-fpu-op0 cgc #xe9 "fldl2t"))
(define (x86-fldl2e cgc)  (x86-fpu-op0 cgc #xea "fldl2e"))
(define (x86-fldpi cgc)   (x86-fpu-op0 cgc #xeb "fldpi"))
(define (x86-fldlg2 cgc)  (x86-fpu-op0 cgc #xec "fldlg2"))
(define (x86-fldln2 cgc)  (x86-fpu-op0 cgc #xed "fldln2"))
(define (x86-fldz cgc)    (x86-fpu-op0 cgc #xee "fldz"))
(define (x86-f2xm1 cgc)   (x86-fpu-op0 cgc #xf0 "f2xm1"))
(define (x86-fyl2x cgc)   (x86-fpu-op0 cgc #xf1 "fyl2x"))
(define (x86-fptan cgc)   (x86-fpu-op0 cgc #xf2 "fptan"))
(define (x86-fpatan cgc)  (x86-fpu-op0 cgc #xf3 "fpatan"))
(define (x86-fxtract cgc) (x86-fpu-op0 cgc #xf4 "fxtract"))
(define (x86-fprem1 cgc)  (x86-fpu-op0 cgc #xf5 "fprem1"))
(define (x86-fdecstp cgc) (x86-fpu-op0 cgc #xf6 "fdecstp"))
(define (x86-fincstp cgc) (x86-fpu-op0 cgc #xf7 "fincstp"))
(define (x86-fprem cgc)   (x86-fpu-op0 cgc #xf8 "fprem"))
(define (x86-fyl2xp1 cgc) (x86-fpu-op0 cgc #xf9 "fyl2xp1"))
(define (x86-fsqrt cgc)   (x86-fpu-op0 cgc #xfa "fsqrt"))
(define (x86-fsincos cgc) (x86-fpu-op0 cgc #xfb "fsincos"))
(define (x86-frndint cgc) (x86-fpu-op0 cgc #xfc "frndint"))
(define (x86-fscale cgc)  (x86-fpu-op0 cgc #xfd "fscale"))
(define (x86-fsin cgc)    (x86-fpu-op0 cgc #xfe "fsin"))
(define (x86-fcos cgc)    (x86-fpu-op0 cgc #xff "fcos"))

(define (x86-fpu-op0 cgc op mnemonic)

  (gen-8 cgc #xd9) ;; opcode
  (gen-8 cgc op) ;; opcode

  (if (gen-listing? cgc)
      (gen-listing
       cgc
       (x86-instr-format mnemonic
                         ""))))

(define (x86-fadd cgc i res-st0?)  (x86-fpu-opi1 cgc i res-st0? #xc0 "fadd"))
(define (x86-fmul cgc i res-st0?)  (x86-fpu-opi1 cgc i res-st0? #xc8 "fmul"))
(define (x86-fcom cgc i)           (x86-fpu-opi1 cgc i #t       #xd0 "fcom"))
(define (x86-fcomp cgc i)          (x86-fpu-opi1 cgc i #t       #xd8 "fcomp"))
(define (x86-fsub cgc i res-st0?)  (x86-fpu-opi1 cgc i res-st0? #xe0 "fsub"))
(define (x86-fsubr cgc i res-st0?) (x86-fpu-opi1 cgc i res-st0? #xe8 "fsubr"))
(define (x86-fdiv cgc i res-st0?)  (x86-fpu-opi1 cgc i res-st0? #xf0 "fdiv"))
(define (x86-fdivr cgc i res-st0?) (x86-fpu-opi1 cgc i res-st0? #xf8 "fdivr"))

(define (x86-fpu-opi1 cgc i res-st0? op mnemonic)

  (gen-8 cgc (if res-st0? #xd8 #xdc)) ;; opcode
  (gen-8 cgc ;; opcode
         (fx+ i
              (if (or (fx< op #xe0) res-st0?)
                  op
                  (fxxor op 8))))

  (if (gen-listing? cgc)
      (gen-listing
       cgc
       (x86-instr-format mnemonic
                         ""
                         (x86-fpu-reg (if res-st0? 0 i))
                         (x86-fpu-reg (if res-st0? i 0))))))

(define (x86-faddp cgc i)  (x86-fpu-opi2 cgc i #xc0 "faddp"))
(define (x86-fmulp cgc i)  (x86-fpu-opi2 cgc i #xc8 "fmulp"))
(define (x86-fsubp cgc i)  (x86-fpu-opi2 cgc i #xe0 "fsubrp"))
(define (x86-fsubrp cgc i) (x86-fpu-opi2 cgc i #xe8 "fsubp"))
(define (x86-fdivp cgc i)  (x86-fpu-opi2 cgc i #xf0 "fdivrp"))
(define (x86-fdivrp cgc i) (x86-fpu-opi2 cgc i #xf8 "fdivp"))

(define (x86-fpu-opi2 cgc i op mnemonic)

  (gen-8 cgc #xde) ;; opcode
  (gen-8 cgc (fx+ i op)) ;; opcode

  (if (gen-listing? cgc)
      (gen-listing
       cgc
       (x86-instr-format mnemonic
                         ""
                         (x86-fpu-reg i)
                         (x86-fpu-reg 0)))))

(define (x86-fld cgc i)
  (x86-fpu-op1 cgc i #xd9 #xc0 "fld"))

(define (x86-fpu-op1 cgc i op1 op2 mnemonic)

  (gen-8 cgc op1) ;; opcode
  (gen-8 cgc (fx+ i op2)) ;; opcode

  (if (gen-listing? cgc)
      (gen-listing
       cgc
       (x86-instr-format mnemonic
                         ""
                         (x86-fpu-reg i)))))

(define (x86-fld-mem cgc opnd width)
  (case width
    ((32) (x86-fldst-mem cgc opnd #xd9 0 "fld"))
    ((64) (x86-fldst-mem cgc opnd #xdd 0 "fld"))
    ((80) (x86-fldst-mem cgc opnd #xdb 5 "fld"))
    (else (error "width must be 32, 64 or 80" width))))

(define (x86-fst cgc i)
  (x86-fpu-op1 cgc i #xdd #xd0 "fst"))

(define (x86-fstp cgc i)
  (x86-fpu-op1 cgc i #xdd #xd8 "fstp"))

(define (x86-fst-mem cgc opnd width #!optional (pop? #f))
  (if pop?
      (case width
        ((32) (x86-fldst-mem cgc opnd #xd9 3 "fstp"))
        ((64) (x86-fldst-mem cgc opnd #xdd 3 "fstp"))
        ((80) (x86-fldst-mem cgc opnd #xdb 7 "fstp"))
        (else (error "width must be 32, 64 or 80" width)))
      (case width
        ((32) (x86-fldst-mem cgc opnd #xd9 2 "fst"))
        ((64) (x86-fldst-mem cgc opnd #xdd 2 "fst"))
        (else (error "width must be 32 or 64" width)))))

(define (x86-fstp-mem cgc opnd width)
  (x86-fst-mem cgc opnd width #t))

(define (x86-fldst-mem cgc opnd op field mnemonic)

  (x86-opnd-prefix cgc 0 0 opnd #f) ;; prefix (width is implicit)
  (gen-8 cgc op) ;; opcode
  (x86-opnd-modrm/sib cgc field opnd) ;; ModR/M

  (if (gen-listing? cgc)
      (gen-listing
       cgc
       (x86-instr-format mnemonic
                         ""
                         opnd))))

(define (x86-mov-label cgc opnd label offset) ;; make obsolete

  ;; opnd = destination

  (define (gen-32bit-label-patch cgc label offset)
    (let ((lbl (asm-make-label (rtl-code-gen-context-code-block cgc) 'fixup)))
      (rtl-code-gen-context-fixup-list-set!
       cgc
       (cons lbl
             (rtl-code-gen-context-fixup-list cgc)))
      (asm-label (rtl-code-gen-context-code-block cgc) lbl)
      (asm-at-assembly

       (rtl-code-gen-context-code-block cgc)

       (lambda (cb self)
         4)
       (lambda (cb self)
         (asm-32 cb (fx+ (asm-label-pos label) offset))))))

  (define (listing width)
    (if (gen-listing? cgc)
        (gen-listing
         cgc
         (x86-instr-format "mov"
                           (x86-width-suffix width)
                           opnd
                           (x86-imm-lbl label offset)))))

  (define (register width)
    (x86-opnd-prefix-opnd cgc width opnd) ;; prefix
    ;; opcode = #xb8-#xbf
    (gen-8 cgc (fx+ #xb8 ;; opcode
                    (fxand 7 (x86-reg-field opnd))))
    (gen-32bit-label-patch cgc label offset)
    (listing width))

  (define (general width)
    (x86-opnd-prefix-opnd cgc width opnd) ;; prefix
    (gen-8 cgc #xc7) ;; opcode
    (x86-opnd-modrm/sib cgc 0 opnd) ;; ModR/M
    (gen-32bit-label-patch cgc label offset)
    (listing width))

  (assert (or (not (x86-reg? opnd))
              (if (x86-64bit-mode? cgc)
                  (x86-r64? opnd)
                  (x86-r32? opnd)))
          "invalid width register" opnd)

  (let ((width (if (x86-64bit-mode? cgc) 64 32)))
    (if (and (x86-reg? opnd)
             (fx= width 32))
        (register width)
        (general width))))

(define (x86-lea cgc reg opnd)

  ;; reg = destination, opnd = source

  (assert (not (x86-r8? reg))
          "first operand of lea must not be an 8 bit register" reg)

  (x86-opnd-prefix-reg-opnd cgc reg opnd)
  (gen-8 cgc #x8d) ;; opcode
  (x86-opnd-modrm/sib-reg-opnd cgc reg opnd)

  (if (gen-listing? cgc)
      (gen-listing
       cgc
       (x86-instr-format "lea"
                         (x86-reg-width-suffix reg)
                         reg
                         opnd))))

(define (x86-test-imm cgc opnd k #!optional (width #f))

  ;; opnd = source

  (define (listing width n)
    (if (gen-listing? cgc)
        (gen-listing
         cgc
         (x86-instr-format "test"
                           (x86-width-suffix width)
                           opnd
                           (x86-imm-num n)))))

  (define (accumulator width)
    (x86-opnd-size-override-prefix cgc width)
    (gen-8 cgc (if (fx= width 8) #xa8 #xa9)) ;; opcode
    (listing width (x86-gen-imm-num cgc k width)))

  (define (general width)
    (x86-opnd-prefix-opnd cgc width opnd) ;; prefix
    (gen-8 cgc (if (fx= width 8) #xf6 #xf7)) ;; opcode
    (x86-opnd-modrm/sib cgc 0 opnd) ;; ModR/M
    (listing width (x86-gen-imm-num cgc k width)))

  (assert (if (x86-reg? opnd)
              (or (not width)
                  (fx= (x86-reg-width opnd) width))
              width)
          "missing or inconsistent operand width" width)

  (if (x86-reg? opnd)
      (let ((width (x86-reg-width opnd))
            (field (x86-reg-field opnd)))
        (if (fx= field 0)
            (accumulator width)
            (general width)))
      (general width)))

;; Escape opcode

(define x86-esc-opcode        #x0f)

;; Unconditional jump/call opcodes

(define x86-jmp-rel8-opcode   #xeb)
(define x86-jmp-rel32-opcode  #xe9)
(define x86-call-rel32-opcode #xe8)

;; Conditional jump opcodes (for the rel32 kind, add #x10 with #x0f opcode)

(define x86-jo-rel8-opcode   #x70)
(define x86-jno-rel8-opcode  #x71)
(define x86-jb-rel8-opcode   #x72)
;;(define x86-jnae-rel8-opcode x86-jb-rel8-opcode)
;;(define x86-jc-rel8-opcode   x86-jb-rel8-opcode)
(define x86-jae-rel8-opcode  #x73)
;;(define x86-jnb-rel8-opcode  x86-jae-rel8-opcode)
;;(define x86-jnc-rel8-opcode  x86-jae-rel8-opcode)
(define x86-je-rel8-opcode   #x74)
;;(define x86-jz-rel8-opcode   x86-je-rel8-opcode)
(define x86-jne-rel8-opcode  #x75)
;;(define x86-jnz-rel8-opcode  x86-jne-rel8-opcode)
(define x86-jbe-rel8-opcode  #x76)
;;(define x86-jna-rel8-opcode  x86-jbe-rel8-opcode)
(define x86-ja-rel8-opcode   #x77)
;;(define x86-jnbe-rel8-opcode x86-ja-rel8-opcode)
(define x86-js-rel8-opcode   #x78)
(define x86-jns-rel8-opcode  #x79)
(define x86-jp-rel8-opcode   #x7a)
;;(define x86-jpe-rel8-opcode  x86-jp-rel8-opcode)
(define x86-jnp-rel8-opcode  #x7b)
;;(define x86-jpo-rel8-opcode  x86-jnp-rel8-opcode)
(define x86-jl-rel8-opcode   #x7c)
;;(define x86-jnge-rel8-opcode x86-jl-rel8-opcode)
(define x86-jge-rel8-opcode  #x7d)
;;(define x86-jnl-rel8-opcode  x86-jge-rel8-opcode)
(define x86-jle-rel8-opcode  #x7e)
;;(define x86-jng-rel8-opcode  x86-jle-rel8-opcode)
(define x86-jg-rel8-opcode   #x7f)
;;(define x86-jnle-rel8-opcode x86-jg-rel8-opcode)

(define (x86-jmp-label cgc label offset)
  (x86-jump-label cgc x86-jmp-rel8-opcode "jmp" label offset))

(define (x86-call-label cgc label offset)
  (x86-jump-label cgc x86-call-rel32-opcode "call" label offset))

(define (x86-jo-label cgc label offset)
  (x86-jump-label cgc x86-jo-rel8-opcode "jo" label offset))

(define (x86-jno-label cgc label offset)
  (x86-jump-label cgc x86-jno-rel8-opcode "jno" label offset))

(define (x86-jb-label cgc label offset)
  (x86-jump-label cgc x86-jb-rel8-opcode "jb" label offset))

(define (x86-jae-label cgc label offset)
  (x86-jump-label cgc x86-jae-rel8-opcode "jae" label offset))

(define (x86-je-label cgc label offset)
  (x86-jump-label cgc x86-je-rel8-opcode "je" label offset))

(define (x86-jne-label cgc label offset)
  (x86-jump-label cgc x86-jne-rel8-opcode "jne" label offset))

(define (x86-jbe-label cgc label offset)
  (x86-jump-label cgc x86-jbe-rel8-opcode "jbe" label offset))

(define (x86-ja-label cgc label offset)
  (x86-jump-label cgc x86-ja-rel8-opcode "ja" label offset))

(define (x86-js-label cgc label offset)
  (x86-jump-label cgc x86-js-rel8-opcode "js" label offset))

(define (x86-jns-label cgc label offset)
  (x86-jump-label cgc x86-jns-rel8-opcode "jns" label offset))

(define (x86-jp-label cgc label offset)
  (x86-jump-label cgc x86-jp-rel8-opcode "jp" label offset))

(define (x86-jnp-label cgc label offset)
  (x86-jump-label cgc x86-jnp-rel8-opcode "jnp" label offset))

(define (x86-jl-label cgc label offset)
  (x86-jump-label cgc x86-jl-rel8-opcode "jl" label offset))

(define (x86-jge-label cgc label offset)
  (x86-jump-label cgc x86-jge-rel8-opcode "jge" label offset))

(define (x86-jle-label cgc label offset)
  (x86-jump-label cgc x86-jle-rel8-opcode "jle" label offset))

(define (x86-jg-label cgc label offset)
  (x86-jump-label cgc x86-jg-rel8-opcode "jg" label offset))

(define (x86-jump-label cgc opcode mnemonic label offset)

  (define (label-dist label offset1 self offset2)
    (fx- (fx+ (asm-label-pos label) offset1) (fx+ self offset2)))

  (define (listing short?)
    (if (gen-listing? cgc)
        (gen-listing
         cgc
         (x86-instr-format mnemonic
                           (x86-jump-label-suffix cgc short?)
                           (string-append
                            (asm-label-name label)
                            (x86-offset->string offset))))))

  (asm-at-assembly

   (rtl-code-gen-context-code-block cgc)

   ;; short displacement (-128..127 bytes)

   (if (fx= opcode x86-call-rel32-opcode)
       (lambda (cb self)
         #f)
       (lambda (cb self)
         (let ((dist (label-dist label offset self 2)))
           (if (signed8? dist)
               2
               #f))))
   (lambda (cb self)
     (let ((dist (label-dist label offset self 2)))
       (asm-8 cb opcode) ;; opcode
       (asm-8 cb (fxand 255 dist))) ;; 8 bit distance
     (listing #t))

   ;; 32 bit relative address

   (lambda (cb self)
     (cond ((or (fx= opcode x86-jmp-rel8-opcode)
                (fx= opcode x86-call-rel32-opcode))
            5)
           (else
            6)))
   (lambda (cb self)
     (let ((dist (label-dist label offset self 5)))
       (cond ((fx= opcode x86-jmp-rel8-opcode)
              (asm-8 cb x86-jmp-rel32-opcode) ;; opcode
              (asm-32 cb dist))               ;; 32 bit distance
             ((fx= opcode x86-call-rel32-opcode)
              (asm-8 cb opcode) ;; opcode
              (asm-32 cb dist)) ;; 32 bit distance
             (else
              ;; opcode is for a conditional jump
              (asm-8 cb x86-esc-opcode)    ;; escape opcode
              (asm-8 cb (fx+ #x10 opcode)) ;; opcode = #x80, #x81, etc
              (asm-32 cb (fx- dist 1)))))  ;; 32 bit distance
     (listing #f))))

(define (x86-jmp cgc opnd)
  (x86-jump-general cgc 4 opnd))

(define (x86-call cgc opnd)
  (x86-jump-general cgc 2 opnd))

(define (x86-jump-general cgc field opnd)

  (assert (or (not (x86-reg? opnd))
              (if (x86-64bit-mode? cgc)
                  (x86-r64? opnd)
                  (x86-r32? opnd)))
          "invalid width register" opnd)

  (x86-opnd-prefix cgc 0 0 opnd #f) ;; prefix (width is implicit)
  (gen-8 cgc #xff) ;; opcode = #xff
  (x86-opnd-modrm/sib cgc field opnd) ;; ModR/M

  (if (gen-listing? cgc)
      (gen-listing
       cgc
       (x86-instr-format (if (fx= field 4) "jmp" "call")
                         #f
                         opnd))))

(define (x86-push cgc opnd)
  (x86-push/pop cgc opnd #f))

(define (x86-pop cgc opnd)
  (x86-push/pop cgc opnd #t))

(define (x86-push/pop cgc opnd pop?)

  (define (listing)
    (if (gen-listing? cgc)
        (gen-listing
         cgc
         (x86-instr-format (if pop? "pop" "push")
                           (x86-32/64bit-mode-suffix cgc)
                           opnd))))

  (define (register)
    (let ((field (x86-reg-field opnd)))

      (if (x86-r32? opnd)
          (begin
            (x86-assert-32bit-mode cgc)
            (assert (fx< field 8)
                    "cannot push/pop extended registers in 32 bit mode"))
          (begin
            (x86-assert-64bit-mode cgc)
            (if (fx>= field 8)
                (gen-8 cgc #x41)))) ;; REX

      (gen-8 cgc (fx+ (if pop? #x58 #x50) ;; opcode = #x50-#x5f
                      (fxand 7 field))))

    (listing))

  (define (general)

    (x86-opnd-prefix cgc 0 0 opnd #f) ;; prefix (width is implicit)

    (let ((field
           (if pop?
               (begin
                 (gen-8 cgc #x8f) ;; opcode = #x8f
                 0)
               (begin
                 (gen-8 cgc #xff) ;; opcode = #xff
                 6))))
      (x86-opnd-modrm/sib cgc field opnd)) ;; ModR/M

    (listing))

  (cond ((and (not pop?) (x86-imm-num? opnd))
         (let ((k (x86-imm-num-value opnd)))
           (x86-push-imm cgc k)))
        ((x86-reg? opnd)
         (register))
        (else
         (general))))

(define (x86-push-imm cgc k) ;; width is always = width of stack pointer

  (define (listing n)
    (if (gen-listing? cgc)
        (gen-listing
         cgc
         (x86-instr-format "push"
                           (x86-32/64bit-mode-suffix cgc)
                           (x86-imm-num n)))))

  (if (signed8? k)
      (begin
        (gen-8 cgc #x6a) ;; opcode = #x6a
        (listing (gen-imm-num cgc k 8)))
      (begin
        (gen-8 cgc #x68) ;; opcode = #x68
        (listing (gen-imm-num cgc k 32)))))

(define (x86-ret cgc) (x86-no-opnd-instr cgc #xc3 "ret"))
(define (x86-cmc cgc) (x86-no-opnd-instr cgc #xf5 "cmc"))
(define (x86-clc cgc) (x86-no-opnd-instr cgc #xf8 "clc"))
(define (x86-stc cgc) (x86-no-opnd-instr cgc #xf9 "stc"))
;;(define (x86-cli cgc) (x86-no-opnd-instr cgc #xfa "cli"))
;;(define (x86-sti cgc) (x86-no-opnd-instr cgc #xfb "sti"))
;;(define (x86-cld cgc) (x86-no-opnd-instr cgc #xfc "cld"))
;;(define (x86-std cgc) (x86-no-opnd-instr cgc #xfd "std"))

(define (x86-no-opnd-instr cgc opcode mnemonic)

  (gen-8 cgc opcode) ;; opcode

  (if (gen-listing? cgc)
      (gen-listing
       cgc
       (x86-instr-format mnemonic ""))))

#; ;;TODO
(define (x86-xchg cgc op mnemonic opnd1 opnd2)

  ;; opnd1 = destination, opnd2 = source

  (define (gen-op reg opnd swapped?)
    (let ((field (x86-reg-field reg))
          (width (x86-reg-width reg)))
      (if (x86-reg? opnd)
          (let ((field2 (x86-reg-field opnd)))
            (assert (fx= width (x86-reg-width opnd)))
            (cond ((and (not (fx= width 8)) (fx= field 0)) ;; AX/EAX/RAX
                   (accumulator width opnd swapped?))
                  ((and (not (fx= width 8)) (fx= field2 0)) ;; AX/EAX/RAX
                   (accumulator width reg (not swapped?)))
                  (else
                   (general width reg opnd swapped?))))
          (general width reg opnd swapped?)))

    (x86-opnd-prefix-reg-opnd cgc reg opnd)
    (gen-8 cgc (fx+ (fxarithmetic-shift-left op 3) ;; opcode
                    (if swapped? 0 2)
                    (if (x86-r8? reg) 0 1)))
    (x86-opnd-modrm/sib-reg-opnd cgc reg opnd)

    (if (gen-listing? cgc)
        (gen-listing
         cgc
         (x86-instr-format mnemonic
                           (x86-reg-width-suffix reg)
                           (if swapped? opnd reg)
                           (if swapped? reg opnd)))))

  (cond ((x86-reg? opnd2)
         (gen-op opnd2 opnd1 #t))
        ((x86-reg? opnd1)
         (gen-op opnd1 opnd2 #f))
        (else
         (error "one of the two operands must be a register" opnd1 opnd2))))

(define (jump-ro cb reg offset)

  (if (x86-r16? reg)
      (asm-8 cb #x66)) ;; operand size override prefix

  (if (>= (x86-reg-field reg) 8)
      (asm-8 cb #x41)) ;; REX.B  (extend reg field)

  (asm-8 cb #xff)

  (cond ((eqv? offset 0)
         (asm-8 cb (+ #xe0 (bitwise-and reg 7))))
        ((signed8? offset)
         (asm-8 cb (+ #x60 (bitwise-and reg 7)))
         (asm-8 cb offset))
        (else
         (asm-8 cb (+ #xa0 (bitwise-and reg 7)))
         (asm-32 cb offset))))

(define-macro (define-registers . definitions)

  (define reg-name-table (make-vector (+ 96 8) "invalidreg"))

  (define (get d attrib)
    (let ((x (member attrib (cdr d))))
      (if x (cadr x) #f)))

  (define (gen-def d)
    (let ((name (car d)))
      (let ((class (get d 'class:))
            (field (get d 'field:))
            (mode  (get d 'mode:)))
        (if (member class '(r8 r16 r32 r64 fpu mm xmm))
            (let ((i (+ field
                        (if (and (eq? class 'r8)
                                 (>= field 4)
                                 (< field 8)
                                 (not (eq? mode 'long)))
                            16
                            0)
                        (case class
                          ((r64) 0)
                          ((r32) 16)
                          ((r16) 32)
                          ((fpu) 48)
                          ((mm)  56)
                          ((xmm) 64)
                          ((r8)  80)))))
              (vector-set! reg-name-table i name)
              `((define-macro (,(string->symbol (string-append "x86-" (symbol->string name))))
                  ,i)))
            `()))))

  (let* ((defs
           (apply append (map gen-def definitions)))
         (code
          `(begin
             (define-macro (x86-reg? x)
               `(fixnum? ,x))
             (define-macro (x86-r8? reg)
               `(let ((n ,reg)) (fx>= n 80)))
             (define-macro (x86-r8-h? reg)
               `(let ((n ,reg)) (fx>= n 96)))
             (define-macro (x86-xmm? reg)
               `(let ((n ,reg)) (and (fx>= n 64) (fx< n 80))))
             (define-macro (x86-mm? reg)
               `(let ((n ,reg)) (and (fx>= n 56) (fx< n 64))))
             (define-macro (x86-fpu? reg)
               `(let ((n ,reg)) (and (fx>= n 48) (fx< n 56))))
             (define-macro (x86-r16? reg)
               `(let ((n ,reg)) (and (fx>= n 32) (fx< n 48))))
             (define-macro (x86-r32? reg)
               `(let ((n ,reg)) (and (fx>= n 16) (fx< n 32))))
             (define-macro (x86-r64? reg)
               `(let ((n ,reg)) (fx< n 16)))
             (define-macro (x86-reg-field reg)
               `(fxand ,reg 15))
             (define-macro (x86-r8 n)
               `(fx+ 80 ,n))
             (define-macro (x86-r16 n)
               `(fx+ 32 ,n))
             (define-macro (x86-r32 n)
               `(fx+ 16 ,n))
             (define-macro (x86-r64 n)
               n)
             (define-macro (x86-fpu n)
               `(fx+ 48 ,n))
             (define-macro (x86-reg-width reg)
               `(let ((n ,reg))
                  (cond ((fx< n 16) 64)
                        ((fx< n 32) 32)
                        ((fx< n 48) 16)
                        ((fx< n 64) 80)
                        ((fx< n 80) 128)
                        (else       8))))
             (define-macro (x86-reg-name reg)
               `(vector-ref ',',reg-name-table ,reg))
             ,@defs)))
;    (pp code)
;    (pp reg-name-table)
    code))
    
(define-registers

  (al      class: r8  field: 0 )
  (cl      class: r8  field: 1 )
  (dl      class: r8  field: 2 )
  (bl      class: r8  field: 3 )
  (ah      class: r8  field: 4 )
  (ch      class: r8  field: 5 )
  (dh      class: r8  field: 6 )
  (bh      class: r8  field: 7 )
  (spl     class: r8  field: 4  mode: long)
  (bpl     class: r8  field: 5  mode: long)
  (sil     class: r8  field: 6  mode: long)
  (dil     class: r8  field: 7  mode: long)
  (r8b     class: r8  field: 8  mode: long)
  (r9b     class: r8  field: 9  mode: long)
  (r10b    class: r8  field: 10 mode: long)
  (r11b    class: r8  field: 11 mode: long)
  (r12b    class: r8  field: 12 mode: long)
  (r13b    class: r8  field: 13 mode: long)
  (r14b    class: r8  field: 14 mode: long)
  (r15b    class: r8  field: 15 mode: long)

  (ax      class: r16 field: 0 )
  (cx      class: r16 field: 1 )
  (dx      class: r16 field: 2 )
  (bx      class: r16 field: 3 )
  (sp      class: r16 field: 4 )
  (bp      class: r16 field: 5 )
  (si      class: r16 field: 6 )
  (di      class: r16 field: 7 )
  (r8w     class: r16 field: 8  mode: long)
  (r9w     class: r16 field: 9  mode: long)
  (r10w    class: r16 field: 10 mode: long)
  (r11w    class: r16 field: 11 mode: long)
  (r12w    class: r16 field: 12 mode: long)
  (r13w    class: r16 field: 13 mode: long)
  (r14w    class: r16 field: 14 mode: long)
  (r15w    class: r16 field: 15 mode: long)

  (eax     class: r32 field: 0 )
  (ecx     class: r32 field: 1 )
  (edx     class: r32 field: 2 )
  (ebx     class: r32 field: 3 )
  (esp     class: r32 field: 4 )
  (ebp     class: r32 field: 5 )
  (esi     class: r32 field: 6 )
  (edi     class: r32 field: 7 )
  (r8d     class: r32 field: 8  mode: long)
  (r9d     class: r32 field: 9  mode: long)
  (r10d    class: r32 field: 10 mode: long)
  (r11d    class: r32 field: 11 mode: long)
  (r12d    class: r32 field: 12 mode: long)
  (r13d    class: r32 field: 13 mode: long)
  (r14d    class: r32 field: 14 mode: long)
  (r15d    class: r32 field: 15 mode: long)

  (rax     class: r64 field: 0 )
  (rcx     class: r64 field: 1 )
  (rdx     class: r64 field: 2 )
  (rbx     class: r64 field: 3 )
  (rsp     class: r64 field: 4 )
  (rbp     class: r64 field: 5 )
  (rsi     class: r64 field: 6 )
  (rdi     class: r64 field: 7 )
  (r8      class: r64 field: 8  mode: long)
  (r9      class: r64 field: 9  mode: long)
  (r10     class: r64 field: 10 mode: long)
  (r11     class: r64 field: 11 mode: long)
  (r12     class: r64 field: 12 mode: long)
  (r13     class: r64 field: 13 mode: long)
  (r14     class: r64 field: 14 mode: long)
  (r15     class: r64 field: 15 mode: long)

  (st      class: fpu field: 0 )
  (|st(1)| class: fpu field: 1 )
  (|st(2)| class: fpu field: 2 )
  (|st(3)| class: fpu field: 3 )
  (|st(4)| class: fpu field: 4 )
  (|st(5)| class: fpu field: 5 )
  (|st(6)| class: fpu field: 6 )
  (|st(7)| class: fpu field: 7 )

  (mm0     class: mm  field: 0 )
  (mm1     class: mm  field: 1 )
  (mm2     class: mm  field: 2 )
  (mm3     class: mm  field: 3 )
  (mm4     class: mm  field: 4 )
  (mm5     class: mm  field: 5 )
  (mm6     class: mm  field: 6 )
  (mm7     class: mm  field: 7 )

  (xmm0    class: xmm field: 0 )
  (xmm1    class: xmm field: 1 )
  (xmm2    class: xmm field: 2 )
  (xmm3    class: xmm field: 3 )
  (xmm4    class: xmm field: 4 )
  (xmm5    class: xmm field: 5 )
  (xmm6    class: xmm field: 6 )
  (xmm7    class: xmm field: 7 )
  (xmm8    class: xmm field: 8  mode: long)
  (xmm9    class: xmm field: 9  mode: long)
  (xmm10   class: xmm field: 10 mode: long)
  (xmm11   class: xmm field: 11 mode: long)
  (xmm12   class: xmm field: 12 mode: long)
  (xmm13   class: xmm field: 13 mode: long)
  (xmm14   class: xmm field: 14 mode: long)
  (xmm15   class: xmm field: 15 mode: long)

  (es      class: seg field: 0 )
  (cs      class: seg field: 1 )
  (ss      class: seg field: 2 )
  (ds      class: seg field: 3 )
  (fs      class: seg field: 4 )
  (gs      class: seg field: 5 )

)
