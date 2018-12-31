(define fxshift 2)
(define fxmask #x03)
(define fxtag #x00)
(define bool_f #x2f)
(define bool_t #x6f)
(define bool_bit 6)
(define bool_mask #xbf)
(define bool_tag  #x2f)
(define wordsize 4) ; bytes

(define nullval #b00111111)
(define charshift 8)
(define chartag #b00001111)
(define charmask #xff)

(define fixnum-bits (- (* wordsize 8) fxshift))

(define fxlower (- (expt 2 (- fixnum-bits 1))))

(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))

(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))

(define (immediate-rep x)
  (cond
   ((fixnum? x) (ash x fxshift))
   ((eq? x #t) bool_t)
   ((eq? x #f) bool_f)
   ((char? x) (+ (ash (char->integer x) charshift) chartag))
   ((null? x) nullval)
   (else (error "must not happen"))))

(define (emit-program expr)
  (emit-function-header "scheme_entry")
  (emit-expr expr)
  (emit "    ret"))

(define (emit-function-header funcname)
  (emit "    .text")
  (emit (string-append "    .global " funcname))
  (emit (string-append funcname ":")))

(define (emit-expr expr)
  (cond
   ((immediate? expr) (emit-immediate expr))
   ((primcall? expr) (emit-primcall expr))
   (else (error "not implemented"))))

(define (emit-immediate x)
  (emit "    movl $~s, %eax" (immediate-rep x)))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error "must not happen")))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (emit-primcall expr)
  (let ((prim (car expr)) (args (cdr expr)))
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) args)))

(define (check-primcall-args prim args)
  (let ((n (getprop prim '*arg-count*))
        (m (length args)))
    (if (= m n)
        #t
      (error "illegal argnum:" m 'for n))))

(define-syntax define-primitive
  (syntax-rules ()
    ((_ (prim-name arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
                (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                (lambda (arg* ...) b b* ...))))))

(define-primitive ($fxadd1 arg)
  (emit-expr arg)
  (emit "    addl $~s, %eax" (immediate-rep 1)))

(define-primitive ($fxsub1 arg)
  (emit-expr arg)
  (emit "    subl $~s, %eax" (immediate-rep 1)))

(define-primitive ($fixnum->char arg)
  (emit-expr arg)
  (emit "    shll $~s, %eax" (- charshift fxshift))
  (emit "    orl $~s, %eax" chartag))

(define-primitive ($char->fixnum arg)
  (emit-expr arg)
  (emit "    shrl $~s, %eax" (- charshift fxshift)))

(define (emit-to-boolean)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" bool_bit)
  (emit "    or $~s, %al" bool_f))

(define-primitive (fixnum? arg)
  (emit-expr arg)
  (emit "    and $~s, %al" fxmask)
  (emit "    cmp $~s, %al" fxtag)
  (emit-to-boolean))

(define-primitive ($fxzero? arg)
  (emit-expr arg)
  (emit "    testl %eax, %eax")
  (emit-to-boolean))

(define-primitive (null? arg)
  (emit-expr arg)
  (emit "    cmp $~s, %al" nullval)
  (emit-to-boolean))

(define-primitive (boolean? arg)
  (emit-expr arg)
  (emit "    and $~s, %al" bool_mask)
  (emit "    cmp $~s, %al" bool_tag)
  (emit-to-boolean))

(define-primitive (char? arg)
  (emit-expr arg)
  (emit "    and $~s, %al" charmask)
  (emit "    cmp $~s, %al" chartag)
  (emit-to-boolean))

(define-primitive (not arg)
  (emit-expr arg)
  (emit "    cmp $~s, %al" bool_f)
  (emit-to-boolean))

(define-primitive ($fxlognot arg)
  (emit-expr arg)
  (emit "    notl %eax")
  (emit "    and $~s, %eax" (lognot fxmask)))
