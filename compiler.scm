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
   ((if? expr)        (emit-if expr))
   ((and? expr)       (emit-and expr))
   ((or? expr)        (emit-or expr))
   ((primcall? expr)  (emit-primcall expr))
   ((predicate-call? expr) (emit-predicate-val expr))
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

(define (emit-predicate-val expr)
  (emit-predicate-test expr)
  (emit-to-boolean))

(define (emit-predicate-test expr)
  (let ((prim (car expr)) (args (cdr expr)))
    (check-primcall-args prim args)
    (apply (predicate-emitter prim) args)))

(define (emit-label label)
  (emit "~a:" label))

(define (emit-test expr)
  (if (predicate-call? expr)
      (begin
        (emit-predicate-test expr)
        #t)
    (begin
      (emit-expr expr)
      (emit "    cmp $~s, %al" bool_f)
      #f)))

(define (emit-if expr)
  (let ((alt-label (unique-label))
        (end-label (unique-label)))
    (if (emit-test (if-test expr))
        (emit "    jne ~a" alt-label)
      (emit "    je ~a" alt-label))
    (emit-expr (if-conseq expr))
    (emit "    jmp ~a" end-label)
    (emit-label alt-label)
    (emit-expr (if-altern expr))
    (emit-label end-label)))

(define (emit-and expr)
  (define (test-false expr false-label end-label)
    (if (predicate-call? expr)
        (begin
          (emit-predicate-test expr)
          (emit "    jne ~a" false-label))  ; Jump to set #f if condition is failed.
      (begin
        (emit-test expr)
        (emit "    je ~a" end-label))))  ; If expr is value (not predicate), the value is #f when failed, so jump to the end directly.
  (let ((p (cdr expr)))
    (cond ((null? p)
           (emit "    mov $~s, %eax" bool_t))
          (else
           (let ((false-label (unique-label))
                 (end-label (unique-label)))
             (let loop ((p p))
               (if (null? (cdr p)) ; Last
                   (emit-expr (car p))
                 (begin
                   (test-false (car p) false-label end-label)
                   (loop (cdr p)))))
             (emit "    jmp ~a" end-label)
             (emit-label false-label)
             (emit "    mov $~s, %eax" bool_f)
             (emit-label end-label))))))

(define (emit-or expr)
  (define (test-true expr true-label end-label)
    (if (predicate-call? expr)
        (begin
          (emit-predicate-test expr)
          (emit "    je ~a" true-label))  ; Jump to set #t if condition is succeeded.
      (begin
        (emit-test expr)
        (emit "    jne ~a" end-label))))  ; If expr is value (not predicate), the value is non-falsy, so jump to the end directly.
  (let ((p (cdr expr)))
    (cond ((null? p)
           (emit "    mov $~s, %eax" bool_f))
          (else
           (let ((true-label (unique-label))
                 (end-label (unique-label)))
             (let loop ((p p))
               (if (null? (cdr p))
                   (emit-expr (car p))
                 (begin
                   (test-true (car p) true-label end-label)
                   (loop (cdr p)))))
             (emit "    jmp ~a" end-label)
             (emit-label true-label)
             (emit "    mov $~s, %eax" bool_t)
             (emit-label end-label))))))

(define (predicate? x)
  (and (symbol? x) (getprop x '*is-predicate*)))

(define (predicate-call? expr)
  (and (pair? expr) (predicate? (car expr))))

(define (predicate-emitter x)
  (or (getprop x '*emitter*) (error "must not happen")))

(define unique-label
  (let ((count 0))
    (lambda ()
      (let ((L (format "L_~s" count)))
        (set! count (add1 count))
        L))))

(define (if? expr)
  (and (pair? expr) (eq? (car expr) 'if)))

(define (if-test expr) (cadr expr))
(define (if-conseq expr) (caddr expr))
(define (if-altern expr) (cadddr expr))

(define (and? expr)
  (and (pair? expr) (eq? (car expr) 'and)))

(define (or? expr)
  (and (pair? expr) (eq? (car expr) 'or)))


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

(define-syntax define-predicate
  (syntax-rules ()
    ((_ (prim-name arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-predicate* #t)
       (putprop 'prim-name '*arg-count*
                (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                (lambda (arg* ...) b b* ...))))))

(define-predicate (fixnum? arg)
  (emit-expr arg)
  (emit "    and $~s, %al" fxmask)
  (emit "    cmp $~s, %al" fxtag))

(define-predicate ($fxzero? arg)
  (emit-expr arg)
  (emit "    testl %eax, %eax"))

(define-predicate (null? arg)
  (emit-expr arg)
  (emit "    cmp $~s, %al" nullval))

(define-predicate (boolean? arg)
  (emit-expr arg)
  (emit "    and $~s, %al" bool_mask)
  (emit "    cmp $~s, %al" bool_tag))

(define-predicate (char? arg)
  (emit-expr arg)
  (emit "    and $~s, %al" charmask)
  (emit "    cmp $~s, %al" chartag))

(define-predicate (not arg)
  (emit-expr arg)
  (emit "    cmp $~s, %al" bool_f))

(define-primitive ($fxlognot arg)
  (emit-expr arg)
  (emit "    notl %eax")
  (emit "    and $~s, %eax" (lognot fxmask)))
