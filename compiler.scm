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


(define (emit-program expr)
  (emit-function-header "scheme_entry")
  (emit "    movl %esp, %ecx")
  (emit "    movl 4(%esp), %esp")
  (emit "    call L_scheme_entry")
  (emit "    movl %ecx, %esp")
  (emit "    ret")

  (emit-label "L_scheme_entry")
  (emit-expr (- wordsize) expr)
  (emit "    ret"))

(define (emit-function-header funcname)
  (emit "    .text")
  (emit (string-append "    .global " funcname))
  (emit (string-append funcname ":")))

(define (emit-expr si expr)
  (cond
   ((immediate? expr) (emit-immediate expr))
   ((if? expr)        (emit-if si expr))
   ((and? expr)       (emit-and si expr))
   ((or? expr)        (emit-or si expr))
   ((primcall? expr)  (emit-primcall si expr))
   ((predicate-call? expr) (emit-predicate-val si expr))
   (else (error "not implemented"))))

(define (emit-immediate x)
  (emit "    movl $~s, %eax" (immediate-rep x)))

(define (emit-primcall si expr)
  (let ((prim (car expr)) (args (cdr expr)))
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si args)))

(define (emit-predicate-val si expr)
  (let ((c (emit-predicate-test si expr))
        (prim (car expr))
        (args (cdr expr)))
    (emit-to-boolean c)))

(define (emit-predicate-test si expr)
  (let ((prim (car expr)) (args (cdr expr)))
    (check-primcall-args prim args)
    (apply (predicate-emitter prim) si args)))

(define (emit-label label)
  (emit "~a:" label))

(define (emit-test si expr)
  (if (predicate-call? expr)
      (emit-predicate-test si expr)
    (begin
      (emit-expr si expr)
      (emit "    cmp $~s, %al" bool_f)
      'NEQ)))

(define (emit-jump-if-not pred label)
  (let ((c (case pred
             ((EQ)  "jne")
             ((NEQ) "je")
             ((LT)  "jge")
             ((GT)  "jle")
             ((LE)  "jg")
             ((GE)  "Jl")
             (else (error "illegal condition")))))
    (emit "    ~a ~a" c label)))

(define (emit-if si expr)
  (let ((alt-label (unique-label))
        (end-label (unique-label)))
    (emit-jump-if-not (emit-test si (if-test expr))
                      alt-label)
    (emit-expr si (if-conseq expr))
    (emit "    jmp ~a" end-label)
    (emit-label alt-label)
    (emit-expr si (if-altern expr))
    (emit-label end-label)))

(define (emit-and si expr)
  (define (test-false expr false-label end-label)
    (if (predicate-call? expr)
        (begin
          (emit-predicate-test si expr)
          (emit "    jne ~a" false-label))  ; Jump to set #f if condition is failed.
      (begin
        (emit-test si expr)
        (emit "    je ~a" end-label))))  ; If expr is value (not predicate), the value is #f when failed, so jump to the end directly.
  (let ((p (cdr expr)))
    (cond ((null? p)
           (emit "    mov $~s, %eax" bool_t))
          (else
           (let ((false-label (unique-label))
                 (end-label (unique-label)))
             (let loop ((p p))
               (if (null? (cdr p)) ; Last
                   (emit-expr si (car p))
                 (begin
                   (test-false (car p) false-label end-label)
                   (loop (cdr p)))))
             (emit "    jmp ~a" end-label)
             (emit-label false-label)
             (emit "    mov $~s, %eax" bool_f)
             (emit-label end-label))))))

(define (emit-or si expr)
  (define (test-true expr true-label end-label)
    (if (predicate-call? expr)
        (begin
          (emit-predicate-test si expr)
          (emit "    je ~a" true-label))  ; Jump to set #t if condition is succeeded.
      (begin
        (emit-test si expr)
        (emit "    jne ~a" end-label))))  ; If expr is value (not predicate), the value is non-falsy, so jump to the end directly.
  (let ((p (cdr expr)))
    (cond ((null? p)
           (emit "    mov $~s, %eax" bool_f))
          (else
           (let ((true-label (unique-label))
                 (end-label (unique-label)))
             (let loop ((p p))
               (if (null? (cdr p))
                   (emit-expr si (car p))
                 (begin
                   (test-true (car p) true-label end-label)
                   (loop (cdr p)))))
             (emit "    jmp ~a" end-label)
             (emit-label true-label)
             (emit "    mov $~s, %eax" bool_t)
             (emit-label end-label))))))

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

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error "must not happen")))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (check-primcall-args prim args)
  (let ((n (getprop prim '*arg-count*))
        (m (length args)))
    (if (= m n)
        #t
      (error "illegal argnum:" m 'for n))))

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
    ((_ (prim-name si arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
                (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                (lambda (si arg* ...) b b* ...))))))

(define-primitive ($fxadd1 si arg)
  (emit-expr si arg)
  (emit "    addl $~s, %eax" (immediate-rep 1)))

(define-primitive ($fxsub1 si arg)
  (emit-expr si arg)
  (emit "    subl $~s, %eax" (immediate-rep 1)))

(define-primitive ($fixnum->char si arg)
  (emit-expr si arg)
  (emit "    shll $~s, %eax" (- charshift fxshift))
  (emit "    orl $~s, %eax" chartag))

(define-primitive ($char->fixnum si arg)
  (emit-expr si arg)
  (emit "    shrl $~s, %eax" (- charshift fxshift)))

(define (emit-to-boolean c)
  (let ((op (case c
              ((EQ)  "sete")
              ((NEQ) "setne")
              ((LT)  "setl")
              ((GT)  "setg")
              ((LE)  "setle")
              ((GE)  "setge")
              (else (error "illegal condition")))))
    (emit "    ~a %al" op)
    (emit "    movzbl %al, %eax")
    (emit "    sal $~s, %al" bool_bit)
    (emit "    or $~s, %al" bool_f)))

(define-syntax define-predicate
  (syntax-rules ()
    ((_ (prim-name si arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-predicate* #t)
       (putprop 'prim-name '*arg-count*
                (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                (lambda (si arg* ...) b b* ...))))))

(define-predicate (fixnum? si arg)
  (emit-expr si arg)
  (emit "    and $~s, %al" fxmask)
  (emit "    cmp $~s, %al" fxtag)
  'EQ)

(define-predicate ($fxzero? si arg)
  (emit-expr si arg)
  (emit "    testl %eax, %eax")
  'EQ)

(define-predicate (null? si arg)
  (emit-expr si arg)
  (emit "    cmp $~s, %al" nullval)
  'EQ)

(define-predicate (boolean? si arg)
  (emit-expr si arg)
  (emit "    and $~s, %al" bool_mask)
  (emit "    cmp $~s, %al" bool_tag)
  'EQ)

(define-predicate (char? si arg)
  (emit-expr si arg)
  (emit "    and $~s, %al" charmask)
  (emit "    cmp $~s, %al" chartag)
  'EQ)

(define-predicate (not si arg)
  (emit-expr si arg)
  (emit "    cmp $~s, %al" bool_f)
  'EQ)

(define-primitive (fxlognot si arg)
  (emit-expr si arg)
  (emit "    notl %eax")
  (emit "    and $~s, %eax" (lognot fxmask)))

(define-primitive (fx+ si arg1 arg2)
  (define (out2)
    (emit-expr si arg1)
    (emit "    movl %eax, ~s(%esp)" si)
    (emit-expr (- si wordsize) arg2)
    (emit "    addl ~s(%esp), %eax" si))
  (define (out1 expr const)
    (emit-expr si expr)
    (emit "    addl $~s, %eax" (immediate-rep const)))
  ;; If two values are constant, they are convolved in much higher-place, so not processed here.
  (cond ((fixnum? arg2) (out1 arg1 arg2))
        ((fixnum? arg1) (out1 arg2 arg1))
        (else (out2))))

(define-primitive (fx- si arg1 arg2)
  (define (out2)
    (emit-expr si arg2)
    (emit "    movl %eax, ~s(%esp)" si)
    (emit-expr (- si wordsize) arg1)
    (emit "    subl ~s(%esp), %eax" si))
  (define (out1 expr const)
    (emit-expr si expr)
    (emit "    subl $~s, %eax" (immediate-rep const)))
  (cond ((fixnum? arg2) (out1 arg1 arg2))
        (else (out2))))

(define-primitive (fx* si arg1 arg2)
  (define (out2)
    (emit-expr si arg1)
    (emit "    sarl $2, %eax")  ; Shift to the right.
    (emit "    movl %eax, ~s(%esp)" si)
    (emit-expr (- si wordsize) arg2)
    (emit "    imull ~s(%esp), %eax" si))
  (define (out1 expr const)
    (emit-expr si expr)
    (emit "    imull $~s, %eax" const))  ; No shift needed.
  (cond ((fixnum? arg2) (out1 arg1 arg2))
        ((fixnum? arg1) (out1 arg2 arg1))
        (else (out2))))

(define-primitive (fxlogor si arg1 arg2)
  (emit-expr si arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg2)
  (emit "    orl ~s(%esp), %eax" si))

(define-primitive (fxlogand si arg1 arg2)
  (emit-expr si arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg2)
  (emit "    andl ~s(%esp), %eax" si))

(define-predicate (fx= si arg1 arg2)
  (emit-expr si arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg2)
  (emit "    cmpl ~s(%esp), %eax" si)
  'EQ)

(define-predicate (fx< si arg1 arg2)
  (emit-expr si arg2)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg1)
  (emit "    cmpl ~s(%esp), %eax" si)
  'LT)

(define-predicate (fx<= si arg1 arg2)
  (emit-expr si arg2)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg1)
  (emit "    cmpl ~s(%esp), %eax" si)
  'LE)

(define-predicate (fx> si arg1 arg2)
  (emit-expr si arg2)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg1)
  (emit "    cmpl ~s(%esp), %eax" si)
  'GT)

(define-predicate (fx>= si arg1 arg2)
  (emit-expr si arg2)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg1)
  (emit "    cmpl ~s(%esp), %eax" si)
  'GE)
