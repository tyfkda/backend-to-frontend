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

  (cond ((letrec? expr) (emit-letrec expr))
        (else
         (emit-label "L_scheme_entry")
         (emit-expr (- wordsize) '() expr)
         (emit "    ret"))))

(define (emit-function-header funcname)
  (emit "    .text")
  (emit (string-append "    .global " funcname))
  (emit (string-append funcname ":")))

(define (emit-expr si env expr)
  (cond
   ((immediate? expr) (emit-immediate expr))
   ((variable? expr)  (emit-variable-ref env expr))
   ((if? expr)        (emit-if si env expr))
   ((and? expr)       (emit-and si env expr))
   ((or? expr)        (emit-or si env expr))
   ((let? expr)       (emit-let si env expr))
   ((primcall? expr)  (emit-primcall si env expr))
   ((predicate-call? expr) (emit-predicate-val si env expr))
   ((app? expr)       (emit-app si env expr))
   (else (error "not implemented"))))

(define (emit-immediate x)
  (emit "    movl $~s, %eax" (immediate-rep x)))

(define (emit-primcall si env expr)
  (let ((prim (car expr))
		(args (cdr expr)))
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si env args)))

(define (emit-predicate-val si env expr)
  (let ((c (emit-predicate-test si env expr))
        (prim (car expr))
        (args (cdr expr)))
    (emit-to-boolean c)))

(define (emit-predicate-test si env expr)
  (let ((prim (car expr))
        (args (cdr expr)))
    (check-primcall-args prim args)
    (apply (predicate-emitter prim) si env args)))

(define (emit-label label)
  (emit "~a:" label))

(define (emit-test si env expr)
  (if (predicate-call? expr)
      (emit-predicate-test si env expr)
    (begin
      (emit-expr si env expr)
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

(define (emit-if si env expr)
  (let ((alt-label (unique-label))
        (end-label (unique-label)))
    (emit-jump-if-not (emit-test si env (if-test expr))
                      alt-label)
    (emit-expr si env (if-conseq expr))
    (emit "    jmp ~a" end-label)
    (emit-label alt-label)
    (emit-expr si env (if-altern expr))
    (emit-label end-label)))

(define (emit-and si env expr)
  (define (test-false expr false-label end-label)
    (if (predicate-call? expr)
        (begin
          (emit-predicate-test si env expr)
          (emit "    jne ~a" false-label))  ; Jump to set #f if condition is failed.
      (begin
        (emit-test si env expr)
        (emit "    je ~a" end-label))))  ; If expr is value (not predicate), the value is #f when failed, so jump to the end directly.
  (let ((p (cdr expr)))
    (cond ((null? p)
           (emit "    mov $~s, %eax" bool_t))
          (else
           (let ((false-label (unique-label))
                 (end-label (unique-label)))
             (let loop ((p p))
               (if (null? (cdr p)) ; Last
                   (emit-expr si env (car p))
                 (begin
                   (test-false (car p) false-label end-label)
                   (loop (cdr p)))))
             (emit "    jmp ~a" end-label)
             (emit-label false-label)
             (emit "    mov $~s, %eax" bool_f)
             (emit-label end-label))))))

(define (emit-or si env expr)
  (define (test-true expr true-label end-label)
    (if (predicate-call? expr)
        (begin
          (emit-predicate-test si env expr)
          (emit "    je ~a" true-label))  ; Jump to set #t if condition is succeeded.
      (begin
        (emit-test si env expr)
        (emit "    jne ~a" end-label))))  ; If expr is value (not predicate), the value is non-falsy, so jump to the end directly.
  (let ((p (cdr expr)))
    (cond ((null? p)
           (emit "    mov $~s, %eax" bool_f))
          (else
           (let ((true-label (unique-label))
                 (end-label (unique-label)))
             (let loop ((p p))
               (if (null? (cdr p))
                   (emit-expr si env (car p))
                 (begin
                   (test-true (car p) true-label end-label)
                   (loop (cdr p)))))
             (emit "    jmp ~a" end-label)
             (emit-label true-label)
             (emit "    mov $~s, %eax" bool_t)
             (emit-label end-label))))))

(define (emit-let si env expr)
  (define (process-let bindings si new-env)
    (cond
     ((empty? bindings)
      (emit-expr si new-env (let-body expr)))
     (else
      (let ((b (first bindings)))
        (emit-expr si env (rhs b))
        (emit-stack-save si)
        (process-let (rest bindings)
                     (next-stack-index si)
                     (extend-env (lhs b) si new-env))))))
  (process-let (let-bindings expr) si env))

(define (emit-stack-save si)
  (emit "    movl %eax, ~s(%esp)" si))

(define (emit-stack-load si)
  (emit "    movl ~s(%esp), %eax" si))

(define (emit-variable-ref env var)
  (cond
   ((lookup var env) => emit-stack-load)
   (else (error "unbound variable: " var))))

(define (let? expr)
  (and (pair? expr) (eq? (car expr) 'let)))

(define (let-bindings expr)
  (cadr expr))

(define (let-body expr)
  (caddr expr))

(define (emit-letrec expr)
  (let* ((bindings (letrec-bindings expr))
         (lvars (map lhs bindings))
         (lambdas (map rhs bindings))
         (labels (unique-labels lvars))
;         (labels (map symbol->string lvars))
         (env (make-initial-env lvars labels)))
    (for-each (emit-lambda env) lambdas labels)
    (emit-label "L_scheme_entry")
    (emit-scheme-entry (letrec-body expr) env)
    (emit "    ret")))

(define (emit-scheme-entry body env)
  (emit-expr (- wordsize) env body))

(define (emit-lambda env)
  (lambda (expr label)
    (emit-function-header label)
    (let ((fmls (lambda-formals expr))
          (body (lambda-body expr)))
      (let f ((fmls fmls)
              (si (- wordsize))
              (env env))
        (cond
         ((empty? fmls)
          (emit-expr si env body)
          (emit "    ret"))
         (else
          (f (rest fmls)
             (- si wordsize)
             (extend-env (first fmls) si env))))))))

(define (emit-app si env expr)
  (define (emit-arguments si args)
    (unless (empty? args)
      (emit-expr si env (first args))
      (emit "    movl %eax, ~s(%esp)" si)
      (emit-arguments (- si wordsize) (rest args))))
  (emit-arguments (- si wordsize) (call-args expr))
  (emit-adjust-base (+ si wordsize))
  (emit-call si (lookup (call-target expr) env))
  (emit-adjust-base (- (+ si wordsize))))

(define (emit-adjust-base n)
  (cond ((< n 0) (emit "    subl $~a, %esp" (- n)))
        ((> n 0) (emit "    addl $~a, %esp" n))))

(define (emit-call si label)
  (emit "    call ~a" label))

(define empty? null?)
(define first car)
(define rest cdr)
(define lhs car)
(define rhs cadr)
(define (next-stack-index si) (- si wordsize))
(define (variable? x)
  (symbol? x))

(define (extend-env varname si env)
  (cons (cons varname si) env))

(define (lookup var env)
  (let ((a (assoc var env)))
    (if a
        (cdr a)
      #f)))

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

(define (unique-labels expr)
  (map (lambda (_) (unique-label)) expr))

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
    ((_ (prim-name si env arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
                (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                (lambda (si env arg* ...) b b* ...))))))

(define-primitive ($fxadd1 si env arg)
  (emit-expr si env arg)
  (emit "    addl $~s, %eax" (immediate-rep 1)))

(define-primitive ($fxsub1 si env arg)
  (emit-expr si env arg)
  (emit "    subl $~s, %eax" (immediate-rep 1)))

(define-primitive ($fixnum->char si env arg)
  (emit-expr si env arg)
  (emit "    shll $~s, %eax" (- charshift fxshift))
  (emit "    orl $~s, %eax" chartag))

(define-primitive ($char->fixnum si env arg)
  (emit-expr si env arg)
  (emit "    shrl $~s, %eax" (- charshift fxshift)))

(define (letrec? expr)
  (and (pair? expr) (eq? (car expr) 'letrec)))

(define (letrec-bindings expr)
  (cadr expr))

(define (letrec-body expr)
  (caddr expr))

(define (make-initial-env lvars labels)
  (map cons lvars labels))

(define (lambda-formals expr)
  (cadr expr))

(define (lambda-body expr)
  (caddr expr))

(define (app? expr)
  (and (pair? expr)))

(define (call-args expr)
  (cdr expr))

(define (call-target expr)
  (car expr))

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
    ((_ (prim-name si env arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-predicate* #t)
       (putprop 'prim-name '*arg-count*
                (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                (lambda (si env arg* ...) b b* ...))))))

(define-predicate (fixnum? si env arg)
  (emit-expr si env arg)
  (emit "    and $~s, %al" fxmask)
  (emit "    cmp $~s, %al" fxtag)
  'EQ)

(define-predicate (fxzero? si env arg)
  (emit-expr si env arg)
  (emit "    testl %eax, %eax")
  'EQ)

(define-predicate (null? si env arg)
  (emit-expr si env arg)
  (emit "    cmp $~s, %al" nullval)
  'EQ)

(define-predicate (boolean? si env arg)
  (emit-expr si env arg)
  (emit "    and $~s, %al" bool_mask)
  (emit "    cmp $~s, %al" bool_tag)
  'EQ)

(define-predicate (char? si env arg)
  (emit-expr si env arg)
  (emit "    and $~s, %al" charmask)
  (emit "    cmp $~s, %al" chartag)
  'EQ)

(define-predicate (not si env arg)
  (emit-expr si env arg)
  (emit "    cmp $~s, %al" bool_f)
  'EQ)

(define-primitive (fxlognot si env arg)
  (emit-expr si env arg)
  (emit "    notl %eax")
  (emit "    and $~s, %eax" (lognot fxmask)))

(define-primitive (fx+ si env arg1 arg2)
  (define (out2)
    (emit-expr si env arg1)
    (emit "    movl %eax, ~s(%esp)" si)
    (emit-expr (- si wordsize) env arg2)
    (emit "    addl ~s(%esp), %eax" si))
  (define (out1 expr const)
    (emit-expr si env expr)
    (emit "    addl $~s, %eax" (immediate-rep const)))
  ;; If two values are constant, they are convolved in much higher-place, so not processed here.
  (cond ((fixnum? arg2) (out1 arg1 arg2))
        ((fixnum? arg1) (out1 arg2 arg1))
        (else (out2))))

(define-primitive (fx- si env arg1 arg2)
  (define (out2)
    (emit-expr si env arg2)
    (emit "    movl %eax, ~s(%esp)" si)
    (emit-expr (- si wordsize) env arg1)
    (emit "    subl ~s(%esp), %eax" si))
  (define (out1 expr const)
    (emit-expr si env expr)
    (emit "    subl $~s, %eax" (immediate-rep const)))
  (cond ((fixnum? arg2) (out1 arg1 arg2))
        (else (out2))))

(define-primitive (fx* si env arg1 arg2)
  (define (out2)
    (emit-expr si env arg1)
    (emit "    sarl $2, %eax")  ; Shift to the right.
    (emit "    movl %eax, ~s(%esp)" si)
    (emit-expr (- si wordsize) env arg2)
    (emit "    imull ~s(%esp), %eax" si))
  (define (out1 expr const)
    (emit-expr si env expr)
    (emit "    imull $~s, %eax" const))  ; No shift needed.
  (cond ((fixnum? arg2) (out1 arg1 arg2))
        ((fixnum? arg1) (out1 arg2 arg1))
        (else (out2))))

(define-primitive (fxlogor si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg2)
  (emit "    orl ~s(%esp), %eax" si))

(define-primitive (fxlogand si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg2)
  (emit "    andl ~s(%esp), %eax" si))

(define-predicate (fx= si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg2)
  (emit "    cmpl ~s(%esp), %eax" si)
  'EQ)

(define-predicate (fx< si env arg1 arg2)
  (define (out2)
    (emit-expr si env arg2)
    (emit "    movl %eax, ~s(%esp)" si)
    (emit-expr (- si wordsize) env arg1)
    (emit "    cmpl ~s(%esp), %eax" si))
  (define (out1 expr const)
    (emit-expr si env expr)
    (emit "    cmpl $~s, %eax" (immediate-rep const)))
  (cond ((fixnum? arg2) (out1 arg1 arg2) 'LT)
        ((fixnum? arg1) (out1 arg2 arg1) 'GT)
        (else (out2) 'LT)))

(define-predicate (fx<= si env arg1 arg2)
  (emit-expr si env arg2)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg1)
  (emit "    cmpl ~s(%esp), %eax" si)
  'LE)

(define-predicate (fx> si env arg1 arg2)
  (emit-expr si env arg2)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg1)
  (emit "    cmpl ~s(%esp), %eax" si)
  'GT)

(define-predicate (fx>= si env arg1 arg2)
  (emit-expr si env arg2)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg1)
  (emit "    cmpl ~s(%esp), %eax" si)
  'GE)
