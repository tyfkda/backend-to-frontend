(use util.match)

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

(define HEAP-ALIGN 8)

(define tag_mask 7)
(define pair_tag 1)
(define pair_size 8)
(define pair_car 0)
(define pair_cdr 4)

(define vector_tag 5)
(define vector_num 0)
(define vector_buf 4)

(define string_tag 6)
(define string_size 0)
(define string_buf 4)


(define (emit-program expr)
  (emit-function-header "scheme_entry")
  (emit "    movl 4(%esp), %ecx")
  (emit "    movl %ebx, 4(%ecx)")
  (emit "    movl %esi, 16(%ecx)")
  (emit "    movl %edi, 20(%ecx)")
  (emit "    movl %ebp, 24(%ecx)")
  (emit "    movl %esp, 28(%ecx)")
  (emit "    movl 12(%esp), %ebp")
  (emit "    movl 8(%esp), %esp")
  (emit "    call L_scheme_entry")
  (emit "    movl 4(%ecx), %ebx")
  (emit "    movl 16(%ecx), %esi")
  (emit "    movl 20(%ecx), %edi")
  (emit "    movl 24(%ecx), %ebp")
  (emit "    movl 28(%ecx), %esp")
  (emit "    ret")

  (cond ((letrec? expr) (emit-letrec expr))
        (else
         (emit-label "L_scheme_entry")
         (emit-expr #t (- wordsize) '() expr)
         (emit-ret))))

(define (emit-function-header funcname)
  (emit "    .text")
  (emit (string-append "    .global " funcname))
  (emit (string-append funcname ":")))

(define (emit-expr tail? si env expr)
  (cond
   ((immediate? expr) (emit-immediate tail? expr))
   ((variable? expr)  (emit-variable-ref env expr))
;;    ((and? expr)       (emit-and si env expr))
;;    ((or? expr)        (emit-or si env expr))
   ((primcall? expr)  (emit-primcall tail? si env expr))
   ((predicate-call? expr) (emit-predicate-val si env expr))
   ((app? expr)       (emit-app tail? si env expr))
   (else (error "not implemented"))))

(define (emit-immediate tail? x)
  (emit "    movl $~a, %eax" (immediate-rep x))
  (when tail?
    (emit-ret)))

(define (emit-primcall tail? si env expr)
  (let ((prim (car expr))
        (args (cdr expr)))
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) tail? si env args)
    (when tail?
      (emit-ret))))

(define (emit-predicate-val si env expr)
  (let ((c (emit-predicate-test si env expr)))
    (emit-to-boolean c)))

(define (emit-predicate-test si env expr)
  (let ((prim (car expr))
        (args (cdr expr)))
    (check-primcall-args prim args)
    (apply (predicate-emitter prim) si env args)))

(define (emit-label label)
  (emit "~a:" label))

(define (emit-test tail? si env expr)
  (if (predicate-call? expr)
      (emit-predicate-test si env expr)
    (begin
      (emit-expr #f si env expr)
      (emit "    cmp $~a, %al" bool_f)
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

(define (emit-stack-save si)
  (emit "    movl %eax, ~a(%esp)" si))

(define (emit-stack-load si)
  (emit "    movl ~a(%esp), %eax" si))

(define (emit-variable-ref env var)
  (cond
   ((lookup var env) => emit-stack-load)
   (else (error "unbound variable: " var))))

(define (emit-letrec expr)
  (let* ((bindings (letrec-bindings expr))
         (lvars (map lhs bindings))
         (lambdas (map rhs bindings))
         (labels (unique-labels lvars))
;         (labels (map symbol->string lvars))
         (env (make-initial-env lvars labels)))
    (for-each (emit-lambda env) lambdas labels)
    (emit-label "L_scheme_entry")
    (emit-scheme-entry (letrec-body expr) env)))

(define (emit-scheme-entry body env)
  (emit-expr #t (- wordsize) env body))

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
          (emit-expr #t si env body)
          (emit-ret))
         (else
          (f (rest fmls)
             (- si wordsize)
             (extend-env (first fmls) si env))))))))

(define (emit-app tail? si env expr)
  (define (emit-arguments si args)
    (unless (empty? args)
      (emit-expr #f si env (first args))
      (emit "    movl %eax, ~a(%esp)" si)
      (emit-arguments (- si wordsize) (rest args))))
  (emit-arguments (- si wordsize) (call-args expr))
  (let ((label (lookup (call-target expr) env)))
    (unless label
      (error "unbound variable:" (call-target expr)))
    (if tail?
        (begin
          (emit-shift (- si wordsize) (- wordsize) (length (call-args expr)))
          (emit-jmp label))
      (begin
        (emit-adjust-base (+ si wordsize))
        (emit-call si label)
        (emit-adjust-base (- (+ si wordsize)))))))

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

(define (emit-shift from to argnum)
  (when (> argnum 0)
    (emit "    movl ~a(%esp), %eax" from)
    (emit "    movl %eax, ~a(%esp)" to)
    (emit-shift (- from wordsize) (- to wordsize) (- argnum 1))))
(define (emit-jmp label)
  (emit "    jmp ~a" label))
(define (emit-ret)
  (emit "    ret"))

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
    (if (or (< n 0) (= m n))
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
      (let ((L (format "L_~a" count)))
        (set! count (add1 count))
        L))))

(define (unique-labels expr)
  (map (lambda (_) (unique-label)) expr))

#|
(define-syntax define-primitive
  (syntax-rules ()
    ((_ (prim-name tail? si env arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
                (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                (lambda (tail? si env arg* ...) b b* ...))))))
|#
(define-macro (define-primitive defs . body*)
  (match defs
         ((prim-name tail? si env . arg*)
          `(begin
             (putprop ',prim-name '*is-prim* #t)
             (putprop ',prim-name '*arg-count*
                      ,(if (list? arg*)
                           (length arg*)
                           -1))
             (putprop ',prim-name '*emitter*
                      (lambda (,tail? ,si ,env ,@arg*) ,@body*))))))

(define-primitive (fxadd1 tail? si env arg)
  (emit-expr #f si env arg)
  (emit "    addl $~a, %eax" (immediate-rep 1)))
(define-primitive ($fxadd1 tail? si env arg)
  (emit-expr #f si env arg)
  (emit "    addl $~a, %eax" (immediate-rep 1)))

(define-primitive (fxsub1 tail? si env arg)
  (emit-expr #f si env arg)
  (emit "    subl $~a, %eax" (immediate-rep 1)))
(define-primitive ($fxsub1 tail? si env arg)
  (emit-expr #f si env arg)
  (emit "    subl $~a, %eax" (immediate-rep 1)))

(define-primitive (fixnum->char tail? si env arg)
  (emit-expr #f si env arg)
  (emit "    shll $~a, %eax" (- charshift fxshift))
  (emit "    orl $~a, %eax" chartag))
(define-primitive ($fixnum->char tail? si env arg)
  (emit-expr #f si env arg)
  (emit "    shll $~a, %eax" (- charshift fxshift))
  (emit "    orl $~a, %eax" chartag))

(define-primitive (char->fixnum tail? si env arg)
  (emit-expr #f si env arg)
  (emit "    shrl $~a, %eax" (- charshift fxshift)))
(define-primitive ($char->fixnum tail? si env arg)
  (emit-expr #f si env arg)
  (emit "    shrl $~a, %eax" (- charshift fxshift)))

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
    (emit "    sal $~a, %al" bool_bit)
    (emit "    or $~a, %al" bool_f)))

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
  (emit-expr #f si env arg)
  (emit "    and $~a, %al" fxmask)
  (emit "    cmp $~a, %al" fxtag)
  'EQ)

(define-predicate (fxzero? si env arg)
  (emit-expr #f si env arg)
  (emit "    testl %eax, %eax")
  'EQ)
(define-predicate ($fxzero? si env arg)
  (emit-expr #f si env arg)
  (emit "    testl %eax, %eax")
  'EQ)

(define-predicate (null? si env arg)
  (emit-expr #f si env arg)
  (emit "    cmp $~a, %al" nullval)
  'EQ)

(define-predicate (boolean? si env arg)
  (emit-expr #f si env arg)
  (emit "    and $~a, %al" bool_mask)
  (emit "    cmp $~a, %al" bool_tag)
  'EQ)

(define-predicate (char? si env arg)
  (emit-expr #f si env arg)
  (emit "    and $~a, %al" charmask)
  (emit "    cmp $~a, %al" chartag)
  'EQ)

(define-predicate (not si env arg)
  (emit-expr #f si env arg)
  (emit "    cmp $~a, %al" bool_f)
  'EQ)

(define-primitive (fxlognot tail? si env arg)
  (emit-expr #f si env arg)
  (emit "    notl %eax")
  (emit "    and $~a, %eax" (lognot fxmask)))

(define-predicate (char= si env a b)
  ;; @todo: Check whether the arguments area character.
  (emit-expr #f si env a)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr #f (- si wordsize) env b)
  (emit "    cmpl %eax, ~a(%esp)" si)
  'EQ)

(define-primitive (fx+ tail? si env arg1 arg2)
  (define (out2)
    (emit-expr #f si env arg1)
    (emit "    movl %eax, ~a(%esp)" si)
    (emit-expr #f (- si wordsize) env arg2)
    (emit "    addl ~a(%esp), %eax" si))
  (define (out1 expr const)
    (emit-expr #f si env expr)
    (emit "    addl $~a, %eax" (immediate-rep const)))
  ;; If two values are constant, they are convolved in much higher-place, so not processed here.
  (cond ((fixnum? arg2) (out1 arg1 arg2))
        ((fixnum? arg1) (out1 arg2 arg1))
        (else (out2))))

(define-primitive (fx- tail? si env arg1 arg2)
  (define (out2)
    (emit-expr #f si env arg2)
    (emit "    movl %eax, ~a(%esp)" si)
    (emit-expr #f (- si wordsize) env arg1)
    (emit "    subl ~a(%esp), %eax" si))
  (define (out1 expr const)
    (emit-expr #f si env expr)
    (emit "    subl $~a, %eax" (immediate-rep const)))
  (cond ((fixnum? arg2) (out1 arg1 arg2))
        (else (out2))))

(define-primitive (fx* tail? si env arg1 arg2)
  (define (out2)
    (emit-expr #f si env arg1)
    (emit "    sarl $2, %eax")  ; Shift to the right.
    (emit "    movl %eax, ~a(%esp)" si)
    (emit-expr #f (- si wordsize) env arg2)
    (emit "    imull ~a(%esp), %eax" si))
  (define (out1 expr const)
    (emit-expr #f si env expr)
    (emit "    imull $~a, %eax" const))  ; No shift needed.
  (cond ((fixnum? arg2) (out1 arg1 arg2))
        ((fixnum? arg1) (out1 arg2 arg1))
        (else (out2))))

(define-primitive (fxlogor tail? si env arg1 arg2)
  (emit-expr #f si env arg1)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr #f (- si wordsize) env arg2)
  (emit "    orl ~a(%esp), %eax" si))

(define-primitive (fxlogand tail? si env arg1 arg2)
  (emit-expr #f si env arg1)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr #f (- si wordsize) env arg2)
  (emit "    andl ~a(%esp), %eax" si))

(define-predicate (fx= si env arg1 arg2)
  (emit-expr #f si env arg1)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr #f (- si wordsize) env arg2)
  (emit "    cmpl ~a(%esp), %eax" si)
  'EQ)

(define-predicate (fx< si env arg1 arg2)
  (define (out2)
    (emit-expr #f si env arg2)
    (emit "    movl %eax, ~a(%esp)" si)
    (emit-expr #f (- si wordsize) env arg1)
    (emit "    cmpl ~a(%esp), %eax" si))
  (define (out1 expr const)
    (emit-expr #f si env expr)
    (emit "    cmpl $~a, %eax" (immediate-rep const)))
  (cond ((fixnum? arg2) (out1 arg1 arg2) 'LT)
        ((fixnum? arg1) (out1 arg2 arg1) 'GT)
        (else (out2) 'LT)))

(define-predicate (fx<= si env arg1 arg2)
  (emit-expr #f si env arg2)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr #f (- si wordsize) env arg1)
  (emit "    cmpl ~a(%esp), %eax" si)
  'LE)

(define-predicate (fx> si env arg1 arg2)
  (emit-expr #f si env arg2)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr #f (- si wordsize) env arg1)
  (emit "    cmpl ~a(%esp), %eax" si)
  'GT)

(define-predicate (fx>= si env arg1 arg2)
  (emit-expr #f si env arg2)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr #f (- si wordsize) env arg1)
  (emit "    cmpl ~a(%esp), %eax" si)
  'GE)



(define-primitive (cons tail? si env a d)
  (emit-expr #f si env a)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr #f (- si wordsize) env d)
  (emit "    movl %eax, ~a(%esp)" (- si wordsize))
  (emit-heap-allocation pair_size)
  (emit "    movl ~a(%esp), %eax" (- si wordsize))
  (emit "    movl %eax, ~a(%edx)" pair_cdr)
  (emit "    movl ~a(%esp), %eax" si)
  (emit "    movl %eax, ~a(%edx)" pair_car)
  (emit "    movl %edx, %eax")
  (emit "    orl $~a, %eax" pair_tag))

(define-primitive (car tail? si env cell)
  (emit-expr #f si env cell)
  (emit "    movl ~a(%eax), %eax" (- pair_car pair_tag)))

(define-primitive (cdr tail? si env cell)
  (emit-expr #f si env cell)
  (emit "    movl ~a(%eax), %eax" (- pair_cdr pair_tag)))

(define-predicate (pair? si env v)
  (emit-expr #f si env v)
  (emit "    and $~a, %al" tag_mask)
  (emit "    cmp $~a, %al" pair_tag)
  'EQ)

(define-primitive (set-car! tail? si env cell a)
  (emit-expr #f si env cell)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr #f (- si wordsize) env a)
  (emit "    movl ~a(%esp), %edx" si)
  (emit "    movl %eax, ~a(%edx)" (- pair_car pair_tag)))

(define-primitive (set-cdr! tail? si env cell d)
  (emit-expr #f si env cell)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr #f (- si wordsize) env d)
  (emit "    movl ~a(%esp), %edx" si)
  (emit "    movl %eax, ~a(%edx)" (- pair_cdr pair_tag)))

(define-predicate (eq? si env a b)
  (emit-expr #f si env a)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr #f (- si wordsize) env b)
  (emit "    cmpl %eax, ~a(%esp)" si)
  'EQ)



(define (emit-heap-allocation expr)
  ;; Allocated heap address is put into edx register.
  (cond ((fixnum? expr)
         (emit "    movl %ebp, %edx")
         (emit "    addl $~a, %ebp" (align-up expr HEAP-ALIGN)))
        ((eq? expr 'eax)  ; eax is a actual numeral, not internal representation.
         (emit "    addl $~a, %eax" (- HEAP-ALIGN 1))
         (emit "    andl $~a, %eax" (- HEAP-ALIGN))
         (emit "    addl %ebp, %eax")
         (emit "    xchgl %eax, %ebp")  ; ebp: next heap free area.
         (emit "    movl %eax, %edx"))  ; edx: allocated heap area.
        (else
         (error "must not happen"))))

(define (align-up x align)
  (* (floor (/ (+ x align -1) align)) align))




(define-primitive (if tail? si env test conseq altern)
  (let ((alt-label (unique-label))
        (end-label (unique-label)))
    (emit-jump-if-not (emit-test #f si env test)
                      alt-label)
    (emit-expr tail? si env conseq)
    (emit "    jmp ~a" end-label)
    (emit-label alt-label)
    (emit-expr tail? si env altern)
    (emit-label end-label)))

(define-primitive (let tail? si env bindings . body)
  (let loop ((bindings bindings)
             (si si)
             (new-env env))
    (if (empty? bindings)
        (emit-expr-list tail? si new-env body)
      (let ((b (first bindings)))
        (emit-expr #f si env (rhs b))
        (emit-stack-save si)
        (loop (rest bindings)
              (next-stack-index si)
              (extend-env (lhs b) si new-env))))))


(define-primitive (and tail? si env . expr)
  (define (test-false expr false-label end-label)
    (if (predicate-call? expr)
        (begin
          (emit-predicate-test si env expr)
          (emit "    jne ~a" false-label))  ; Jump to #f if failed.
      (begin
        (emit-test #f si env expr)
        (emit "    je ~a" end-label))))  ; If it is a value, then it already #f so jump to the end.
  (let ((p expr))
    (cond ((null? p)
           (emit "    mov $~a, %eax" bool_t))
          (else
           (let ((false-label (unique-label))
                 (end-label (unique-label)))
             (let loop ((p p))
               (if (null? (cdr p)) ; Last
                   (emit-expr tail? si env (car p))
                 (begin
                   (test-false (car p) false-label end-label)
                   (loop (cdr p)))))
             (emit "    jmp ~a" end-label)
             (emit-label false-label)
             (emit "    mov $~a, %eax" bool_f)
             (emit-label end-label))))))

(define-primitive (or tail? si env . expr)
  (define (test-true expr true-label end-label)
    (if (predicate-call? expr)
        (begin
          (emit-predicate-test si env expr)
          (emit "    je ~a" true-label))  ; Jump to #t if succeeded.
      (begin
        (emit-test #f si env expr)
        (emit "    jne ~a" end-label))))  ; If it is a value, then jump to the end.
  (let ((p expr))
    (cond ((null? p)
           (emit "    mov $~a, %eax" bool_f))
          (else
           (let ((true-label (unique-label))
                 (end-label (unique-label)))
             (let loop ((p p))
               (if (null? (cdr p))
                   (emit-expr tail? si env (car p))
                 (begin
                   (test-true (car p) true-label end-label)
                   (loop (cdr p)))))
             (emit "    jmp ~a" end-label)
             (emit-label true-label)
             (emit "    mov $~a, %eax" bool_t)
             (emit-label end-label))))))

(define (emit-expr-list tail? si env exprs)
  (when (not (null? exprs))
    (let loop ((p exprs))
      (if (null? (cdr p))
          (emit-expr tail? si env (car p))
        (begin
          (emit-expr #f si env (car p))
          (loop (cdr p)))))))

(define-primitive (begin tail? si env . expr)
  (print (list 'BEGIN expr))
  (emit-expr-list tail? si env expr))

;; 固定数分だけビットシフトする
(define (emit-const-shift-fx n)
  ;; n > 0 : 左シフト
  ;; n < 0 : 右シフト
  (cond ((> n 0) (emit "    shll $~a, %eax" n))
		((< n 0) (emit "    shrl $~a, %eax" (- n)))))

(define-primitive (make-vector tail? si env n . v)
  (let ((v (cond ((null? v) #f)
                 ((null? (cdr v)) (car v))
                 (else (error "illegal arguments for make-vector"))))
        (l-loop (unique-label))
        (l-test (unique-label)))
    (emit-expr #f si env v)
    (emit "    movl %eax, ~a(%esp)" si)
    (emit-expr #f (- si wordsize) env n)
    ;; @todo: Check whether negative or not.
    (emit "    movl %eax, ~a(%esp)" (- si wordsize))
    (emit-const-shift-fx (- 2 fxshift))  ; x4
    (emit "    addl $~a, %eax" wordsize)  ; +4
    (emit-heap-allocation 'eax)
    (emit "    movl ~a(%esp), %ebx" (- si wordsize))  ; ebx = element count (<< fxshift)
    (emit "    movl %ebx, ~a(%edx)" vector_num)

    (emit "    movl ~a(%esp), %eax" si)  ; Initial element.
    (emit "    lea ~a(%edx), %edi" vector_buf)  ; Array element.
    (emit "    testl %ebx, %ebx")
    (emit-jmp l-test)
    (emit-label l-loop)
    (emit "    movl %eax, (%edi)")
    (emit "    addl $~a, %edi" wordsize)
    (emit "    subl $~a, %ebx" wordsize)
    (emit-label l-test)
    (emit "    jne ~a" l-loop)
    (emit "    movl %edx, %eax")  ; Vector.
    (emit "    orl $~a, %eax" vector_tag)))

(define-predicate (vector? si env v)
  (emit-expr #f si env v)
  (emit "    and $~a, %al" tag_mask)
  (emit "    cmp $~a, %al" vector_tag)
  'EQ)

(define-primitive (vector-length tail? si env v)
  (emit-expr #f si env v)
  ;; @todo: Check wheter vector or not.
  (emit "    movl ~a(%eax), %eax" (- vector_num vector_tag)))

(define-primitive (vector-set! tail? si env v idx val)
  (emit-expr #f si env val)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr #f (- si wordsize) env idx)
  (emit "    movl %eax, ~a(%esp)" (- si wordsize))
  (emit-expr #f (- si (* 2 wordsize)) env v)
  ;; @todo: Check whether vector or not.
  ;; @todo: Check whether index is in-range.
  (emit "    movl %eax, %edx")
  (emit "    movl ~a(%esp), %eax" (- si wordsize))
  (emit-const-shift-fx (- 2 fxshift))  ; x4
  (emit "    addl %eax, %edx")
  (emit "    movl ~a(%esp), %eax" si)
  (emit "    movl %eax, ~a(%edx)" (- vector_buf vector_tag)))

(define-primitive (vector-ref tail? si env v idx)
  (emit-expr #f si env idx)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr #f (- si wordsize) env v)
  ;; @todo: Check whether vector or not.
  ;; @todo: Check whether index is in-range.
  (emit "    movl %eax, %edx")
  (emit "    movl ~a(%esp), %eax" si)
  (emit-const-shift-fx (- 2 fxshift))  ; x4
  (emit "    movl ~a(%edx,%eax), %eax"  (- vector_buf vector_tag)))


(define-primitive (make-string tail? si env n . v)
  (let ((v (cond ((null? v) #\space)
                 ((null? (cdr v)) (car v))
                 (else (error "illegal arguments for make-string"))))
        (l-loop (unique-label))
        (l-test (unique-label)))
    (emit-expr #f si env v)
    (emit "    movl %eax, ~a(%esp)" si)
    (emit-expr #f (- si wordsize) env n)
    ;; @todo: Check whehter negative or not.
    (emit-const-shift-fx (- fxshift))  ; x1
    (emit "    movl %eax, ~a(%esp)" (- si wordsize))
    (emit "    addl $~a, %eax" (+ wordsize 1))  ; +4+1
    (emit-heap-allocation 'eax)
    (emit "    movl ~a(%esp), %ebx" (- si wordsize))  ; ebx = Element count.
    (emit "    movl %ebx, ~a(%edx)" string_size)

    (emit "    movl ~a(%esp), %eax" si)  ; Initial element.
    (emit "    shrl $~a, %eax" charshift)
    (emit "    lea ~a(%edx), %edi" string_buf)  ; Array element.
    (emit "    testl %ebx, %ebx")
    (emit-jmp l-test)
    (emit-label l-loop)
    (emit "    movb %al, (%edi)")
    (emit "    incl %edi")
    (emit "    decl %ebx")
    (emit-label l-test)
    (emit "    jne ~a" l-loop)
    (emit "    movb $0, (%edi)")  ; \0
    (emit "    movl %edx, %eax")  ; String.
    (emit "    orl $~a, %eax" string_tag)))

(define-predicate (string? si env v)
  (emit-expr #f si env v)
  (emit "    and $~a, %al" tag_mask)
  (emit "    cmp $~a, %al" string_tag)
  'EQ)

(define-primitive (string-set! tail? si env str idx val)
  (emit-expr #f si env val)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr #f (- si wordsize) env idx)
  (emit-const-shift-fx (- fxshift))
  (emit "    movl %eax, ~a(%esp)" (- si wordsize))
  (emit-expr #f (- si (* 2 wordsize)) env str)
  ;; @todo: Check whether string or not.
  ;; @todo: Check whether index is in-range.
  ;; @todo: Check whether character or not.
  (emit "    movl %eax, %edx")
  (emit "    movl ~a(%esp), %eax" si)
  (emit "    shrl $~a, %eax" charshift)  ; Internal string representation to ascii code.
  (emit "    movl ~a(%esp), %ebx" (- si wordsize))
  (emit "    movb %al, ~a(%edx, %ebx)" (- string_buf string_tag)))

(define-primitive (string-ref tail? si env str idx)
  (emit-expr #f si env idx)
  (emit-const-shift-fx (- fxshift))
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr #f (- si wordsize) env str)
  ;; @todo: Check whether string or not.
  ;; @todo: Check whether index is in-range.
  (emit "    movl %eax, %edx")
  (emit "    movl ~a(%esp), %eax" si)
  (emit "    movb ~a(%edx,%eax), %al"  (- string_buf string_tag))
  ;; @todo: Convert the value from 8bit to 32bit
  (emit "    shll $~a, %eax" charshift)
  (emit "    orl $~a, %eax" chartag))

(define-primitive (string-length tail? si env str)
  (emit-expr #f si env str)
  ;; @todo: Check whether string or not.
  (emit "    movl ~a(%eax), %eax"  (- string_size string_tag))
  (emit "    shll $~a, %eax" fxshift))
