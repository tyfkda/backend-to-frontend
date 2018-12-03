(use gauche.parameter)

(define (printf . x) (apply format #t x))
(define fprintf format)
(define flush-output-port flush)
(define system sys-system)
(define (fxzero? x) (and fixnum? x) (zero? x))
(define (void) (values))
(define (add1 x) (+ x 1))
(define (sub1 x) (- x 1))

(define (compile-program expr)
  (emit-program expr))

(define *global-property-table* (make-hash-table))
(define (get-prop-table tbl)
  (unless (hash-table-exists? *global-property-table* tbl)
    (hash-table-put! *global-property-table* tbl (make-hash-table)))
  (hash-table-get *global-property-table* tbl))

(define (putprop key tbl val)
  (hash-table-put! (get-prop-table tbl) key val))

(define (getprop key tbl)
  (and (hash-table-exists? (get-prop-table tbl) key)
       (hash-table-get (get-prop-table tbl) key)))

;; Duplicated definitions:
;; test-with-string-output, run-compile, execute, get-string

;; Unsed functions:
;; runtime-file, input-filter, build-program, show-compiler-outut

;;;; original

(define all-tests '())

(define-syntax add-tests-with-string-output
  (syntax-rules (=>)
    [(_ test-name [expr => output-string] ...)
     (set! all-tests
        (cons
           '(test-name [expr string  output-string] ...)
            all-tests))]))

(define (build runtime-srcfn)
  (unless (zero? (system (string-append "gcc -o stst -m32 "
                                        runtime-srcfn
                                        " stst.s")))
    (error 'make "could not build target")))


(define (test-one test-id test runtime-srcfn)
  (let ([expr (car test)]
        [type (cadr test)]
        [out  (caddr test)])
    (printf "test ~s:~s ..." test-id expr)
    (flush-output-port)
    (case type
     [(string) (test-with-string-output test-id expr out runtime-srcfn)]
     [else (error 'test "invalid test type ~s" type)])
    (printf " ok\n")))

(define (test-all runtime-srcfn)
  (let f ([i 0] [ls (reverse all-tests)])
    (if (null? ls)
        (printf "passed all ~s tests\n" i)
        (let ([x (car ls)] [ls (cdr ls)])
          (let* ([test-name (car x)]
                 [tests (cdr x)]
                 [n (length tests)])
            (printf "Performing ~a tests ...\n" test-name)
            (let g ([i i] [tests tests])
              (cond
                [(null? tests) (f i ls)]
                [else
                 (test-one i (car tests) runtime-srcfn)
                 (g (add1 i) (cdr tests))])))))))


(define compile-port
  (make-parameter
    (current-output-port)
    (lambda (p)
       (unless (output-port? p)
         (error 'compile-port "not an output port ~s" p))
       p)))

(define (run-compile expr)
  (let ([p (open-output-file "stst.s")])
    (parameterize ([compile-port p])
       (compile-program expr))
    (close-output-port p)))


(define (execute)
  (unless (fxzero? (system "./stst > stst.out"))
    (error 'execute "produced program exited abnormally")))

(define (get-string)
  (with-output-to-string
    (lambda ()
      (with-input-from-file "stst.out"
        (lambda ()
          (let f ()
            (let ([c (read-char)])
              (cond
               [(eof-object? c) (void)]
               [else (display c) (f)]))))))))

(define (test-with-string-output test-id expr expected-output runtime-srcfn)
   (run-compile expr)
   (build runtime-srcfn)
   (execute)
   (unless (string=? expected-output (get-string))
     (error 'test (format "output mismatch for test ~s, expected ~s, got ~s"
                          test-id expected-output (get-string)))))

(define (emit . args)
  (apply fprintf (compile-port) args)
  (newline (compile-port)))
