(define fxshift 2)
(define fxmask #x03)
(define bool_f #x2f)
(define bool_t #x6f)
(define wordsize 4) ; bytes

(define nullval #b00111111)
(define charshift 8)
(define chartag #b00001111)

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

(define (emit-program x)
  (unless (immediate? x) (error ---))
  (emit "    .text")
  (emit "    .global scheme_entry")
  (emit "scheme_entry:")
  (emit "    movl $~s, %eax" (immediate-rep x))
  (emit "    ret"))
