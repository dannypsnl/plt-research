(import (chezscheme))

(define handler-key (gensym))
(define th #f)

(define x 0)

(define (sub)
  (define n
    (call/cc (lambda (k)
      (define abort (continuation-marks-first (current-continuation-marks) handler-key))
      (abort k))))
  (set! x (+ x n)))

(write x)
(newline)

(with-continuation-mark
  handler-key (lambda (resume)
    (set! th (fork-thread (lambda () (write 20))))
    (resume 10))
  (sub))

(write x)
(newline)
(thread-join th) ; blocked by runtime
