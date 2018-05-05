(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (force delayed-object) (delayed-object))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())

(define stream-null? null?)
;
;(define (return-rest p q)
;  (-  p (* (floor (/ p q)) q)))
;
;(define (return-coefficient p q)
;  (floor (/ p q)))
;  
;
;(define (simple-frac->continued p q)
;  (cond ((= q 0) '(0))
;  (else (cons-stream (return-coefficient p q) (simple-frac->continued q (return-rest p q))))))
;
;(define (display-n str n)
;  (display (stream-car str))
;  (newline)
;  (cond ((or (= n 0) (= (stream-car str) 0)) )
;        (else (display-n (stream-cdr str) (- n 1)))))
;
;(define (pick-num bool)
;  (cond ((eq? bool #t) 1)
;        (else 2)))
;
;(define (one-two bool)
;  (cons-stream (pick-num bool) (one-two (not bool))))
;
;(display-n (simple-frac->continued 2 7) 5)

(define (return-coefficient p q)
  (cond ((= q 0) 0)
        (else (floor (/ p q)))))

(define (return-rest p q)
  (-  p (* (floor (/ p q)) q)))

(define (convert-fr-cf a b)
  (define (helper p q count)
    (cond ((and (= (return-coefficient p q) 0) (> count 0)) '())
          (else (cons-stream (return-coefficient p q) (helper q (return-rest p q) (+ count 1))))))
  (helper a b 0))

(define (display-cf st max)
  (cond ((stream-null? st) 'end)
        ((= max 0) (display 'end))
        (else (display (stream-car st))
              (newline)
              (display-cf (stream-cdr st) (- max 1)))))

;(display-cf (convert-fr-cf -95 82) 10)

(define (invert fr)
  (/ 1 fr))
  

(define (convert-fr cf precision)
  (cond ((null? (stream-cdr cf)) (stream-car cf))
        ((= precision 0) (/ (stream-car cf) 1))
        (else (+ (stream-car cf) (invert (convert-fr (stream-cdr cf) (- precision 1)))))))

;(display-cf(convert-fr-cf (sqrt 2) 1) 10000)

(define (floor->exact x)
  (inexact->exact (floor x)))




(define (convert-dec-cf x precision)
  (cond ((= x 0)  '())
        ((< (- x (floor x)) precision)(cons-stream (floor->exact x) (convert-dec-cf 0 precision)))
        (else (cons-stream (floor->exact x) (convert-dec-cf (invert (- x (floor x))) precision)))))

(display-cf (convert-dec-cf  .1) 1000)
                          

