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

;convert from fraction a/b to continued fraction
(define (convert-fr-cf a b)
  (define (helper p q count)
    (cond ((and (= (return-coefficient p q) 0) (> count 0)) '())
          (else (cons-stream (return-coefficient p q) (helper q (return-rest p q) (+ count 1))))))
  (helper a b 0))

;displays continued fraction
(define (display-cf st max)
  (cond ((stream-null? st) 'end)
        ((= max 0) (display 'end))
        (else (display (stream-car st))
              (newline)
              (display-cf (stream-cdr st) (- max 1)))))

;(display-cf (convert-fr-cf -95 82) 10)

(define (invert fr)
  (/ 1 fr))
  

;converts from continued fraction to fraction
(define (convert-fr cf precision)
  (cond ((null? (stream-cdr cf)) (stream-car cf))
        ((= precision 0) (/ (stream-car cf) 1))
        (else (+ (stream-car cf) (invert (convert-fr (stream-cdr cf) (- precision 1)))))))

;(display-cf(convert-fr-cf (sqrt 2) 1) 10000)

(define (floor->exact x)
  (inexact->exact (floor x)))



;converts from decimal value to contued fraction
(define (convert-dec-cf x precision)
  (cond ((= x 0)  '())
        ((< (- x (floor x)) precision)(cons-stream (floor->exact x) (convert-dec-cf 0 precision)))
        (else (cons-stream (floor->exact x) (convert-dec-cf (invert (- x (floor x))) precision)))))

;(display-cf (convert-dec-cf  1.1 .000000000000000000001) 1000)

;generates e
(define (cf-e)
  (define (helper next-val)
    (cond ((= next-val 0) 2)
          ((= (modulo next-val 3) 2) (+ (* 2 (quotient next-val 3)) 2))
          (else 1)))
  (define (generate-e count n)
    (cond ((= count 0) '())
          (else (cons-stream (helper n) (generate-e (- count 1) (+ n 1))))))
  (generate-e 1000 0))



;(display-cf (cf-e) 10)

;adding continued fraction to normal fraction
(define (addition cf-init a b)
  ;selectors
  (define (s-a mat)
    (car (car mat)))
  (define (s-b mat)
    (caar (cdr mat)))
  (define (s-c mat)
    (cadr (car mat)))
  (define (s-d mat)
    (cadr (cadr mat)))
  ;ingestion and egestion (either matrix 
  (define (ingestion tbt p)
    (list (list (s-b tbt) (s-d tbt)) (list (+ (s-a tbt) (* (s-b tbt) p)) (+ (s-c tbt) (* (s-d tbt) p)))))
  (define (egestion tbt q)
    (list (list (s-c tbt) (- (s-a tbt) (* (s-c tbt) q))) (list (s-d tbt) (- (s-b tbt) (* (s-d tbt) q)))))
  ;generate input
  (define (gen cf1 whole-fr)
    (cond ((null? whole-fr) '())
          ((stream-null? cf1) (car whole-fr))
          (else (stream-car cf1))))
  (define (gen-rest-cf cf)
    (cond ((stream-null? cf) '())
          (else (stream-cdr cf))))
  (define (gen-rest-whole-fr cf whole-fr)
    (cond ((null? whole-fr) '())
          ((stream-null? cf) (cdr whole-fr))
          (else whole-fr)))
  ;logic for selecting ingestion or egestion
  (define (overall tbt cf)
;    (newline)
;    (display 'tbt)
;    (display tbt)
;    (newline)
   ; (newline)
    (cond ((or (= (s-c tbt) 0) (= (s-d tbt) 0))
           (overall (ingestion tbt (stream-car cf)) (stream-cdr cf)))
          ((and (stream-null? cf) (= (quotient (s-a tbt) (s-c tbt)) (quotient (s-b tbt) (s-d tbt))))
           (cons-stream (quotient (s-b tbt) (s-d tbt)) (overall (egestion tbt (quotient (s-a tbt) (s-c tbt))) cf)))
          ((stream-null? cf)
           (cons-stream (quotient (s-b tbt) (s-d tbt)) '())) 
          ((= (quotient (s-a tbt) (s-c tbt)) (quotient (s-b tbt) (s-d tbt)))
           (cons-stream (quotient (s-a tbt) (s-c tbt)) (overall (egestion tbt (quotient (s-a tbt) (s-c tbt))) cf)))
          (else (overall (ingestion tbt (stream-car cf)) (stream-cdr cf) ))))
  (overall '((1 2) (2 0)) cf-init))

(display-cf (cf-e) 1000)



  
  
  

 