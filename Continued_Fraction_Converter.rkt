

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
    (newline)
    ;(display 'coeff)
    ;(display )
    (newline)
   ; (newline)
    (cond ((and (not (stream-null? cf))(or (= (s-c tbt) 0) (= (s-d tbt) 0)) ;if denominator 0
           (overall (ingestion tbt (stream-car cf)) (stream-cdr cf))))
          ((and (stream-null? cf) (= (quotient (s-a tbt) (s-c tbt)) (quotient (s-b tbt) (s-d tbt)))) ;once continued fraction finishes
           (cons-stream (quotient (s-b tbt) (s-d tbt)) (overall (egestion tbt (quotient (s-a tbt) (s-c tbt))) cf))) 
         ((stream-null? cf)
           (cons-stream (quotient (s-b tbt) (s-d tbt)) '()))
          ((= (quotient (s-a tbt) (s-c tbt)) (quotient (s-b tbt) (s-d tbt)))
           (cons-stream (quotient (s-a tbt) (s-c tbt)) (overall (egestion tbt (quotient (s-a tbt) (s-c tbt))) cf)))
          (else (overall (ingestion tbt (stream-car cf)) (stream-cdr cf) ))))
  (overall '((1 2) (2 0)) cf-init))
;INCOMPLETE - EXPLANATION ALSO INCOMPLETE
;(display-cf (addition (convert-fr-cf 13 11) 1 2) 1000)



;(define (cf-arithmetic cf1 cf2 op)
;  ;STARTING MATRICES - DEPENDING ON THE CHOSEN OPERATION, ONE OF THE MATRICES WILL BE SELECTED TO BE THE INITIAL MATRIX 
;  (define addition-matrix '((0 0) (1 0) (1 0) (0 1)))
;  (define subtraction-matrix '((0 1) (1 0) (-1 0) (0 0)))
;  (define multiplication-matrix '((0 1) (0 0) (0 0) (1 0)))
;  (define division-matrix '((0 0) (1 0) (0 1) (0 0)))
;  ;INFINITY - defined
;  (define inf +inf.0)
;  ;SELECTORS
;  (define (s-a m)
;    (caar m))
;  (define (s-b m)
;    (caadr m))
;  (define (s-c m)
;    (caaddr m))
;  (define (s-d m)
;    (caaddr(cdr m)))
;  (define (s-e m)
;    (cadar m))
;  (define (s-f m)
;    (cadadr m))
;  (define (s-g m)
;    (cadadr (cdr m)))
;  (define (s-h m)
;    (cadadr (cddr m)))
;    
;  ;MODIFIED QUOTIENT function that returns infinity if denominator equals 0
;  (define (q-mod n d)
;    (cond ((= d 0) 2147483647)
;          (else (quotient n d))))
;  ;INGESTION AND EGESTION - ingestion-p is when value from cf1 is taken in and ingestion q is when value from cf2 is taken in
;  (define (ingest-x tbf x)
;    (cond ((stream-null? x)
;           (list (list (s-a tbf) (s-b tbf)) (list (s-b tbf) (s-e tbf)) (list (s-a tbf) (s-e tbf)) (list (s-b tbf) (s-f tbf))))
;          (else (list (list (s-b tbf) (s-f tbf)) (list (+ (s-a tbf) (* (s-b tbf) (stream-car x))) (+ (s-e tbf) (* (s-f tbf) (stream-car x)))) (list (s-d tbf) (s-h tbf)) (list (+ (s-c tbf) (* (s-d tbf) (stream-car x))) (+ (s-g tbf) (* (s-h tbf) (stream-car x))))))))
;  (define (ingest-y tbf y)
;    (cond ((stream-null? y)
;           (list (list (s-a tbf) (s-e tbf)) (list (s-a tbf) (s-e tbf)) (list (s-c tbf) (s-g tbf)) (list (s-c tbf) (s-g tbf))))
;           (else (list (list (+ (* (s-a tbf) (stream-car y)) (s-b tbf)) (+ (* (s-e tbf) (stream-car y)) (s-f tbf))) (list (s-a tbf) (s-e tbf)) (list (+ (* (s-c tbf) (stream-car y)) (s-d tbf)) (+ (* (s-g tbf) (stream-car y)) (s-h tbf))) (list (s-c tbf) (s-g tbf))))))
;  (define (egest tbf r)
;    (list (list (s-e tbf) (- (s-a tbf) (* (s-e tbf) r))) (list (s-f tbf) (- (s-b tbf) (* (s-f tbf) r))) (list (s-g tbf) (- (s-c tbf) (* (s-g tbf) r))) (list (s-h tbf) (- (s-d tbf) (* (s-h tbf) r)))))
;  ;IDIFF: for use in conditions
;  (define (idiff a b)
;    (cond ((eq? a inf) a)
;          ((eq? b inf) b)
;          (else (abs (- a b)))))
;  ;MODIFIED STREAM CDR
;  (define (st-cdr-mod s)
;    (cond ((stream-null? s) (list inf))
;          (else (stream-cdr s))))
;  ;MODIFIED MULTIPLICATION
;  (define (m x y)
;    (* x y))
;  ;OVERALL - main logic
;  (define (overall tbf x y)
;    (newline)
;    (display 'tbf)
;    (display tbf)
;    (newline)
;    (cond ((= inf (s-e tbf) (s-f tbf) (s-g tbf) (s-h tbf))
;           (cons-stream inf '()))
;          ((= (q-mod (s-a tbf) (s-e tbf)) (q-mod (s-b tbf) (s-f tbf)) (q-mod (s-c tbf) (s-g tbf)) (q-mod (s-d tbf) (s-h tbf)))
;           (display 'output)
;           (display (q-mod (s-a tbf) (s-e tbf)))
;           (cons-stream (q-mod (s-a tbf) (s-e tbf)) (overall (egest tbf (q-mod (s-a tbf) (s-e tbf))) x y)))
;          ((> (max (idiff (q-mod (s-a tbf) (s-e tbf)) (q-mod (s-c tbf) (s-g tbf))) (idiff (q-mod (s-b tbf) (s-f tbf)) (q-mod (s-d tbf) (s-h tbf)))) ;(> (max (a/e - c/g) (b/f - d/h)
;              (max (idiff (q-mod (s-a tbf) (s-e tbf)) (q-mod (s-b tbf) (s-f tbf))) (idiff (q-mod (s-c tbf) (s-g tbf)) (q-mod (s-d tbf)(s-h tbf))))) ;(max (a/e - b/f) (c/g - d/h))
;           (display 'ingest-x)
;           (overall (ingest-x tbf x) (st-cdr-mod x) y))
;          (else
;           (display 'ingest-y)
;           (overall (ingest-y tbf y) x (st-cdr-mod y)))))
;  
;  (cond ((eq? op '+) (overall addition-matrix cf1 cf2))
;        ((eq? op '-) (overall subtraction-matrix cf1 cf2))
;        ((eq? op '*) (overall multiplication-matrix cf1 cf2))
;        ((eq? op '/) (overall division-matrix cf1 cf2))))
;;(display-cf (cf-arithmetic (convert-fr-cf 1 2) (convert-fr-cf 1 2) '+) 1000)
;
;;(or (= 2 1) (= 1 1) (= 1 0))

(define (cf-arithmetic cf1 cf2 op)
  ;STARTING MATRICES - DEPENDING ON THE CHOSEN OPERATION, ONE OF THE MATRICES WILL BE SELECTED TO BE THE INITIAL MATRIX 
  (define addition-matrix '((0 1) (1 0) (1 0) (0 0)))
  (define subtraction-matrix '((0 1) (1 0) (-1 0) (0 0)))
  (define multiplication-matrix '((0 1) (0 0) (0 0) (1 0)))
  (define division-matrix '((0 0) (1 0) (0 1) (0 0)))
  ;SELECTORS
  (define (s-a m)
    (caar m))
  (define (s-b m)
    (caadr m))
  (define (s-c m)
    (caaddr m))
  (define (s-d m)
    (caaddr(cdr m)))
  (define (s-e m)
    (cadar m))
  (define (s-f m)
    (cadadr m))
  (define (s-g m)
    (cadadr (cdr m)))
  (define (s-h m)
    (cadadr (cddr m)))

  (define (abs-difference x y)
    (cond ((null? x)
           (cond ((null? y)
                  0)
                 (else '())))
          ((null? y) '())
          (else (abs (- x  y)))))

  (define (divide n d)
    (cond ((= d 0) '())
          (else (quotient n d))))

  (define (compare-to x y)
    (cond ((= x  y) 0)
          ((< x y) -1)
          (else 1)))

  (define (compare x y)
    (cond ((null? x)
           (cond ((null? y) 0)
                 (else 1)))
          ((null? y) -1)
          (else (compare-to x y))))

  (define (isDone? tbf)
    (and (null? (divide (s-a tbf) (s-e tbf))) (null? (divide (s-b tbf) (s-f tbf))) (null?(divide (s-c tbf) (s-g tbf))) (null?(divide (s-d tbf) (s-h tbf)))))

  (define (ingest-x tbf x)
    (cond ((stream-null? x)
           (list (list (s-b tbf) (s-f tbf)) (list (s-b tbf) (s-f tbf)) (list (s-d tbf) (s-h tbf)) (list (s-d tbf) (s-h tbf))))
          (else (list (list (s-b tbf) (s-f tbf)) (list (+ (s-a tbf) (* (s-b tbf) (stream-car x))) (+ (s-e tbf) (* (s-f tbf) (stream-car x)))) (list (s-d tbf) (s-h tbf)) (list (+ (s-c tbf) (* (s-d tbf) (stream-car x))) (+ (s-g tbf) (* (s-h tbf) (stream-car x))))))))

  (define (ingest-y tbf y)
    (cond ((stream-null? y)
           (list (list (s-c tbf) (s-g tbf)) (list (s-d tbf) (s-h tbf)) (list (s-c tbf) (s-g tbf)) (list (s-d tbf) (s-h tbf))))
          (else (list (list (s-c tbf) (s-g tbf)) (list (s-d tbf) (s-h tbf)) (list (+ (s-a tbf) (* (s-c tbf) (stream-car y))) (+ (s-e tbf) (* (s-g tbf) (stream-car y)))) (list (+ (s-b tbf) (* (s-d tbf) (stream-car y))) (+ (s-f tbf) (* (s-h tbf) (stream-car y))))))))

  (define (check-egest tbf)
    (cond ((or (not (= (compare (divide (s-a tbf) (s-e tbf)) (divide (s-b tbf) (s-f tbf))) 0)) (not (= (compare (divide (s-b tbf) (s-f tbf)) (divide (s-c tbf) (s-g tbf))) 0)) (not (= (compare (divide (s-c tbf) (s-g tbf)) (divide (s-d tbf) (s-h tbf))) 0)))
                 '())
                 (else (divide (s-a tbf) (s-e tbf)))))

  (define (x-or-y? tbf)
    (> (compare (abs-difference (divide (s-b tbf) (s-f tbf)) (divide (s-a tbf) (s-e tbf))) (abs-difference (divide (s-c tbf) (s-g tbf)) (divide (s-a tbf) (s-e tbf)))) 0))

  (define (egest tbf r)
    (list (list (s-e tbf) (- (s-a tbf) (* (s-e tbf) r))) (list (s-f tbf) (- (s-b tbf) (* (s-f tbf) r))) (list (s-g tbf) (- (s-c tbf) (* (s-g tbf) r))) (list (s-h tbf) (- (s-d tbf) (*(s-h tbf) r)))))

  (define (mod-st-cdr x)
    (cond ((stream-null? x) '())
          (else (stream-cdr x))))
  
  (define (overall tbf x y)
    (cond ((isDone? tbf) '())
          ((not (null? (check-egest tbf))) (cons-stream (check-egest tbf) (overall (egest tbf (check-egest tbf)) x y)))
          ((x-or-y? tbf) (overall (ingest-x tbf x) (mod-st-cdr x) y))
          (else (overall (ingest-y tbf y) x (mod-st-cdr y)))))

  (cond ((eq? op '+) (overall addition-matrix cf1 cf2))
        ((eq? op '-) (overall subtraction-matrix cf1 cf2))
        ((eq? op '*) (overall multiplication-matrix cf1 cf2))
        ((eq? op '/) (overall division-matrix cf1 cf2))))

(convert-fr (cf-arithmetic (convert-fr-cf 1 3) (convert-fr-cf 1 3) '+) 1000)