#lang racket

(define fkt
  (lambda (x)
    (+ (* -0.0005 (expt x 6)) (* 0.001 (expt x 5)) (* 0.11 (expt x 4)) (* -0.025 (expt x 3)) (* -7.5(expt x 2)) (* x -6) 140)))

(define x-von -10)
(define x-bis 12)
(define y-von -100)
(define y-bis 160)
(define dx 0.33)

(define eps 1e-10)


(define h 1e-06)
(define x-eps 1e-10)
(define y-eps 1e-06)
(define anz 3)

(define ableitung
  (lambda (fkt)
    (lambda (x)
      (/ (- (fkt (+ x h)) (fkt x)) h))))

(define runden
  (lambda (x anz)
    (/ (round (* x (expt 10 anz))) (expt 10 anz))))

(define tabausgabe
  (lambda (ls1 ls2)
    (if (or (null? ls1) (null? ls2))
        'ok
        (begin
          (display (number->string (runden (car ls1) anz))
                   (string #\tab) 
                   (number->string (runden (car ls2) anz)))
          (tabausgabe (cdr ls1) (cdr ls2))))))

(define wertetabelle
  (lambda (fkt ls)
    (display "x" (string #\tab) "f(x)")
    (tabausgabe ls (map fkt ls))))

(define wertausgabe
  (lambda (ls)
    (letrec
        ([ausgabe
          (lambda (ls)           
            (cond
              [(null? ls) '()]
              [(null? (cdr ls)) (list (runden (car ls) anz))]
              [else
               (cons (runden (car ls) anz) (cons ", " (ausgabe (cdr ls))))]))])
      (apply display (ausgabe ls))
      'ok)))

(define nstsuche
  (lambda (fkt xl xr)
    (let ([x (/ (+ xl xr) 2)])
     (cond
      [(< (- xr xl) eps) x]
      [(< (abs (fkt x)) eps) x]
      [(< (* (fkt xl) (fkt x)) 0) (nstsuche fkt xl x)]
      [else (nstsuche fkt x xr)]))))

(define nstliste
  (lambda (fkt xl xr dx)
    (cond
      [(>= xl xr) '()]
      [(<= (* (fkt xl) (fkt (+ xl dx))) 0)
       (let ([x0 (nstsuche fkt xl (+ xl dx))])
         (if (<= (abs (fkt x0) ) x-eps)
             (cons (runden x0 anz) (nstliste fkt (+ xl dx) xr dx))
             (nstliste fkt (+ xl dx) xr dx)))]
      [else (nstliste fkt (+ xl dx) xr dx)])))

(define funk (lambda (x) (+ (expt x 3) (* 6 (expt x 2)) (* x 11) 6)))
(define f  '(+ (expt x 3) (* 6 (expt x 2)) (* x 11) 6))
(nstliste funk -10 10 1e-5)
(nstliste (fkt f) -10 10 1e-5)
(define fkt
  (lambda (term var)
    (eval (list 'lambda (list var) term) ns)))