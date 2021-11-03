#lang racket

(define ^ expt)
(define ln log)
(define e (exp 1))


(define derivative
  (lambda (term var)
    (simplify (cond
      [(number? term) 0]
      [(symbol? term) (if (eqv? term var) 1 0)]
      [(list? term)
       (case (length term)
         [(0) "Unknown!"]
         [(1) (single term var)]
         [(2) (double term var)]
         [(3) (triple term var)]
         [else (derivative (convert term) var)]
         )]
      [else "Unknown!"]
      ))))

(define single
  (lambda (term var)
    (let ([term1 (car term)])
    (cond
      [(number? term1) 0]
      [(symbol? term1) (if (eqv? term var) 1 0)]
      [else "Unknown!"]
      ))))

(define double
  (lambda (term var)
    (let
        ([op (car term)]
         [term1 (cadr term)])
      (case op
        [(+) (list '+ (derivative term1 var))]
        [(-) (list '- (derivative term1 var))]
        [(*) (list '* (derivative term1 var))]
        [(/) (list '/ 1 (derivative term1 var))]
        [(sin) (list '* (list 'cos term1) (derivative term1 var))]
        [(cos) (list '* (list '- (list 'sin term1)) (derivative term1 var))]
        [(tan) (list '/ (derivative term1 var) (list '^ (list 'cos term1) 2))]
        [(arcsin) (list '/ (derivative term1 var) (list 'sqrt (list '- 1 (list '^ term1 2))))]
        [(arccos) (list '/ (- (derivative term1 var)) (list 'sqrt (list '- 1 (list '^ term1 2))))]
        [(arctan) (list '/ (derivative term1 var) (list '+ 1 (list '^ term1 2)))]
        [(sqrt) (derivative (list '^ term1 0.5) var)]
        [(ln) (list '/ (derivative term1 var) term1)]
        [else "Unknown!"]
        )
    )))

(define triple
  (lambda (term var)
    (let
        ([op (car term)]
         [term1 (cadr term)]
         [term2 (caddr term)])
      (case op
        [(+) (list '+ (derivative term1 var) (derivative term2 var))]
        [(-) (list '- (derivative term1 var) (derivative term2 var))]
        [(*) (list '+ (list '* (derivative term1 var) term2 ) (list '* (derivative term2 var) term1 ))]
        [(/) (list '/ (list '- (list '* (derivative term1 var) term2) (list '* (derivative term2 var) term1)) (list '* term2 term2))]
        [(^) (list '* (list '+ (list '/ (list '* (derivative term1 var) term2) term1) (list '* (list 'ln (abs term1)) (derivative term2 var))) (list '^ term1 term2))]
        [(log) (list '- (list '/ (derivative term1 var) (list '* (list 'ln term2) term1)) (list '/ (list '* (derivative term2 var) (list 'ln term1)) (list '* term2 (list '^ (list 'ln term2) 2))))]
        [else "Unknown!"]
        )
    )))


(define convert
  (lambda (term)
    (if (and (list? term)
             (> (length term) 3)
             (symbol? (car term)))
        (let ([op (car term)]
          [term1 (cdr term)])
      (case op
        [(+ *) (list op (car term1) (convert (cons op (cdr term1))))]
        [(-) (list '- (car term1) (convert (cons '+ (cdr term1))))]
        [(/) (list '/ (car term1) (convert (cons '* (cdr term1))))]
        [else "Unknown!"]))
    term
    )))

(define simplify
  (lambda (term)
    (letrec
        ([help-single
          (lambda (op term1)
            (case op
              [(+ *) (simplify term1)]
              [(/) (list '/ 1 (simplify term1))]
              [(-) (list '- (simplify term1))]
              [else
               (list op (simplify term1))]))]
         [help-double
          (lambda (op term1 term2)
            (case op
              [(+) (cond 
                     [(eqv? term1 0) (simplify term2)]
                     [(eqv? term2 0) (simplify term1)]
                     [(equal? term1 term2) (list '* 2 (simplify term1))]
                     [(and (number? term1) (number? term2)) (+ term1 term2)]
                     [else
                      (list '+ (simplify term1) (simplify term2))])]
              [(-) (cond 
                     [(eqv? term1 0) (list '- (simplify term2))]
                     [(eqv? term2 0) (simplify term1)]
                     [(equal? term1 term2) 0]
                     [(and (number? term1) (number? term2)) (- term1 term2)]
                     [else
                      (list '- (simplify term1) (simplify term2))])]
              [(*) (cond 
                     [(eqv? term1 0) 0]
                     [(eqv? term2 0) 0]
                     [(eqv? term1 1) term2]
                     [(eqv? term2 1) term1]
                     [(equal? term1 term2) (list '^ term1 2)]
                     [(and (number? term1) (number? term2)) (* term1 term2)]
                     [else
                      (list '* (simplify term1) (simplify term2))])]
              [(/) (cond 
                     [(eqv? term1 0) 0]
                     [(eqv? term2 0) 'Unbekannt!]
                     [(equal? term1 term2) 1]
                     [(and (number? term1) (number? term2)) (/ term1 term2)]
                     [else
                      (list '/ (simplify term1) (simplify term2))])]
              [(expt) (cond
                        [(and (eqv? term1 0) (eqv? term2 0)) "Unknown!"]
                        [(eqv? term1 0) 0]
                        [(eqv? term2 0) 1]
                        [(and (number? term1) (number? term2)) (expt term1 term2)]
                        [else
                         (list 'expt (simplify term1) (simplify term2))])]
              [else
               term]))]) 
      (cond
        [(or (number? term) (symbol? term)) term]
        [(= (length term) 2) (help-single (car term) (cadr term))]
        [(= (length term) 3)
         (help-double (car term) (cadr term) (caddr term))]
        [else
         term]))))

(define simplify-full
  (lambda (term)
    (let ([term1 (simplify term)])
      (if (equal? term term1)
          term1
      (simplify-full term1))
      )))

(define supermember
  (lambda (lst el)
    (if
     (eq? #f (member el (flatten lst)))
     #f
     #t
  )))

(define solve
  (lambda (term var val)
    (cond
      [(number? term) term]
      [(number? (car term)) (car term)]
      [(eqv? var term) val]
      [(and (eqv? 1 (length term))(eqv? var (car term))) val]
      [(eqv? "Unknown!" term) term]
      [(supermember "Unknown!" term) "Unknown!"]
      [else
       ((eval (list 'lambda (list var) term)) val)])))
