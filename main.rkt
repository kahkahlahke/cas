#lang racket
(require racket/trace)
(define super-ersetzen
  (lambda (el1 ls el2)
    (if (null? ls)
        ls
        (if (list? (car ls))
            (cons (super-ersetzen el1 (car ls) el2) (super-ersetzen el1 (cdr ls) el2))
            (if (equal? el1 (car ls))
                (cons el2 (super-ersetzen el1 (cdr ls) el2))
                (cons (car ls) (super-ersetzen el1 (cdr ls) el2))
                )
            ))))

(super-ersetzen 'x '(lambda (x) (+ (* 2 x) y)) 'z)


(define ln log)
(define e (exp 1))
;Differentiationsregeln
(define ableitung
  (lambda (ausdr var)
    (cond
      [(number? ausdr) 0]
      [(symbol? ausdr) (if (eqv? ausdr var) 1 0)]
      [(= (length ausdr) 2)
       (let ([op (car ausdr)] [term1 (cadr ausdr)])
         (case op
           [(+) 
            (list '+ (ableitung term1 var))]
           [(sqr)
            (list '* 2
                  (list '* term1 (ableitung term1 var)))]
           [(sin)
            (list '*
                  (list 'cos term1)
                  (ableitung term1 var))]
           [(cos)
            (list '*
                  (list '* -1 (list 'sin term1))
                  (ableitung term1 var))]
           [(sqrt)
            (list '* 
                  (list '/ 1  (list '* 2 (list 'sqrt term1)))
                  (ableitung term1 var))]
           [else
            'Unbekannt!]))]
      [(= (length ausdr) 3)
       (let ([op (car ausdr)] [term1 (cadr ausdr)] [term2 (caddr ausdr)])
         (case op
           [(+) 
            (list '+ (ableitung term1 var) (ableitung term2 var))]
           [(-) 
            (list '- (ableitung term1 var) (ableitung term2 var))]
           [(*)
            (list '+
                  (list '* (ableitung term1 var) term2)
                  (list '* term1 (ableitung term2 var)))]
           [(/)
            (list '/
                  (list '- (list '* term1 (ableitung term2 var)) (list '* (ableitung term1 var) term2))
                  (list '^ term2 2))]
	   
           [else
            'Unbekannt!]))]
      [else
       'Unbekannt!])))

(define vereinfache
  (lambda (ausdr)
    (letrec
        ([help-unop
          (lambda (op term1)
            (case op
              [(+ *) (vereinfache term1)]
              [(/) (list '/ 1 (vereinfache term1))]
              [(-) (list '- (vereinfache term1))]
              [(sqrt) (cond
                        [(number? term1) (sqrt term1)]
                        [(and (list? term1) (equal? (car term1) 'sqr)) (cadr (vereinfache term1))]
                        [else (list 'sqrt (vereinfache term1))])]
              [(sqr) (cond
                        [(and (number? term1) (< 0 term1)) 'ungueltig]
                        [(number? term1) (sqr term1)]
                        [(and (list? term1) (equal? (car term1) 'sqrt)) (cadr (vereinfache term1))]
                        [else (list 'sqr (vereinfache term1))])]
          
              ;hier stehen weitere Vereinfachungsregeln für unäre Operationen
             [else
              (list op (vereinfache term1))]))]
         [help-binop
          (lambda (op term1 term2)
            (case op
              [(+) (cond 
                     [(eqv? term1 0) (vereinfache term2)]
                     [(eqv? term2 0) (vereinfache term1)]
                     [(equal? term1 term2) (list '* 2 (vereinfache term1))]
                     [(and (number? term1) (number? term2)) (+ term1 term2)]
                     [else
                      (list '+ (vereinfache term1) (vereinfache term2))])]
              [(*) (cond
                     [(eqv? term1 0) 0]
                     [(eqv? term2 0) 0]
                     [(eqv? term1 1) (vereinfache term2)]
                     [(eqv? term2 1) (vereinfache term1)]
                     [(equal? term1 term2) (list '^ (vereinfache term1) 2)]
                     [(and (number? term1) (number? term2)) (* term1 term2)]
                     [else (list '* (vereinfache term1) (vereinfache term2))])]
              [(-) (cond 
                     [(eqv? term1 0) (- (vereinfache term2))]
                     [(eqv? term2 0) (vereinfache term1)]
                     [(equal? term1 term2) 0]
                     [(and (number? term1) (number? term2)) (- term1 term2)]
                     [else
                      (list '- (vereinfache term1) (vereinfache term2))])]
              [(/) (cond
                     [(eqv? term1 0) 0]
                     [(eqv? term2 0) 'ungueltig]
                     [(eqv? term2 1) (vereinfache term1)]
                     [(equal? term1 term2) 1]
                     [(and (number? term1) (number? term2)) (/ term1 term2)]
                     [else (list '/ (vereinfache term1) (vereinfache term2))])]
              [(expt ^) (cond
		     [(and (eqv? term1 0) (eqv? term2 0)) 'ungueltig]
                     [(eqv? term1 0) 0]
                     [(eqv? term2 0) 1]
                     [(eqv? term1 1) 1]
                     [(eqv? term2 1) (vereinfache term1)]
                     [(and (number? term1) (number? term2)) (^ term1 term2)]
                     [else (list '^ (vereinfache term1) (vereinfache term2))])]
              ;hier stehen weitere Vereinfachungsregeln für binäre Operationen
              [else
               ausdr]))]) 
      (cond
        [(or (number? ausdr) (symbol? ausdr)) ausdr]
        [(= (length ausdr) 2) (help-unop (car ausdr) (cadr ausdr))]
        [(= (length ausdr) 3)
         (help-binop (car ausdr) (cadr ausdr) (caddr ausdr))]
        [else
         ausdr]))))
; I have no fucking clue what in the motherfucking hell this is
; I hope I die of croniavirus
(define re-n-ab-ver-voll
  (lambda (g var n)
    (vereinfache-vollstaendig (n-ab (reduziere g) var n))))

(define vereinfache-vollstaendig
  (lambda (term)
    (let ([term-neu (vereinfache term)])
      (if (not (equal? term term-neu))
          (vereinfache-vollstaendig term-neu)
          term-neu))))

(vereinfache-vollstaendig '(sqrt (* 2 )) )
