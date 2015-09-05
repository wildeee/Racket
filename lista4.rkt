#lang racket

(require rackunit)
(require rackunit/text-ui)
(require math/number-theory)

;----------- INÍCIO TRECHOS DE CÓDIGO COMUNS ENTRE EXERCÍCIOS ------------;



;----------- FIM TRECHOS DE CÓDIGO COMUNS ENTRE EXERCÍCIOS ------------;


;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 4.1

;; Número -> Número
;; Devolve o fatorial do número n

(define fatorial-tests
  (test-suite 
    "fatorial-tests"
    (check-equal? (fatorial 5) 120)
    (check-equal? (fatorial 0) 1)
    (check-not-equal? (fatorial 2) 3)
  )
)

(define (fatorial n)
  (cond
     [(zero? n) 1] 
     [else (* n (fatorial (- n 1 )))]
  )
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 4.2

;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 4.3

;; Número -> Boolean
;; Devolve true se a é par, falso caso contrário.

(define e-par?-tests
  (test-suite
    "e-par? tests"
    (check-equal? (e-par? 0) #t)
    (check-equal? (e-par? 1) #f)
    (check-equal? (e-par? 2) #t)
  )
)
   
(define e-impar?-tests
  (test-suite
    "e-impar? tests"
    (check-equal? (e-impar? 0) #f)
    (check-equal? (e-impar? 1) #t)
    (check-equal? (e-impar? 2) #f)
  )
)   

(define (e-par? a)
  (cond
    [(zero? a) #t]
    [else (e-impar? (sub1 a))]
  )
)

(define (e-impar? a)
  (cond
    [(zero? a) #f]
    [else (e-par? (sub1 a))]
  )
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 4.4

;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 4.5

;; Intervalo -> Número
;; Devolve a quantidade de números primos de um intervalo (ini e fim).

(define conta-primo-tests
  (test-suite
    "conta-primo tests"
    (check-equal? (conta-primo 1 5) 2)
    (check-equal? (conta-primo 3 10) 3)
    (check-not-equal? (conta-primo 10 20) 3)
  )
) 

(define (conta-primo ini fim)
  (length (filter prime? (range ini fim)))
)
