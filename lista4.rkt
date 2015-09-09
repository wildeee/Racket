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

;;Número -> Número
;;Devolve o resultado da operação escolhida (+ - ou *)

(define primitive-tests
  (test-suite 
    "primitive-tests"
    (check-equal? (plus 4 3) 7)
    (check-equal? (less 4 3) 1)
    (check-equal? (times 4 3) 12)
    (check-equal? (times 0 3) 0)
    (check-equal? (times 4 0) 0)
    (check-equal? (times 4 1) 4)
    (check-equal? (times 1 4) 4)
    (check-not-equal? (plus 2 3) 7)
    (check-not-equal? (less 2 3) 1)
    (check-not-equal? (times 1 3) 12)
  )
)

(define (plus n1 n2)
    (cond
         [(zero? n2) n1]
         [else (plus (add1 n1) (sub1 n2))]
    )
)

(define (less n1 n2)
    (cond
         [(zero? n2) n1]
         [else (less (sub1 n1) (sub1 n2))]
    )
)
(define (times n1 n2)
  (define (internal-times soma n)
       (cond
            [(zero? (sub1 n)) soma]
            [else (internal-times (plus soma n1) (sub1 n))]
       )
  )
   (cond
        [(zero? n2) 0]
        [(zero? n1) 0]
        [(zero? (sub1 n2)) n1]
        [else (internal-times (plus n1 n1) (sub1 n2))]
    )
)

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

;;Número -> booleano
;;Retorna #t se o núnero for perfeito (soma de seus divisores próprios é igual a ele)


(define perfeito?-tests
  (test-suite 
    "perfeito?-tests"
    (check-equal? (perfeito? 6) #t)
    (check-equal? (perfeito? 28) #t)
    (check-equal? (perfeito? 8) #f)
  )
)

(define (perfeito? num)
  (define (soma-array lst)
        (cond
          [(empty? (rest lst)) (first lst)]
          [else (+ (first lst) (soma-array (rest lst)))]
        )
   )
  (define (divisores n)
      (define (internal-divisores n)
           (cond
                [(>= n num) empty]
                [else
                 (cond
                      [(= 0 (modulo num n)) (cons n (internal-divisores (add1 n)))]
                      [else (internal-divisores (add1 n))]
                 )
                 
                ]
           )
      )
    (internal-divisores 1)
  )
    (= num (soma-array (divisores num)))
)



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


(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

(executa-testes
                fatorial-tests
                primitive-tests
                e-par?-tests
                e-impar?-tests
                perfeito?-tests
                conta-primo-tests 
                )