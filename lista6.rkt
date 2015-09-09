#lang racket

(require rackunit)
(require rackunit/text-ui)

;----------- INÍCIO TRECHOS DE CÓDIGO COMUNS ENTRE EXERCÍCIOS ------------;



;----------- FIM TRECHOS DE CÓDIGO COMUNS ENTRE EXERCÍCIOS ------------;


;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 6.8

;; Função, Lista -> Número
;; Devolve o número de elementos da lista que são avaliados com valor #t pela função passada por parâmetro

(define cont-tests
  (test-suite
    "cont tests"
    (check-equal? (cont positive? (list -1 2 3)) 2)
    (check-equal? (cont positive? (list -1 -2 -3)) 0)
    (check-equal? (cont positive? (list -1 2 3 4 5)) 4)
    (check-equal? (cont positive? (list -1 2 3 -4 5)) 3)
    (check-equal? (cont negative? (list -1 2 3 4 5 6 7 8 9)) 1)
    (check-equal? (cont negative? (list 1 2 3 4 5 6 7 8 9)) 0)
    (check-equal? (cont (λ (num) (<= num 5)) (list 1 2 3 4 5 6 7 8 9)) 5)
  )
)

(define (cont condition lst)
   (define (internal-cont lst soma)
        (cond
            [(empty? lst) soma]
            [else
                 (cond
                      [(condition (first lst)) (internal-cont (rest lst) (add1 soma))]
                      [else (internal-cont (rest lst) soma)]
                  )
                 
            ]
         )
   )

  (internal-cont lst 0)
)


;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 6.10

;; Listas -> lista
;; Devolve todos os parâmetros concatenados

(define concatena-tests
  (test-suite
    "concatena tests"
    (check-equal? (concatena (list -1 2 3) (list 1 2 3)) '(-1 2 3 1 2 3))
    (check-equal? (concatena (list 1 2 3)) '(1 2 3))
    (check-equal? (concatena (list 1 2 3) (list 4 5 6) (list 7 8 9)) '(1 2 3 4 5 6 7 8 9))
    (check-equal? (concatena (list -1 2 3) (list 3 2 1) ) '(-1 2 3 3 2 1))
    (check-equal? (concatena empty empty ) empty)
    (check-equal? (concatena) empty)
  )
)

(define (concatena . lst)
    (foldr append empty lst)    
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 6.11

;; function, listas de mesmo tamanho -> (resultado dependente da function passada)
;; Função map pré definida. Retorna o resultado da function passada operando uma posição de cada vez das listas passadas.

;(define mapeia-tests
;  (test-suite
;    "mapeia tests"
;    (check-equal? (mapeia + (list 1 2 3) (list 4 5 6) (list 7 8 9)) '(12 15 18))
;    (check-equal? (mapeia list (list 1 2 3) (list 4 5 6) (list 7 8 9)) '((1 4 7) (2 5 8) (3 6 9)))
;    (check-equal? (mapeia * (list 1 2 3) (list 4 5 6) (list 7 8 9)) '(28 80 162))
;  )
;)

;(define (mapeia callback . lst) ;'((1 2 3) (4 5 6) (7 8 9))
;
;    (cond
;         [(empty? lst) empty]
;         [else
;              (cons (mapeia callback lst))
;              ;(cons (callback (first lst)) (mapeia callback (rest lst)))
;         ]
;    )
;)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 6.12

;; Números -> List de números
;; Devolve uma lista de elementos de mesma paridade do primeiro parâmetro.

(define paridade-tests
  (test-suite
   "Exercício 6.12"
   (check-equal? (paridade 1 2 3) (list 1 3))
   (check-equal? (paridade 4) (list 4))
   (check-equal? (paridade 2 3 4 5 8 12) (list 2 4 8 12))
   ))

(define (paridade n . ns)
  (cond
    [(even? n) (cons n (filter even? ns))]
    [else (cons n (filter odd? ns))]))



(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

(executa-testes
                cont-tests
                concatena-tests
                paridade-tests
                )