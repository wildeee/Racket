#lang racket

(require rackunit)
(require rackunit/text-ui)

;----------- INÍCIO TRECHOS DE CÓDIGO COMUNS ENTRE EXERCÍCIOS ------------;



;----------- FIM TRECHOS DE CÓDIGO COMUNS ENTRE EXERCÍCIOS ------------;


;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 7.1 -> 3.2

;;Qualquer lista -> lista
;;Devolve uma lista que é como lst, mas sem as ocorrências de a (usando acumuladores).

(define remove-todos-acc-tests
  (test-suite
    "remove-todos-acc tests"
    (check-equal? (remove-todos-acc empty 1) empty)
    (check-equal? (remove-todos-acc (list 1) 1) empty)
    (check-equal? (remove-todos-acc (list 1 2 3 4 4) 4) (list 1 2 3))
    (check-not-equal? (remove-todos-acc (list 1 2 2 4 4) 2) (list 1 2 3))
   )
)

(define (remove-todos-acc lst n)
  (define (funcao lst0 n0 acc)
    (cond
      [(empty? lst0) acc]
      [(equal? n0 (first lst0)) (funcao (rest lst0) n0 acc)]
      [else (funcao (rest lst0) n0 (append acc (list(first lst0))))]
	)
  )
  (funcao lst n empty)
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 7.1 -> 3.7

;; Lista -> Lista
;; Recebe lista com números naturais e devolve a mesma lista apenas com números ímpares (usando acumuladores).

(define impares-acc-tests
  (test-suite
    "impares-acc tests"
    (check-equal? (impares-acc empty) empty)
    (check-equal? (impares-acc (list 1)) '(1))
    (check-equal? (impares-acc (list 2)) empty)
    (check-equal? (impares-acc (list 1 2 3 4 5 6)) '(1 3 5))
    (check-not-equal? (impares-acc (list 1 1 2 1 1)) '(1 2))
  )
)

(define (par? x)
    (equal? (modulo x 2) 0)
)

(define (impares-acc lst)
  (define (funcao lst0 acc)
    (cond
      [(empty? lst0) acc]
      [(par? (first lst0)) (funcao (rest lst0) acc)]
      [else (funcao (rest lst0) (append acc (list (first lst0))))]
    )
  )
  (funcao lst empty)
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 7.1 -> 3.9

;; Lista -> Inteiro
;; Devolve o maior número de uma lista (usando acumuladores).

(define maximo-acc-tests
  (test-suite
    "maximo-acc tests"
    (check-not-equal? (maximo-acc (list 1 2 3)) 2)
    (check-equal? (maximo-acc (list )) empty)
    (check-equal? (maximo-acc (list 1 3 2 5)) 5)
    (check-equal? (maximo-acc (list 5)) 5)
  )
)

(define (max a b)
  (if (>= a b) a b)
)

(define (maximo-acc lst)
  (define (funcao lst0 max-acc)
    (cond
      [(empty? lst0) max-acc]
      [else (funcao (rest lst0) (max max-acc (first lst0)))]
    )
  )
  (cond
    [(empty? lst) empty]
    [else (funcao lst (first lst))]
  )
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 7.2 -> 4.1

;; Número -> Número
;; Devolve o fatorial do número n (usando acumuladores).

(define fatorial-acc-tests
  (test-suite
    "fatorial-acc tests"
    (check-equal? (fatorial-acc 5) 120)
    (check-equal? (fatorial-acc 0) 1)
    (check-not-equal? (fatorial-acc 2) 3)
  )
)

(define (fatorial-acc n)
  (define (funcao x acc)
    (cond
      [(<= x 1) acc]
      [else (funcao (sub1 x) (* acc x))]
    )
  )
  (funcao n 1)
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 7.2 -> 4.5

;; Intervalo -> Número
;; Devolve a quantidade de números primos de um intervalo (ini e fim, utilizando acumuladores).

(define conta-primo-acc-tests
  (test-suite
    "conta-primo-acc tests"
    (check-equal? (conta-primo-acc 1 5) 4)
    (check-equal? (conta-primo-acc 3 10) 3)
    (check-not-equal? (conta-primo-acc 10 20) 3)
  )
)

(define (fator? x n)
  (cond
    [(= x 1) #f]
    [(zero? (modulo n x)) #t]
    [else (fator? (sub1 x) n)]
  )
)

(define (primo? n)
  (cond
    [(= n 1) #t]
    [else (not(fator? (sub1 n) n))]
  )
)

(define (conta-primo-acc i f)
  (define (funcao i f acc)
    (cond
      [(> i f) acc]
      [(primo? i) (funcao (add1 i) f (add1 acc))]
      [else (funcao (add1 i) f acc)]
    )
  )
  (funcao i f 0)
)


(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

(executa-testes
                remove-todos-acc-tests
                impares-acc-tests
                maximo-acc-tests
                fatorial-acc-tests
                conta-primo-acc-tests
                )