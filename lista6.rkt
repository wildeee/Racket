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
  )
)

(define (concatena . lst)
    (cond
         [(empty? (rest lst)) (first (first lst))]
         [else (append (first lst) (concatena (rest lst)))] ; Problema aqui. Em vez de jogar os elementos do resto da lista, está sendo jogado o resto da lista inteira.
    )
)


;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 6.11

;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 6.12

;;
;;
;;