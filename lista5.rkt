#lang racket

(require rackunit)
(require rackunit/text-ui)

;----------- INÍCIO TRECHOS DE CÓDIGO COMUNS ENTRE EXERCÍCIOS ------------;



;----------- FIM TRECHOS DE CÓDIGO COMUNS ENTRE EXERCÍCIOS ------------;

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 5.1

;; Número, Número -> Boolean
;; Devolve verdadeiro ou falso de acordo com a operação.

(define >?-tests
  (test-suite
    ">? tests"
    (check-equal? (>? 0 0) #f)
    (check-equal? (>? 0 2) #f)
    (check-not-equal? (>? 2 0) #f)
  )
)

(define >?-tests
  (test-suite
    ">? tests"
    (check-equal? (>? 0 0) #f)
    (check-equal? (>? 0 2) #f)
    (check-not-equal? (>? 2 0) #t)
  )
)

(define (>? a b)
  (cond
    [(zero? a) #f]
    [(zero? b) #t]
    [else (>? (sub1 a) (sub1 b))]
  )
)

(define (>=? a b)
  (cond
    [(= a b) #t] 
    [(zero? a) #f]
    [(zero? b) #t]
    [else (>? (sub1 a) (sub1 b))]
  )
)

(define (<? a b)
  (cond
    [(zero? b) #f]
    [(zero? a) #t]    
    [else (<? (sub1 a) (sub1 b))]
  )
)

(define (<=? a b)
  (cond
    [(= a b) #t]
    [(zero? b) #f]
    [(zero? a) #t]    
    [else (<=? (sub1 a) (sub1 b))]
  )
)

(define (=? a b)
  (cond
    [(= a b) #t]
    [else #f]
  )
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 5.2

;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 5.3

;; Lista, Número -> Lista
;; Devolve a mesma lista lst recebida sem os n primeiros elementos.

(define drop-tests
  (test-suite
    "drop tests"
    (check-equal? (drop (list 1 2 3) 0) '(1 2 3))
    (check-equal? (drop (list 1 2 3 4 5 6) 3) '(4 5 6))
    (check-not-equal? (drop (list 1 2 3) 1) '(1 2 3))
  )
) 

(define (drop lst n)
  (cond
    [(zero? n) lst]
    [(empty? lst) empty]
    [else (drop (rest lst) (sub1 n))]
  )
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 5.4

;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 5.5

;; Lista, Número, Número -> Lista
;; Devolve a mesma lista de entrada mas com um dado elemento inserido em uma dada posição.

(define insert-at-tests
  (test-suite
    "insert-at tests"
    (check-equal? (insert-at (list 1 2 3) 4 3) '(1 2 3 4))
    (check-equal? (insert-at (list 1 2 3) 0 0) '(0 1 2 3))
    (check-not-equal? (insert-at (list 1 2 3) 4 2) '(1 2 3 4))
  )
) 

(define (insert-at lst n pos)
  (cond
    [(zero? pos) (append (list n) lst)] 
    [(empty? lst) n]    
    [else (cons (first lst) (insert-at (rest lst) n (sub1 pos)))]
  )
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 5.6

;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 5.7

;; Lista, Número -> Lista
;; Devolve a mesma lista de entrada mas com os elementos rotacionados n posições a esquerda.

(define rotate-left-tests
  (test-suite
    "rotate-left tests"
    (check-equal? (rotate-left (list 10 20 30 40 50) 2) '(30 40 50 10 20))
    (check-equal? (rotate-left (list 1 2 3) 0) '(1 2 3))
    (check-not-equal? (rotate-left (list 1 2 3) 2) '(1 2 3))
  )
) 

(define (rotate-left lst n)
  (cond
    [(empty? lst) empty]
    [(zero? n) lst]
    [else (rotate-left (append (rest lst) (list (first lst))) (sub1 n))]
  )
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 5.8

;;
;;
;;


;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 5.9

;; Lista, Lista -> Lista
;; Devolve uma nova lista com os elementos das duas listas de entrada em ordem crescente.

(define merge-tests
  (test-suite
    "merge tests"
    (check-equal? (merge (list 3 7 12) (list 2 4 5)) '(2 3 4 5 7 12))
    (check-equal? (merge (list 1 2 3) (list )) '(1 2 3))
    (check-not-equal? (merge (list 1 2 3) (list 5 7 2)) '(1 2 3))
  )
)

(define (insere-ordenado n lst)
  (cond
    [(empty? lst) (list n)]
    [(n . < . (first lst)) (cons n lst)]
    [else (cons (first lst) (insere-ordenado n (rest lst)))]
  )
)

(define (ordem-cresc lst)
  (cond
    [(empty? lst) empty]
    [else (insere-ordenado (first lst) (ordem-cresc (rest lst)))]
  )
)


(define (merge lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [else (ordem-cresc (append lst1 lst2))]
  )
)