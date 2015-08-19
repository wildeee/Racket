#lang racket

(require rackunit)
(require rackunit/text-ui)

;----------- INÍCIO TRECHOS DE CÓDIGO COMUNS ENTRE EXERCÍCIOS ------------;

(struct bin-tree (l r val) #:transparent)


;----------- FIM TRECHOS DE CÓDIGO COMUNS ENTRE EXERCÍCIOS ------------;


;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.1

;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.2

;;Qualquer lista -> lista
;;Devolve uma lista que é como lst, mas sem as ocorrências de a

(define remove-todos-tests
  (test-suite
   "remove-todos tests"
   (check-equal? (remove-todos empty 3) empty)
   (check-equal? (remove-todos (list 1 2 3 4 5) 3) (list 1 2 4 5))
   (check-equal? (remove-todos (list 1 2 3 4 4 4 5) 4) (list 1 2 3 5))
   (check-equal? (remove-todos (list 1 2 3 4 4 4 5) 9) (list 1 2 3 4 4 4 5))
   (check-equal? (remove-todos (list 3 3 3 3 3 3) 3) empty)))

(define (remove-todos lst a)
    (cond
         [(empty? lst) empty]
         [else
             (cond
                 [(equal? (first lst) a) (remove-todos (rest lst) a)]
                 [else (cons (first lst) (remove-todos (rest lst) a))]
             )
         ]
    )
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.7

;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.8

;;Qualquer lista -> elemento
;;Devolve o último elemento da lista
;;Obs.: Erro gerado se a lista for vazia


(define return-last-tests
  (test-suite
   "return-last tests"
   (check-exn exn:fail? (thunk (return-last empty)))
   (check-equal? (return-last (list 1 2 3 4 5)) 5)
   (check-equal? (return-last (list 2)) 2)
  ))

(define (return-last lst)
  (define (return-last-not-empty lista)
       (cond
            [(equal? (length lista) 1) (first lista)]
            [else (return-last-not-empty (rest lista))]
       )
  )
    (cond
         [(empty? lst) (error "Lista vazia")]
         [else (return-last-not-empty lst)]
    )
)


;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.11

;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.12

;;Qualquer lista -> lista
;;Devolve uma lista tal como lst, porém com apenas uma ocorrência dos elementos repetidos consecutivos

(define remove-duplicates-tests
  (test-suite
   "remove-duplicates tests"
   (check-equal? (remove-duplicates (list 1 2 3 4 5)) (list 1 2 3 4 5))
   (check-equal? (remove-duplicates (list 1 1 2 2 3)) (list 1 2 3))
   (check-equal? (remove-duplicates empty) empty)
   (check-equal? (remove-duplicates (list 1 1 1 1 1)) (list 1))
  ))


(define (remove-duplicates lst)
  (define (remove-duplicates-recursive lista last)
       (cond
            [(empty? lista) empty]
            [(equal? (first lista) last) (remove-duplicates-recursive (rest lista) last)]
            [else (cons (first lista) (remove-duplicates-recursive (rest lista) (first lista)))]
       )
  )
    (cond
        [(empty? lst) empty]
        [else (cons (first lst)(remove-duplicates-recursive (rest lst) (first lst)))]
    )
)


;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.13

;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.15

;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.16

;;Qualquer bin-tree -> booleano
;;Devolve #t se bin for uma árvore binária de busca, #f caso contrário

(define search-bin-tree?-tests
  (test-suite
   "search-bin-tree? tests"
   (check-equal? (search-bin-tree? (bin-tree (bin-tree
                                              (bin-tree empty empty 1)
                                              (bin-tree
                                               (bin-tree empty empty 4)
                                               (bin-tree empty empty 7)
                                               6)
                                              3)
                                             (bin-tree
                                              empty
                                              (bin-tree
                                               (bin-tree empty empty
                                                13) empty
                                               14)
                                              10)
                                             8))
                 #t)
; Adaptado, de http://videos.web-03.net/artigos/Higor_Medeiros/ArvoreBinaria/ArvoreBinaria1.jpg

   (check-equal? (search-bin-tree? empty) #f)
   (check-equal? (search-bin-tree? (bin-tree (bin-tree empty empty 10) (bin-tree empty empty 14) 8)) #f)
  ))

(define (search-bin-tree? bin)
    #f ;stub implementation
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.17

;;
;;
;;

