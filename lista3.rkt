#lang racket

(require rackunit)
(require rackunit/text-ui)

;----------- INÍCIO TRECHOS DE CÓDIGO COMUNS ENTRE EXERCÍCIOS ------------;

(struct bin-tree (l r val) #:transparent)
(struct arvore-bin (v esq dir) #:transparent)


;----------- FIM TRECHOS DE CÓDIGO COMUNS ENTRE EXERCÍCIOS ------------;


;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.1

;; Lista -> Boolean
;; Verifica se um determinado elemento esta em uma lista

(define contem-tests
  (test-suite 
    "contem-tests"
    (check-equal? (contem 3 empty) #f)
    (check-equal? (contem 3 (list 1 2 3 4)) #t)
    (check-equal? (contem 3 (list 1 2 4 5)) #f)
  )
)

(define (contem a lst)
  (cond 
    [(empty? lst) #f]
    [(equal? a (first lst)) #t] 
    [else (contem a (rest lst))]
  )
)

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

;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.3

;; Lista -> Lista
;; Devolve a mesma lista de entrada mais com a no final.

(define add-fim-tests
  (test-suite
    "add-fim tests"
    (check-equal? (add-fim 1 empty) (list 1))
    (check-equal? (add-fim 3 (list 5)) (list 5 3))
    (check-equal? (add-fim 8 (list 2 5)) (list 2 5 8))
    (check-exn exn:fail? (add-fim 1 empty) (list 2))
  )
)

(define (add-fim a lst)
  (cond 
    [(empty? lst) (list a)]
    [else (cons (first lst) (add-fim a (rest lst)))]
  )
)

;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.4

;; Lista -> Lista
;; Devolva uma lista com os mesmos elementos de lst mas em ordem reversa.
;; OBS: Podíamos utilizar também a função reverse.

(define inverte-tests
  (test-suite
    "inverte tests"
    (check-equal? (inverte empty) empty)
    (check-equal? (inverte (2)) (2))
    (check-equal? (inverte (2 8 9)) (9 8 2))
  )
)

(define (inverte lst)
  (cond
    [(empty? lst) empty]
    [else (append (inverte (rest lst)) (list (first lst)))]
;;    [else (reverse lst)] 
  )
)

;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.5

;; Lista -> Boolean
;; Devolve verdadeiro se lst é palíndromo, e falso caso contrario

(define palindromo-tests
  (test-suite
    "palindromo tests"
    (check-equal? (palindromo empty) #t)
    (check-equal? (palindromo (2)) #t)
    (check-equal? (palindromo (1 2)) #f)
  )
)

(define (palindromo lst)
  (equal? lst (inverte lst))
)

;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.6

;; Lista Número -> Lista
;; Devolve uma nova lista com n somado a cada elemento de lst.
(define lista-add-num-tests
  (test-suite
    "lista-add-num tests"
    (check-equal? (lista-add-num empty 3) empty)
    (check-equal? (lista-add-num (list 2) 4) (list 6))
    (check-equal? (lista-add-num (list 1 2) 5) (list 6 7))
    (check-equal? (lista-add-num (list 5 -2 3) -2) (list 3 -4 1))
  )
)

(define (lista-add-num lst n)
  (cond
    [(empty? lst) empty]
    [else (cons (+ n (first lst)) (lista-add-num (rest lst) n))]
  )
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.7

;; Lista -> Lista
;; Recebe lista com números naturais e devolve a mesma lista apenas com números ímpares

(define (impares-tests)
  (test-suite
    "impares tests"
    (check-equal? (impares empty) empty)
    (check-equal? (impares (list 1 2 3 4 5)) (list 1 3 5))
    (check-exn exn:fail? (impares (list 1 2 3 4 5)) (list 2 4))
  )
)
               
(define (impares lst)
  (cond
    [(empty? lst) empty]
    [(even? (first lst)) (impares(rest lst))]
    [else (append (list (first lst)) (impares (rest lst)))]
  )
)

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
;; Exercício 3.9

;; Lista -> Inteiro
;; Devolve o maior número de uma lista

(define maximo-tests
  (test-suite
    "maximo tests"
    (check-exn exn:fail? (maximo (list 1 2 3)) 2)
    (check-equal? (maximo (list 4)) 4)
    (check-equal? (maximo (list 2 4 8 3)) 8)
  )
)

(define (maximo lst)
  (cond
    [(empty? lst) empty]
    [(empty? (rest lst)) (first lst)]
    [(max (first lst) (maximo (rest lst)))]
  )
)

;; Número, Número -> Número
;; Devolve a se a >= b. Devolve b caso contrário.

(define (max a b)
  (if (>= a b) a b)
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.10

;; Lista -> Lista
;; Devolve uma nova lista com os mesmos elemento de lst em ordem não
;; decrescente.

(define insere-ordenado-tests
  (test-suite
    "insere-ordenado tests"
    (check-equal? (insere-ordenado 2 empty) (list 2))
    (check-equal? (insere-ordenado 1 (list 2)) (list 1 2))
    (check-equal? (insere-ordenado 3 (list 2)) (list 2 3))
    (check-equal? (insere-ordenado 1 (list 2 6 9)) (list 1 2 6 9))
    (check-equal? (insere-ordenado 4 (list 2 6 9)) (list 2 4 6 9))
    (check-equal? (insere-ordenado 7 (list 2 6 9)) (list 2 6 7 9))
    (check-equal? (insere-ordenado 10 (list 2 6 9)) (list 2 6 9 10))))

;; Usando o template de função para listas implementamos o insertion-sort.

(define (insere-ordenado n lst)
  (cond
    [(empty? lst) (list n)]
    [(n . < . (first lst)) (cons n lst)]
    [else (cons (first lst) (insere-ordenado n (rest lst)))]
  )
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.11

;; Lista -> Lista
;; Recebe uma lista aleatória e retorna uma lista com os mesmos elementos em ordem crescente.

(define ordem-cresc-tests
  (test-suite
    "ordem-cresc tests"
    (check-equal? (ordem-cresc empty) empty)
    (check-equal? (ordem-cresc (list 2)) (list 2))
    (check-equal? (ordem-cresc (list 1 5 3 2)) (list 1 2 3 5))
    (check-exn exn:fail? (ordem-cresc (list 3 2 1)) (list 1 3 2))
  )
)

;; Usando o template de função para listas implementamos o insertion-sort.

(define (ordem-cresc lst)
  (cond
    [(empty? lst) empty]
    [else (insere-ordenado (first lst) (ordem-cresc (rest lst)))]
  )
)

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

;; Lista -> List
;; Recebe uma lista aninhada e devolve a mesma lista em ordem reversa

(define inverte-list-tests
  (test-suite
    "inverte-list tests"
    (check-equal? (inverte-list empty) empty)
    (check-equal? (inverte-list (list 1 (list 2 22) 3 4 (list 5 (list 55)))) '(((55) 5) 4 3 (22 2) 1))
    (check-not-equal? (inverte-list (list 10 (list 20 30) 40)) '(40 (20 30) 10))
  )
)

(define (inverte-list lst)
  (cond
    [(empty? lst) empty]
    [(list? (first lst)) (append (inverte-list (rest lst)) (list (inverte-list (first lst))))]
    [else (append (inverte-list (rest lst)) (list (first lst)))]
  )
)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.15

;; Árvore -> Árvore
;; Recebe uma árvore t e uma valor n, devolve a mesma árvore t mas com n somado nos valores do nó

;(define (soma-tree-n t n)
;  (cond
;    [(empty? t) empty]
;    [else
;      (set! t(
;              (+ 1 n)
;              (soma-tree-n (arvore-bin-esq t) n)
;              (soma-tree-n (arvore-bin-dir t) n)))] 
;  )
;)

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

   ;(check-equal? (search-bin-tree? empty) #f)
   ;(check-equal? (search-bin-tree? (bin-tree (bin-tree empty empty 10) (bin-tree empty empty 14) 8)) #f)
;  ))

;(define (check-sub-tree operation bin elem) ;Retorna #t se operation retornar #t para a operacao (<= ou >) de todos os elementos de bin sobre elem
;    (cond 
;        [(and (empty? (bin-tree-l bin)) (empty? (bin-tree-r bin))) (operation elem (bin-tree-val bin))]
;        [else (cond
;                   [(empty? (bin-tree-r bin)) (and (operation elem (bin-tree-val bin)) (check-sub-tree operation (bin-tree-l bin) elem))]        
;                   [(empty? (bin-tree-l bin)) (and (operation elem (bin-tree-val bin)) (check-sub-tree operation (bin-tree-r bin) elem))]
;                   [else
;                        (and 
;                             (operation elem (bin-tree-val bin))
;                             (check-sub-tree operation (bin-tree-l bin) elem)  
;                             (check-sub-tree operation (bin-tree-r bin) elem)
;                        ) 
;                   ]
;        )]
;    )
;)

;(define testee (bin-tree (bin-tree
;                                              (bin-tree empty empty 1)
;                                              (bin-tree
;                                               (bin-tree empty empty 4)
;                                               (bin-tree empty empty 7)
;                                               6)
;                                              3)
;                                             (bin-tree
;                                              empty
;                                              (bin-tree
;                                               (bin-tree empty empty
;                                                13) empty
;                                               14)
;                                              10)
;                                             8))

;(define (search-bin-tree? bin)
;    (cond 
;        [(and (empty? (bin-tree-l bin)) (empty? (bin-tree-r bin))) #t]
;        [else (cond
;                  [(empty? (bin-tree-r bin)) (and (check-sub-tree <= (bin-tree-l bin) (bin-tree-val bin)) (search-bin-tree? (bin-tree-l bin)))]  
;                  [(empty? (bin-tree-l bin)) (and (check-sub-tree > (bin-tree-r bin) (bin-tree-val bin)) (search-bin-tree? (bin-tree-r bin)))]
;                  [else (and 
;                              (check-sub-tree <= (bin-tree-l bin) (bin-tree-val bin))
;                              (check-sub-tree > (bin-tree-r bin) (bin-tree-val bin))
;                              (search-bin-tree? (bin-tree-l bin))
;                              (search-bin-tree? (bin-tree-r bin))
;                         
;                  )]
;        )]
;    )
;)

;;;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.17

;;
;;
;;


