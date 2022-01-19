; 1 - reverse -- reverse of list of atoms

(define (reverse lst)
  (if (null? lst) ()
    (append (reverse (cdr lst)) (list (car lst)))
  )
)

; 2 - spec -- recursive specular

(define (spec lst)
  (if (null? lst) ()
    (append (spec (cdr lst)) (list (if (atom? (car lst))
                                        (car lst)
                                        (spec (car lst))
                                    )
                              )
    )
  )
)

; 3 - natoms -- number of atoms in list

(define (natoms lst)
  (if (null? lst) 0
    (+ (natoms (cdr lst)) (if (atom? (car lst))
                            1
                            (natoms (car lst))
                          )
    )
  )
)

; 4 - atomi -- flat list of atoms

(define (atomi L)
  (if (null? L) ()
    (append
      (if (atom? (car L))
        (list (car L))
        (atomi (car L))
      )
      (atomi (cdr L))
    )
  )
)

; 5 - occ -- number of occurrences in list

(define (occ x L)
  (if (null? L) 0
    (if (equal? (car L) x)
      (+ 1 (occ x (cdr L)))
      (occ x (cdr L))
    )
  )
)

; 6 - select -- predicate-based selection

(define (select fun lista)
  (if (null? lista) ()
    (if (fun (car lista))
      (cons (car lista) (select fun (cdr lista)))
      (select fun (cdr lista))
    )
  )
)

; 7 - cancella -- removes elements from list

(define (cancella x L)
  (if (null? L) ()
    (if (equal? x (car L))
      (cancella x (cdr L))
      (cons (car L) (cancella x (cdr L)))
    )
  )
)

; 8 - zip -- python-like zip

(define (zip L1 L2)
  (if (or (null? L1) (null? L2)) ()
    (cons
      (list (car L1) (car L2))
      (zip (cdr L1) (cdr L2))
    )
  )
)

; 9 - shrink -- odd positioned elements

(define (shrink L)
  (if (null? L) ()
    (if (null? (cdr L)) L
      (cons (car L) (cddr L))
    )
  )
)

; 10 - remdup -- remove duplicates

(define (remdup L)
  (if (null? L) ()
    (if (member (car L) (cdr L))
      (remdup (cdr L))
      (cons (car L) (remdup (cdr L)))
    )
  )
)

; 11 - remove -- predicate-based removal

(define (remove f lista)
  (if (null? lista) ()
    (if (f (car lista))
      (remove f (cdr lista))
      (cons (car lista) (remove f (cdr lista)))
    )
  )
)

; 12 - serie -- serie da 1 a n

(define (serie n)
  (if (= n 0) ()
    (append (serie (- n 1)) (list n))
  )
)

; 13 - minimo -- minimum across list

(define (minimo lista)
  (if (null? (cdr lista)) (car lista)
    (if (<= (car lista) (minimo (cdr lista)))
      (car lista)
      (minimo (cdr lista))
    )
  )
)

; 14 - apply -- applies a function to an expression

(define (apply f lista)
  (f (eval lista))
)

; 15 - prefix? -- L1 is a prefix of L2

(define (prefix? L1 L2)
  (if (null? L1)
    #t
    (if (null? L2)
      #f
      (and (equal? (car L1) (car L2)) (prefix? (cdr L1) (cdr L2)))
    )
  )
)

; 16 - appartiene -- x \in set

(define (appartiene x set)
  (if (null? set) #f
    (if (equal? (car set) x) #t
      (appartiene x (cdr set))
    )
  )
)
(define (intersezione set1 set2)
  (if (null? set1) ()
    (if (appartiene (car set1) set2)
      (cons (car set1) (intersezione (cdr set1) set2))
      (intersezione (cdr set1) set2)
    )
  )
)

; 17 - manca -- x not in L

(define (manca x L)
  (if (null? L) #t
    (if (equal? x (car L))
      #f
      (manca x (cdr L))
    )
  )
)
(define (unione L1 L2)
  (if (null? L1)
    L2
    (if (manca (car L1) L2)
      (unione (cdr L1) L2)
      (cons (car L1) (unione (cdr L1) L2))
    )
  )
)

; 18 - take

(define (take n lista)
  (if (or (equal? n 0) (null? lista)) ()
    (cons (car lista) (take (- n 1) (cdr lista)))
  )
)

; 19 - domino

(define (domino lista)
  (if (or (null? lista) (equal? (length lista) 1))
    #t
    (if (equal? (cadar lista) (caadr lista))
      (domino (cdr lista))
      #f
    )
  )
)

; 20 - sublist?

(define (sublist? S L)
  (if (> (length S) (length L)) #f
    (if (prefix? S L)
      #t
      (sublist? S (cdr L))
    )
  )
)

; 21 - indexing

(define (indexing A key)
  (if (null? A) 'error
    (if (equal? (caar A) key)
      (cadar A)
      (indexing (cdr A) key)
    )
  )
)

; 22 - duplica

(define (duplica lista)
  (if (null? lista) ()
    (cons (list (car lista) (car lista)) (duplica (cdr lista)))
  )
)

; 23 - listafib

(define (fib n)
  (if (= n 0) 0
    (if (= n 1) 1
      (+ (fib (- n 1)) (fib (- n 2)))
    )
  )
)
(define (listafib n)
  (if (< n 0) ()
    (append (listafib (- n 1) list(fib n)))
  )
)

; 24 - iniziale

(define (iniziale lista)
  (if (<= (length lista) 1) ()
    (cons (car lista) (iniziale (cdr lista)))
  )
)

; 25 - inserisci

(define (inserisci n lista)
  (if (null? lista) (list n)
    (if (< (car lista) n)
      (cons (car lista) (inserisci n (cdr lista))) ; prosegui
      (cons n lista) ; inserisci
    )
  )
)
(define (ordina lista)
  (if (null? lista) ()
    (inserisci (car lista) (ordina (cdr lista)))
  )
)

; 26 - fattoriali

(define (fact n)
  (if (= n 0) 1
    (* n (fact (- n 1)))
  )
)
(define (fattoriali n)
  (if (= n 0) '(1)
    (append (fattoriali (- n 1)) (list (fact n)))
  )
)