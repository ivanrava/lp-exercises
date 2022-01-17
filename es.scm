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