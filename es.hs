-- 1 - lista non ordinata
member :: Eq a => a -> [a] -> Bool
member el head:tail
  | el == head  = True
  | otherwise   = member el tail

-- 1 - lista ordinata
member :: Ord a => a -> [a] -> Bool
member x lst
  | lst == []       = False -- finita la ricerca :(
  | x == head lst   = True  -- trovato!
  | x < head lst    = False -- l'abbiamo già passato :(
  | otherwise       = member x (tail lst) -- continuiamo a cercare...

-- creo una lista di quadrati (potenzialmente infinita)
quadrati :: [Integer]
quadrati = [ x^2 | x <- [ 0.. ] ]

-- verifico se il numero appartiene alla lista di quadrati
is_square :: Integer -> Bool
is_square x = member x quadrati

-- 2 - reverse

reverse :: [a] -> [a]
reverse []            = []
reverse (testa:coda)  = reverse(coda) ++ [testa]

-- 3 - Prodotti e selezione

type Codice = Integer
type Nome = String
type Prezzo = Integer
type Prodotto = (Codice, Nome, Prezzo)

selezione :: [Prodotto] -> Prezzo -> Prezzo -> [Nome]
selezione prodotti min max = [ n | (c, n, p) <- prodotti, p <= max, p >= min ]

-- 4 - Studente e docente

type Matricola = String
type Anno = Int
type Corso = String
type Studente = (Matricola, Anno, Corso)
type Docente = (Nome, Corso)

docenti_dello_studente :: [Studente] -> [Docente] -> Matricola -> [Nome]
-- join e matricola
docenti_dello_studente stud doc mat = [ n | (n, c_d) <- doc, (m, a, c_s) <- stud, c_d == c_s, m == mat ]

-- 5 - Esami

type Matricola = String
type Corso = String
type Voto = Int
type Esame = (Matricola, Corso, Voto)

esamiSostenuti :: [Esame] -> Matricola -> [Corso]
esamiSostenuti esami mat = [ c | (m,c,v) <- esami, m == mat ]

-- 6 - Orari

type Corso = String
type Giorno = Integer
type Ora = Int
type Aula = String
type Orario = (Corso, Giorno, Ora, Ora, Aula)

auleOccupate :: [Orario] -> Giorno -> Ora -> [Aula]
auleOccupate orari giorno ora = [ aula | (_, g, o1, o2, aula) <- orari, g == giorno, o1 <= ora, ora <= o2]

-- 9 - computa

computa :: (Integer -> Integer) -> (Integer -> Integer) -> [Integer] -> [Integer]
computa f g [] = []
computa f g (h:tail) = (f h + g h):(computa f g tail)

-- 10 - last

last :: [a] -> a
last (testa:[]) = testa
last (_:coda) = last coda

-- 11 - shrink

shrink :: [a] -> [a]
shrink [] = []
shrink [x] = [x]
shrink (testa:(testa2:coda2)) = testa:(shrink coda2)

-- 12 - campionato

campionato :: [String] -> [(String, String)]
campionato squadre = [ (s1,s2) | s1 <- squadre, s2 <- squadre, s1 /= s2 ]