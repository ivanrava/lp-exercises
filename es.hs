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

-- 7 - Libri

type Titolo = String
type Autore = String
type Edizione = Int
type Nome = String
type Docente = String
type Libro = (Titolo, Autore, Edizione)
type Corso = (Nome, Docente, Libro)
type Cognome = String
type Studente = (Cognome, Corso)

studenti_autori :: [Libro] -> [Corso] -> [Studente] -> [Cognome]
studenti_autori libri corsi studenti = [ stud | (stud, corso) <- studenti, (nome, _, libro) <- corsi, (t, a, _) <- libri, a == stud, nome == corso, t == libro]

-- 8 - Fiumi

type NomeCitta =  String
type NumAbitanti = Int
type Citta = (NomeCitta, NumAbitanti)
type NomeFiume = String
type Lunghezza = Int
type Fiume = (NomeFiume, Lunghezza)
type Attraversamento = (NomeFiume, NomeCitta)

grosse_citta_con_lunghi_fiumi :: [Citta] -> [Fiume] -> [Attraversamento] -> [NomeCitta]
grosse_citta_con_lunghi_fiumi citta fiumi attraversamenti = [ nc | (nc, na) <- citta, na > 1000000, (nf, nc') <- attraversamenti, nf == nc', (nf', l) <- fiumi, nf == nf', l > 500 ]

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

-- 13 - unzip

lista :: [(a,b)] -> ([a], [b])
lista lista_di_coppie = ([ c1 | (c1,_) <- lista_di_coppie ], [ c2 | (_,c2) <- lista_di_coppie ])

-- 14 - take

take :: Int -> [a] -> [a]
take _ [] = []
take 0 _  = []
take n (testa:coda) = testa:(take (n-1) coda)

-- 15 - prefix

prefix :: Eq a => [a] -> [a] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (h1:t1) (h2:t2) = if h1 == h2 then prefix t1 t2 else False

-- 16 - all

all :: [a] -> (a -> Bool) -> Bool
all [] f = True
all (head:tail) f = (f head) && (all f tail)

-- 17 - manca

manca :: Eq a => a -> [a] -> Bool
manca _ [] = True
manca x (head:tail) = (x /= head) && (manca x tail)

unione :: Eq a => [a] -> [a] -> [a]
unione [] l2 = l2
unione l1 [] = l1
unione (testa:coda) l2
  | manca testa l2 = testa:(unione coda l2)
  | otherwise      = unione coda l2

-- 18 - naturals

naturals :: Int -> [Int]
naturals 0 = []
naturals n = naturals (n-1) ++ [n-1]

-- 19 - catena

catena :: [(Int -> Int)] -> Int -> Int
catena [] valore = valore
catena (testa:coda) valore = testa(catena coda valore)

-- 20 - omonimie

type Cognome = String
type Madre = String
type Padre = String
type Figli = [String]
type Famiglia = (Cognome, Madre, Padre, Figli)

omonimie :: [Famiglia] -> [Cognome]
omonimie famiglie = [ c | (c, m, p, figli) <- famiglie, f <- figli, f == m || f == p ]

-- 21 - stessacitta

stessacitta :: [Cliente] -> [Filiale] -> [(String, String)]
stessacitta clienti filiali = [ (nc, nf) | (nc, _, cc) <- clienti, (nf, cf, _) <- filiali, cc == cf ]

-- 22 - applica

applica :: (Int -> Int) -> (Int -> Int) -> [Int] -> [Int]
applica f1 f2 lista = map (f1.f2) lista

-- 23 - dispari

dispari :: Int -> [Int]
dispari 0 = []
dispari n = take n (iterate (+2) 1)

-- 24 - coppie

coppie :: [a] -> (a -> a -> Bool) -> [(a,a)]
coppie lista f = [ (x,y) | x <- lista, y <- lista, f x y ]

-- 25 - discendenti

discendenti :: [String,[String]] -> String -> [String]
discendenti genealogia persona = [ f | (g, figli) <- genealogia, f <- figli, g == persona ] ++ 
                                  [ d | (g, figli) <- genealogia, f <- figli, g == persona, d <- (discendenti genealogia f) ]