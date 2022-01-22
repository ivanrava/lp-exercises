% 1

docenteOccupato(D, G) :- orario(Corso, G, _, _), corso(Corso, D).
docenteLibero(D, G) :- docente(D,_), \+(docenteOccupato(D, G)).
precedente(P, C) :- prerequisito(P, C). % piede della ricorsione
precedente(P, C) :- prerequisito(P, Intermedio), precedente(Intermedio, C). % definizione ricorsiva

% 2

fratello(X, Y) :- genitore(Genitore, X), genitore(Genitore, Y), X \= Y.
cugino(X, Y) :- genitore(G1, X), genitore(G2, Y), fratello(G1, G2).

% 3

superclass(C1, C2) :- inherits(C2, C1). % caso base
superclass(C1, C2) :- inherits(X, C1), superclass(X, C2). % def. ricorsiva
ambref(V, C) :- var(V, C). % def. locale
ambref(V, C) :- superclass(X, C), var(V, X). % def. in superclasse

% 4

activates(P, Q) :- calls(P, Q).
activates(P, Q) :- calls(P, X), activates(X, Q).

% 5

antenato(Antenato, Persona) :- genitore(Antenato, Persona). % caso base
antenato(Antenato, Persona) :- antenato(Antenato, X), genitore(X, Persona). % caso ricorsivo
parenti(X, Y) :- antenato(A, X), antenato(A, Y), X \= Y. % verifica se hanno un antenato in comune

% 6

nonno(X, Y) :- genitore(X, Figli), member(Padre, Figli), genitore(Padre, Nipoti), member(Y, Nipoti).
antenato(X, Y) :- genitore(X, Figli), member(Y, Figli).
antenato(X, Y) :- genitore(X, Figli), member(Appoggio, Figli), antenato(Appoggio, Y).

% 7

raggiungibile(P1, P2) :- vicolo(P1, P2). % caso base
raggiungibile(P1, P2) :- vicolo(X, P2), raggiungibile(P1, X). % caso ricorsivo
circolare(P) :- raggiungibile(P, P). % circolare...

% 8

gen(S, []) :- final(S). % caso base: siamo già in fondo e non dobbiamo fare nulla
gen(S, [H|T]) :- trans(S, H, Appoggio), % H dev'essere raggiungibile (subito) da S, e va in Finale...
                  gen(Appoggio, T). %...e da Finale dobbiamo poter raggiungere uno stato finale

generates(L) :- initial(Iniziale), % parto da uno stato iniziale...
                    gen(Iniziale, L). % ...e vedo se arrivo in uno stato finale

% 9

% Dev'esserci un cammino tra N1 e N2
% cammino come predicato d'appoggio serve per evitare ricorsioni infinite
cammino(N1, N2) :- node(N1, L), member(N2, L). % caso base
cammino(N1, N2) :- node(N1, L), member(X, L), confrontabili(X, N2). % caso ricorsivo
confrontabili(N, N) :- node(N, _).
confrontabili(N1, N2) :- cammino(N1, N2).
confrontabili(N1, N2) :- cammino(N2, N1).

% 10

% I cut sono necessari per far terminare subito "l'esercitazione"
fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, F) :- N1 is N-1, fib(N1, F1), N2 is N-2, fib(N2, F2), F is F1+F2.

% 11

shrunk([], []).
shrunk([X], [X]).
shrunk([H1,_|T1], [H1|T2]) :- shrunk(T1, T2). % sintassi speciale per la 'head' di una lista

% 12

connesso :- finali(Finali), raggiungibili(Finali). % se i nodi finali sono raggiungibili
raggiungibili([H]) :- raggiungibile(H). % caso base: un solo nodo finale
raggiungibili([H|T]) :- raggiungibile(H), raggiungibili(T). % caso ricorsivo: se uno è raggiungibile e anche gli altri lo sono
raggiungibile(Finale) :- iniziale(Iniziale), cammino(Iniziale, Finale). % se esiste un cammino dall'inizio alla fine
% predicato ausiliario: cammino
cammino(Iniziale, Finale) :- arco(Iniziale, Finale).
cammino(Iniziale, Finale) :- arco(Iniziale, X), cammino(X, Finale).

% 13

semisomma([], 0).
semisomma([N], N). % necessario perché accediamo ai primi 2 elementi
semisomma([H,_|T], S) :- semisomma(T, S1), S is S1 + H.

% 14

parola([],0,N,N).
parola([H|T],L,N1,N2) :- L > 0, archi(Archi), member(arco(N1,H,Intermedio), Archi), parola(T,L1,Intermedio,N2), L1 is L-1.

% 15

scatola(P1, P2, P3, P4, P5) :-
  Scaffali = [scaffale(P5,5), scaffale(P4,4), scaffale(P3,3), scaffale(P2,2), scaffale(P1,1)],
  member(scaffale(rossa, R), Scaffali), R \= 3,
  member(scaffale(verde, V), Scaffali), V \= 3,
  member(scaffale(blu, B), Scaffali), B \= 1, B \= 5,
  B is R+1,
  V is B+1,
  member(scaffale(giallo, G), Scaffali), G < B,
  member(scaffale(azzurra, A), Scaffali), A is V+1.

% 16

cugini(X,Y) :- famiglia(_,_,F), famiglia(P1,M1,F1), famiglia(P2,M2,F2), member(M1,F), member(M2,F), member(X,F1), member(Y,F2), P1 \= P2.

% 17

riconosce(A,P) :- automa(A,I,F,T), genera(P,I,F,T).
genera([],S,F,_) :- member(S,F).
genera([H|T],S,F,Trans) :- member(tr(S,H,Succ), Trans), genera(T,Succ,F,Trans).

% 18

percorso(C1,D12,C2) :- link(C1,L,C2), L =< D12.
percorso(C1,D12,C2) :- link(C1,L,Intermedio), Remaining is D12 - L, Remaining > 0, percorso(Intermedio,Remaining,C2).
% is va prima della chiamata ricorsiva a percorso, altrimenti il =< usa operandi non istanziati

% 19

deriva([],[]). % identità
deriva([H|TS],[H|TF]) :- deriva(TS,TF).

deriva([H|T],Frase) :- nonterminali(Nonterm), member(H, Nonterm), produzione(H, R),
                        append(R, T, Forma), deriva(Forma, Frase).

corretta(Frase) :- deriva([H], Frase), assioma(H).

% 20

stessa_generazione(X,Y) :- generazione(X,G), generazione(Y,G), X \= Y.
generazione(X,0) :- capostipite(X).
generazione(X,G) :- member(X,L), genitore(Padre,L), generazione(Padre,G0), G is G0+1

% 21

path(A,A,[A]).
path(N1,N2,[N1|P]) :- arco(N1,S), path(S,N2,P).

% 22

percorso(Piazza) :- strade(L), copre(Piazza, L).

copre(_, []).
copre(Piazza, Strade) :- member(strada(Piazza,Succ), Strade), rimuovi(Strade, strada(Piazza,Succ), Rimanenti), copre(Succ, Rimanenti).

rimuovi([H|T], H, T) :- !.
rimuovi([H|T], G, [H|Tn]) :- rimuovi(T, G, Tn).

% 23

ordinata(X) :- var(X), !.
ordinata([]) :- !.
ordinata([_]) :- !.
ordinata([N1,N2|T]) :- N1 =< N2, ordinata([N2|T]).

ins(N,L1,L2) :- ordinata(L1), ordinata(L2), inserisci(N,L1,L2).

inserisci(N,[],[N]) :- !.
inserisci(N,[H|T],[N,H|T]) :- N =< H, !.
inserisci(N,[H|T],[H|Tn]) :- inserisci(N,T,Tn).

% 24

foglie([]).
foglie([H|T]) :- arco(_,H), \+(arco(H,_)), foglie(T).

% 25

% N = [-3,4,0,2,-1]
% P = [4,2,-2]
positivi([],[]).
positivi([H|Tn],[H|Tp]) :- H > 0, positivi(Tn,Tp).
positivi([H|Tn],Pos) :- H =< 0, positivi(Tn,Pos).

% 26

media(L,M) :- sum(L,S), length(L,Len), M is S / Len.

sum([],0).
sum([H|T],S) :- sum(T,Sn), S is H + Sn.

length([],0).
length([H|T],Len) :- length(T,Slen), Len is Slen + 1.

% esame boh

mu(I,V) :- sostituzione(Stato), member(valore(I,V), Stato).
mu(I,V) :- sostituzione(Stato), \+(member(valore(I,V), Stato)), V=I.

computa([],0).
computa([H],V) :- mu(H,V).
computa([H,+],V) :- mu(H,V).
computa([H,-],V) :- mu(H,V).
computa([H,+|T],V) :- computa(T,Res), mu(H,Val), V is Val+Res.
computa([H,-|T],V) :- computa(T,Res), mu(H,Val), V is Val-Res.

% altro esame boh

pn(N) :- X is N mod 2, X == 0.

pari([], []).
pari([H|T1], [H|T2]) :- pn(H), pari(T1,T2).
pari([H|T1], P) :- \+(pn(H)), pari(T1,P).

