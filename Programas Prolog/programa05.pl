miembro(X,[X|_]).
miembro(X,[_|Y]):-miembro(X,Y).

concatenar([],L,L).
concatenar([X|L1],L2,[X|L3]):- concatenar(L1,L2,L3).