% Equipo: 01
% Maria del Carmen Bracho Felix-221000121
% Erick Gabriel Perez Castro- 22130850
% Alfredo Puentes Vargas- 22130803
% Alexis Eliazith Guerrero Contreras- 21130559

% PRACTICA 10- ARITMETICA CON PROLOG

%2.1 Relación Máximo. Determina el mayor de dos números.
% maximo(+X, +Y, ?Z)
maximo(X, Y, X) :- X >= Y.
maximo(X, Y, Y) :- X < Y.

%2.2 Relación Factorial. Calcula el factorial de un número. Caso base: 0! = 1.
% factorial(+X, ?Y)
factorial(0, 1).
factorial(X, Y) :- 
    X > 0, 
    X1 is X - 1, 
    factorial(X1, Y1), 
    Y is X * Y1.

% 2.3 Sucesión de Fibonacci. Calcula el N-ésimo término. Casos base: 0 es 0, 1 es 1.
% fibonacci(+N, -X)
fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, X) :- 
    N > 1, 
    N1 is N - 1, 
    N2 is N - 2, 
    fibonacci(N1, R1), 
    fibonacci(N2, R2), 
    X is R1 + R2.

% 2.4 Máximo Común Divisor (MCD). Usa el algoritmo de Euclides.
% mcd(+X, +Y, ?Z)
mcd(X, 0, X).
mcd(X, Y, Z) :- 
    Y > 0, 
    R is X mod Y, 
    mcd(Y, R, Z).

% 2.5 Longitud de una lista. Cuenta los elementos de una lista recursivamente.
% longitud(?L, ?N)
longitud([], 0).
longitud([_|Cola], N) :- 
    longitud(Cola, N1), 
    N is N1 + 1.

% 2.6 Lista Acotada. Verifica si todos los elementos son menores que la longitud total de la lista. (Requiere el predicado longitud definido arriba).
% lista_acotada(+L)
lista_acotada(L) :- 
    longitud(L, Len), 
    menores_que(L, Len).

% Predicado auxiliar para verificar la condición
menores_que([], _).
menores_que([Cabeza|Cola], Len) :- 
    Cabeza < Len, 
    menores_que(Cola, Len).

% 2.7 Máximo de una lista. Recorre la lista comparando elementos para encontrar el mayor. (Usa maximo del ejercicio 2.1).
% max_lista(+L, ?X)
max_lista([X], X). % Caso base: una lista de un elemento, ese es el máximo
max_lista([Cabeza|Cola], Max) :- 
    max_lista(Cola, MaxCola), 
    maximo(Cabeza, MaxCola, Max).

% 2.8 Suma de lista. Suma todos los elementos.
% suma_lista(+L, ?X)
suma_lista([], 0).
suma_lista([Cabeza|Cola], X) :- 
    suma_lista(Cola, Resto), 
    X is Cabeza + Resto.

% 2.9 Lista Ordenada. Verifica si está ordenada ascendentemente.
% ordenada(+L)
ordenada([]).
ordenada([_]).
ordenada([A, B | Cola]) :- 
    A =< B, 
    ordenada([B | Cola]).

% 2.10 Suma Parcial (Subconjuntos). Busca un subconjunto de la lista L1 que sume exactamente X.
% suma_parcial(+L1, +X, ?L2)
suma_parcial(_, 0, []).
suma_parcial([Cabeza|Cola], Suma, [Cabeza|Resto]) :- 
    Suma >= Cabeza, 
    NuevaSuma is Suma - Cabeza, 
    suma_parcial(Cola, NuevaSuma, Resto).
suma_parcial([_|Cola], Suma, L2) :- 
    suma_parcial(Cola, Suma, L2).

% 2.11 Generar Lista de Ns. Crea una lista de longitud N donde cada elemento es el número N.
% lista(+N, -L)
lista(0, []).
lista(N, [N|Resto]) :- 
    N > 0, 
    N1 is N - 1, 
    lista(N1, Resto).

% 2.12 Lista de Números (Rango). Genera una lista con los números desde N hasta M.
% lista_de_numeros(+N, +M, -L)
lista_de_numeros(N, N, [N]).
lista_de_numeros(N, M, [N|Resto]) :- 
    N < M, 
    N1 is N + 1, 
    lista_de_numeros(N1, M, Resto).

% 2.13 Generador "Entre". Genera números enteros X tal que N1 <= X <= N2.
% entre(+N1, +N2, ?X)
entre(N1, N2, N1) :- N1 =< N2.
entre(N1, N2, X) :- 
    N1 < N2, 
    N3 is N1 + 1, 
    entre(N3, N2, X).

% 2.14 Elemento En (Índice). Encuentra el elemento en la posición K (empezando en 1).
% elemento_en(+K, ?L, ?X)
elemento_en(1, [Cabeza|_], Cabeza).
elemento_en(K, [_|Cola], X) :- 
    K > 1, 
    K1 is K - 1, 
    elemento_en(K1, Cola, X).

% 2.15 Lista Multiplicada. Crea una lista L2 donde cada elemento de L1 se repite N veces.
% multiplicada(+L1, +N, -L2)
multiplicada([], _, []).
multiplicada([Cabeza|Cola], N, L2) :- 
    repetir(Cabeza, N, Repetidos), 
    multiplicada(Cola, N, Resto), 
    append(Repetidos, Resto, L2).

% Predicado auxiliar para repetir un elemento N veces
repetir(_, 0, []).
repetir(Elem, N, [Elem|Resto]) :- 
    N > 0, 
    N1 is N - 1, 
    repetir(Elem, N1, Resto).