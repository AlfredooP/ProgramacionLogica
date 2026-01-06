% Hechos
padre(juan, maria).
padre(juan, jose).
padre(carlos, juan).

% Regla derivada
abuelo(X, Y) :- padre(X, Z),
                padre(Z, Y).