% 1.Suponemos definida la siguiente base de datos de relaciones familiares:
progenitor(clara,jose).
progenitor(tomas, jose).
progenitor(tomas,isabel).
progenitor(jose, ana).
progenitor(jose, patricia).
progenitor(patricia,jaime).

% Se pide la respuesta de PROLOG y el enunciado verbal de las siguientes preguntas:
% a) ?-progenitor(jaime,X). 
% false: Jaime no es progenitor de ninguna persona.

% b) ?-progenitor(X,jaime). 
% X = patricia.: El progenitor de Jaime es Patricia.

% c) ?-progenitor(clara,X), progenitor(X,patricia). 
% X = jose.: Clara es abuela de Patricia.

% d) ?-progenitor(tomas,X), progenitor(X,Y), progenitor(Y,Z). 
% X = jose, Y = ana, Z = _G1234 ; X = jose, Y = patricia, Z = jaime ; false.: Tomás es bisabuelo de Jaime.

% 2.Dada la base de datos familiar del ejercicio 1, formula en PROLOG las siguientes preguntas:
% a) ¿Quién es el progenitor de Patricia? -> ?- progenitor(X, patricia).
% b) ¿Tiene Isabel un hijo o una hija? -> ?- progenitor(isabel, X).
% c) ¿Quién es el abuelo de Isabel? -> ?- progenitor(X, Y), progenitor(Y, isabel).
% d) ¿Cuáles son los tíos de Patricia? -> ?- progenitor(X, Y), progenitor(Y, patricia), progenitor(Z, Y), X \= Z.

% 3.Considere las reglas para definir un predecesor de alguien:
predecesor(X,Y):-progenitor(X,Y).
predecesor(X,Y):-progenitor(X,Z), predecesor(Z,Y).

% ¿Es una alternativa válida a la definición de predecesor la siguiente?
% predecesor(X,Z):-progenitor(X,Z).
% predecesor(X,Z):- progenitor(Y,Z), predecesor(X,Y).
% Sí, es otra forma correcta de definir la relación recursiva.

% 3. Decir si la unificación tiene éxito y cuál es el resultado de la instanciación de las variables en:
triangulo(punto(-1,0),P2,P3) = triangulo(P1,punto(1,0),punto(0,Y)).
% ¿A qué familia de triángulos da lugar la instanciación resultante?
% Sí, la unificación tiene éxito.
% Triángulos con vértices fijos en (−1, 0) y (1, 0), y un tercer punto en cualquier coordenada (0, Y).

% 4. Con la siguiente definición de segmento:
segmento(punto(X1,Y1),punto(X2,Y2)).
% Representar cualquier segmento línea vertical con X=5.