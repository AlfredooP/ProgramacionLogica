% sintomas(Equipo, ListaDeSintomas).
% Sintomas posibles: sin_internet, ping_falla, wifi_ok, cable_ok,
solo_algunos_sitios, muy_lento.
sintomas(pc1, [sin_internet, ping_falla, wifi_ok]).
sintomas(pc2, [sin_internet, ping_falla, cable_ok, solo_algunos_sitios]).
sintomas(pc3, [muy_lento, wifi_ok]).
sintomas(pc4, [sin_internet, ping_falla]).

% Predicado auxiliar: miembro de lista
miembro(X, [X|_]).
miembro(X, [_|T]) :- miembro(X, T).

% Reglas de diagnostico (simplificadas)
posible_causa(Equipo, router) :-
 sintomas(Equipo, S),
 miembro(sin_internet, S),
 miembro(ping_falla, S),
 miembro(wifi_ok, S).

 posible_causa(Equipo, dns) :-
 sintomas(Equipo, S),
 miembro(solo_algunos_sitios, S).
posible_causa(Equipo, proveedor) :-
 sintomas(Equipo, S),
 miembro(sin_internet, S),
 miembro(ping_falla, S),
 \+ miembro(wifi_ok, S),
 \+ miembro(cable_ok, S).

 usa(Equipo, saturacion_red) :-
 sintomas(Equipo, S),
 miembro(muy_lento, S).