%medico (Id, Nombre).
medico(m1, 'Dra. Perez').
medico(m2, 'Dr. Lopez').

%horario_posible (Medico, Dia, Hora).
horario_posible(m1, lunes, '10:00').
horario_posible(m1, lunes, '11:00').
horario_posible(m1, martes, '10:00').
horario_posible(m2, lunes, '10:00').
horario_posible(m2, miercoles, '12:00').

%cita (Medico, Dia, Hora, Paciente).
cita(m1, lunes, '10:00', 'Ana').
cita(m2, lunes, '10:00', 'Luis').

disponible(Medico, Dia, Hora) :-
 horario_posible(Medico, Dia, Hora),
 \+ cita(Medico, Dia, Hora, _).