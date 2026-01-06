% Equipo: 01
% Maria del Carmen Bracho Felix-221000121
% Erick Gabriel Perez Castro- 22130850
% Alfredo Puentes Vargas- 22130803
% Alexis Eliazith Guerrero Contreras- 21130559

% PRACTICA 14 - PREDICADOS DINAMICOS

% Ejercicio 1: Familias. Define hechos sobre relaciones familiares. Utiliza asserta para agregar
% padres al principio de la base de conocimientos y assertz para agregar hijos al final. Luego,
% utiliza retract para eliminar a un miembro de la familia.
% Indica que los predicados padre/2 y madre/2 pueden modificarse.
:- dynamic padre/2.
:- dynamic madre/2.

% Hechos iniciales
padre(jose, juan).
madre(maria, juan).

% Agregar padre al inicio
% asserta coloca el hecho en la primera posición.
agregar_padre(Parent, Child) :-
    asserta(padre(Parent, Child)).

% Agregar hijo al final
% assertz inserta al final.
agregar_hijo(Parent, Child) :-
    assertz(padre(Parent, Child)).

% Eliminar miembro específico
% retract busca un hecho que coincida y lo elimina.
eliminar_padre(Parent, Child) :-
    retract(padre(Parent, Child)).

% Reiniciar base (padre y madre)
% abolish elimina todos los hechos y el predicado como si nunca hubiera existido.
reiniciar_familia :-
    abolish(padre/2),
    abolish(madre/2).

% Ejercicio 2: Notas de Estudiantes Crea una base de conocimientos sobre estudiantes y sus
% notas. Utiliza assert para agregar nuevas notas y retract para eliminar notas específicas.
% Finalmente, usa abolish para eliminar todas las notas de un estudiante.
:- dynamic nota/2.

% Hechos iniciales
nota(juan, 8).
nota(maria, 9).

% Agregar nuevas notas
% assert puede agregar al inicio o al final dependiendo del motor, pero no importa aquí
agregar_nota(Estudiante, Nota) :-
    assert(nota(Estudiante, Nota)).

% Eliminar nota específica
eliminar_nota(Estudiante, Nota) :-
    retract(nota(Estudiante, Nota)).

% Eliminar todas las notas de un estudiante
% retract(nota(E, _)) elimina una nota. Si existen más notas, Prolog volverá a intentar (backtracking).
% fail obliga a seguir buscando más hechos hasta que ya no existan. ; true asegura que el predicado termine con éxito.
eliminar_notas_estudiante(Estudiante) :-
    retract(nota(Estudiante, _)),
    fail ; true.

% Reiniciar todas las notas
reiniciar_notas :-
    abolish(nota/2).

% Ejercicio 3: Libros y Autores Define una base de conocimientos sobre libros y sus autores.
% Utiliza assertz para agregar nuevos libros al final y retract para eliminar un libro específico.
% Además, usa abolish para eliminar todos los libros de un autor.
:- dynamic autor/2.

% Hechos iniciales
autor(harry_potter, jk_rowling).
autor(hunger_games, suzanne_collins).

% Agregar libro al final
% Se agrega al final para mantener orden cronológico de registro.
agregar_libro(Libro, Autor) :-
    assertz(autor(Libro, Autor)).

% Eliminar libro específico
eliminar_libro(Libro) :-
    retract(autor(Libro, _)).

% Eliminar todos los libros de un autor
% Mismo patrón explicado antes para eliminar múltiples hechos.
eliminar_libros_autor(Autor) :-
    retract(autor(_, Autor)),
    fail ; true.

% Reiniciar todos los hechos
reiniciar_autores :-
    abolish(autor/2).

% Ejercicio 4: Información de Empleados Crea hechos sobre empleados y sus departamentos.
% Utiliza asserta para agregar nuevos empleados al principio y retract para eliminar empleados
% específicos. Usa abolish para eliminar todos los empleados de un departamento.
:- dynamic empleado/2.

% Hechos iniciales
empleado(ana, marketing).
empleado(carlos, sistemas).

% Agregar empleado al inicio
agregar_empleado(Nombre, Dep) :-
    asserta(empleado(Nombre, Dep)).

% Eliminar empleado específico
eliminar_empleado(Nombre) :-
    retract(empleado(Nombre, _)).

% Eliminar todos los empleados de un departamento
eliminar_empleados_departamento(Dep) :-
    retract(empleado(_, Dep)),
    fail ; true.

% Reiniciar
reiniciar_empleados :-
    abolish(empleado/2).

% Ejercicio 5: Lista de Tareas Define hechos sobre tareas pendientes. Utiliza assert, assertz y
% retract para agregar, reorganizar y eliminar tareas. Luego, usa abolish para reiniciar la lista de
% tareas.
:- dynamic tarea/1.

% Hechos iniciales
tarea(hacer_compras).
tarea(limpiar_casa).

% Agregar tarea al final
agregar_tarea(T) :-
    assertz(tarea(T)).

% Agregar tarea al inicio
agregar_tarea_inicio(T) :-
    asserta(tarea(T)).

% Eliminar una tarea
eliminar_tarea(T) :-
    retract(tarea(T)).

% Reorganizar (ejemplo simple: mover tarea al final)
% Esto simula un cambio de orden.
mover_tarea_al_final(T) :-
    retract(tarea(T)),
    assertz(tarea(T)).

% Reiniciar lista de tareas
reiniciar_tareas :-
    abolish(tarea/1).

