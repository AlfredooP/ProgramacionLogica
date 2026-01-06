% ----- Datos de alumnos -----
% alumno(Nombre, Semestre, AreaInteres).
alumno(Nombre, Semestre, AreaInteres).
alumno(ana, 5, ia).
alummno(juan, 7, redes).
alumno(luis, 4, web).
alumno(maria, 8, datos).

% ----- Datos de cursos optativos -----
% curso(Clave, Nombre, Area, SemestreMinimo).
curso(ia1, 'Introduccion a la IA', ia, 5).
curso(ia2, 'Sistemas Expertos', ia, 7).
curso(r1, 'Redes Inalambricas', redes, 6).
curso(r2, 'Seguridad en Redes', redes, 7).
curso(w1, 'Desarrollo Web Fullstack', web, 5).
curso(d1, 'Mineria de Datos', datos, 7).
curso(d2, 'Aprendizaje Automatico', datos, 8).

recomienda(NombreAlumno, NombreCurso) :-
 alumno(NombreAlumno, SemActual, Area),
 curso(_, NombreCurso, Area, SemMin),
 SemActual >= SemMi.