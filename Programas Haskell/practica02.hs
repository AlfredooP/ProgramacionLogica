{- 
Equipo: 01
Maria del Carmen Bracho Felix-221000121
Erick Gabriel Perez Castro- 22130850
Alfredo Puentes Vargas- 22130803
Alexis Eliazith Guerrero Contreras- 21130559

FUNCIONES DECLARADAS DEL LIBRO PIENSA EN HASKEL, EJERCICIOS 1.10-1.31
-}

{-
1.10. Reconocimiento de palíndromos
Ejercicio 1.10.1. Definir la función palindromo tal que (palindromo xs) se verifica si xs es
un palíndromo; es decir, es lo mismo leer xs de izquierda a derecha que de derecha a izquierda.
Por ejemplo,
palindromo [3,2,5,2,3] == True
palindromo [3,2,5,6,2,3] == False
-}

palindromo xs = xs == reverse xs

{-
1.11. Elementos interiores de una lista
Ejercicio 1.11.1. Definir la función interior tal que (interior xs) es la lista obtenida eliminando los extremos de la lista xs. Por ejemplo,
interior [2,5,3,7,3] == [5,3,7]
interior [2..7] == [3,4,5,6]
-}

interior xs = tail (init xs)

{-
1.12. Finales de una lista
Ejercicio 1.12.1. Definir la función finales tal que (finales n xs) es la lista formada por
los n finales elementos de xs. Por ejemplo,
finales 3 [2,5,4,7,9,6] == [7,9,6]
-}

finales n xs = drop (length xs - n) xs

{-
1.13. Segmentos de una lista
Ejercicio 1.13.1. Definir la función segmento tal que (segmento m n xs) es la lista de los
elementos de xs comprendidos entre las posiciones m y n. Por ejemplo,
segmento 3 4 [3,4,1,2,7,9,0] == [1,2]
segmento 3 5 [3,4,1,2,7,9,0] == [1,2,7]
segmento 5 3 [3,4,1,2,7,9,0] == []
-}

segmento m n xs = drop (m-1) (take n xs)

{-
1.14. Extremos de una lista
Ejercicio 1.14.1. Definir la función extremos tal que (extremos n xs) es la lista formada
por los n primeros elementos de xs y los n finales elementos de xs. Por ejemplo,
extremos 3 [2,6,7,1,2,4,5,8,9,2,3] == [2,6,7,9,2,3]
-}

extremos n xs = take n xs ++ drop (length xs - n) xs

{-
1.15. Mediano de 3 números
Ejercicio 1.15.1. Definir la función mediano tal que (mediano x y z) es el número mediano
de los tres números x, y y z. Por ejemplo,
mediano 3 2 5 == 3
mediano 2 4 5 == 4
mediano 2 6 5 == 5
mediano 2 6 6 == 6
-}

mediano x y z = x + y + z- minimum [x,y,z] - maximum [x,y,z]

{-
1.16. Igualdad y diferencia de 3 elementos
Ejercicio 1.16.1. Definir la función tresIguales tal que (tresIguales x y z) se verifica si
los elementos x, y y z son iguales. Por ejemplo,
tresIguales 4 4 4 == True
tresIguales 4 3 4 == False
-}

tresIguales x y z = x == y && y == z

{-
Ejercicio 1.16.2. Definir la función tresDiferentes tal que (tresDiferentes x y z) se
verifica si los elementos x, y y z son distintos. Por ejemplo,
tresDiferentes 3 5 2 == True
tresDiferentes 3 5 3 == False
-}

tresDiferentes x y z = x /= y && x /= z && y /= z

{-
1.17. Igualdad de 4 elementos
Ejercicio 1.17.1. Definir la función cuatroIguales tal que (cuatroIguales x y z u) se
verifica si los elementos x, y, z y u son iguales. Por ejemplo,
cuatroIguales 5 5 5 5 == True
cuatroIguales 5 5 4 5 == False
Indicación: Usar la función tresIguales.
-}

cuatroIguales x y z u = x == y && tresIguales y z u

{-
1.18. Propiedad triangular
Ejercicio 1.18.1. Las longitudes de los lados de un triángulo no pueden ser cualesquiera. Para
que pueda construirse el triángulo, tiene que cumplirse la propiedad triangular; es decir, longitud
de cada lado tiene que ser menor que la suma de los otros dos lados.
Definir la función triangular tal que (triangular a b c) se verifica si a, b y c complen
la propiedad triangular. Por ejemplo,
triangular 3 4 5 == True
triangular 30 4 5 == False
triangular 3 40 5 == False
triangular 3 4 50 == False
-}

triangular a b c = a < b+c && b < a+c && c < a+b

{-
1.19. División segura
Ejercicio 1.19.1. Definir la función divisionSegura tal que (divisionSegura x y) es x
y
si
y no es cero y 9999 en caso contrario. Por ejemplo,
divisionSegura 7 2 == 3.5
divisionSegura 7 0 == 9999.0
-}

divisionSegura _ 0 = 9999
divisionSegura x y = x/y

{-
1.20. Disyunción excluyente
La disyunción excluyente xor de dos fórmulas se verifica si una es verdadera y la
otra es falsa.
Ejercicio 1.20.1. Definir la función xor1 que calcule la disyunción excluyente a partir de la
tabla de verdad. Usar 4 ecuaciones, una por cada línea de la tabla.
-}
xor1 True True = False
xor1 True False = True
xor1 False True = True
xor1 False False = False

{-
Ejercicio 1.20.2. Definir la función xor2 que calcule la disyunción excluyente a partir de la
tabla de verdad y patrones. Usar 2 ecuaciones, una por cada valor del primer argumento.

-}
xor2 True y = not y
xor2 False y = y

{-
Ejercicio 1.20.3. Definir la función xor3 que calcule la disyunción excluyente a partir de la
disyunción (||), conjunción (&&) y negación (not). Usar 1 ecuación.
-}

xor3 x y = (x || y) && not (x && y)

{-
Ejercicio 1.20.4. Definir la función xor4 que calcule la disyunción excluyente a partir de desigualdad (/=). Usar 1 ecuación.
-}
xor4 x y = x /= y

{-
1.21. Módulo de un vector
Ejercicio 1.21.1. Definir la función modulo tal que (modulo v) es el módulo del vector v. Por
ejemplo,
modulo (3,4) == 5.0
-}

modulo (x,y) = sqrt(x^2+y^2)

{-
1.22. Rectángulo de área máxima
Ejercicio 1.22.1. Las dimensiones de los rectángulos puede representarse por pares; por ejemplo,
(5,3) representa a un rectángulo de base 5 y altura 3. Definir la función mayorRectangulo tal
que (mayorRectangulo r1 r2) es el rectángulo de mayor área entre r1 y r2. Por ejemplo,
mayorRectangulo (4,6) (3,7) == (4,6)
mayorRectangulo (4,6) (3,8) == (4,6)
mayorRectangulo (4,6) (3,9) == (3,9)
-}

mayorRectanglo (a,b) (c,d) 
  | a*b >= c*d = (a,b)
  | otherwise = (c,d)


{-
1.23. Puntos del plano
Los puntos del plano se puede representar por un par de números que son sus coordenadas.
1.23.1. Cuadrante de un punto
Ejercicio 1.23.1. Definir la función cuadrante tal que (cuadrante p) es es cuadrante del
punto p (se supone que p no está sobre los ejes). Por ejemplo,
cuadrante (3,5) == 1
cuadrante (-3,5) == 2
cuadrante (-3,-5) == 3
cuadrante (3,-5) == 4
-}

cuadrante (x,y)
  | x > 0 && y > 0 = 1
  | x < 0 && y > 0 = 2
  | x < 0 && y < 0 = 3
  | x > 0 && y < 0 = 4

{-
1.23.2. Intercambio de coordenadas
Ejercicio 1.23.2. Definir la función intercambia tal que (intercambia p) es el punto obtenido intercambiando las coordenadas del punto p. Por ejemplo,
intercambia (2,5) == (5,2)
intercambia (5,2) == (2,5)
-}

intercambia (x,y) = (y,x)

{-
1.23.3. Punto simétrico
Ejercicio 1.23.3. Definir la función simetricoH tal que (simetricoH p) es el punto simétrico
de p respecto del eje horizontal. Por ejemplo,
simetricoH (2,5) == (2,-5)
simetricoH (2,-5) == (2,5)
-}

simetricoH (x,y) = (x,-y)

{-
1.23.4. Distancia entre dos puntos
Ejercicio 1.23.4. Definir la función distancia tal que (distancia p1 p2) es la distancia
entre los puntos p1 y p2. Por ejemplo,
distancia (1,2) (4,6) == 5.0
-}

distancia (x1,y1) (x2,y2) = sqrt((x1-x2)^2+(y1-y2)^2)


{-
1.23.5. Punto medio entre otros dos
Ejercicio 1.23.5. Definir la función puntoMedio tal que (puntoMedio p1 p2) es el punto
medio entre los puntos p1 y p2. Por ejemplo,
puntoMedio (0,2) (0,6) == (0.0,4.0)
puntoMedio (-1,2) (7,6) == (3.0,4.0)
-}

puntoMedio (x1,y1) (x2,y2) = ((x1+x2)/2, (y1+y2)/2)

{-
1.24. Números complejos
Los números complejos pueden representarse mediante pares de números complejos. Por ejemplo, el número 2 + 5i puede representarse mediante el par (2,5).
1.24.1. Suma de dos números complejos
Ejercicio 1.24.1. Definir la función sumaComplejos tal que (sumaComplejos x y) es la suma
de los números complejos x e y. Por ejemplo,
sumaComplejos (2,3) (5,6) == (7,9)
-}

sumaComplejos (a,b) (c,d) = (a+c, b+d)

{-
1.24.2. Producto de dos números complejos
Ejercicio 1.24.2. Definir la función productoComplejos tal que (productoComplejos x y)
es el producto de los números complejos x e y. Por ejemplo,
productoComplejos (2,3) (5,6) == (-8,27)
-}

productoComplejos (a,b) (c,d) = (a*c-b*d, a*d+b*c)

{-
1.24.3. Conjugado de un número complejo
Ejercicio 1.24.3. Definir la función conjugado tal que (conjugado z) es el conjugado del
número complejo z. Por ejemplo,
conjugado (2,3) == (2,-3)
-}

conjugado (a,b) = (a,-b)

{-
1.25. Intercalación de pares
Ejercicio 1.25.1. Definir la función intercala que reciba dos listas xs e ys de dos elementos
cada una, y devuelva una lista de cuatro elementos, construida intercalando los elementos de xs
e ys. Por ejemplo,
intercala [1,4] [3,2] == [1,3,4,2]
-}

intercala [x1,x2] [y1,y2] = [x1,y1,x2,y2]


{-
1.26. Permutación cíclica de una lista
Ejercicio 1.26.1. Definir una función ciclo que permute cíclicamente los elementos de una
lista, pasando el último elemento al principio de la lista. Por ejemplo,
ciclo [2, 5, 7, 9] == [9,2,5,7]
ciclo [] == [9,2,5,7]
ciclo [2] == [2]
-}

ciclo [] = []
ciclo xs = last xs : init xs

{-
1.27. Mayor número de 2 cifras con dos dígitos dados
Ejercicio 1.27.1. Definir la funcion numeroMayor tal que (numeroMayor x y) es el mayor
número de dos cifras que puede construirse con los dígitos x e y. Por ejemplo,
numeroMayor 2 5 == 52
numeroMayor 5 2 == 52
-}

numeroMayor x y = a*10 + b
  where a = max x y
        b = min x y

{-
1.28. Número de raíces de una ecuación cuadrática
Ejercicio 1.28.1. Definir la función numeroDeRaices tal que (numeroDeRaices a b c) es el
número de raíces reales de la ecuación ax2 + bx + c = 0. Por ejemplo,
numeroDeRaices 2 0 3 == 0
numeroDeRaices 4 4 1 == 1
numeroDeRaices 5 23 12 == 2
-}

numeroDeRaices a b c
  | d < 0     = 0
  | d == 0    = 1
  | otherwise = 2
  where d = b^2 - 4*a*c

{-
1.29. Raíces de las ecuaciones cuadráticas
Ejercicio 1.29.1. Definir la función raices de forma que (raices a b c) devuelve la lista de
las raices reales de la ecuación ax2 + bx + c = 0. Por ejemplo,
raices 1 (-2) 1 == [1.0,1.0]
raices 1 3 2 == [-1.0,-2.0]
-}

raices_1 a b c = [(-b+d)/t,(-b-d)/t]
  where d = sqrt (b^2 - 4*a*c)
        t = 2*a

raices_2 a b c
  | d >= 0    = [(-b+e)/(2*a), (-b-e)/(2*a)]
  | otherwise = error "No tiene raices reales"
  where d = b^2 - 4*a*c
        e = sqrt d

{-
1.30. Área de un triángulo mediante la fórmula de Herón
Ejercicio 1.30.1. En geometría, la fórmula de Herón, descubierta por Herón de Alejandría, dice
que el área de un triángulo cuyo lados miden a, b y c es p
s(s − a)(s − b)(s − c), donde s es el
semiperímetro 
s = a+b+c
2.
Definir la función area tal que (area a b c) es el área de un triángulo de lados a, b y c.
Por ejemplo,
area 3 4 5 == 6.0
-}

area a b c = sqrt (s*(s-a)*(s-b)*(s-c))
  where s = (a+b+c)/2

{-
1.31. Números racionales como pares de enteros
Los números racionales pueden representarse mediante pares de números enteros.
Por ejemplo, el número 2
5
puede representarse mediante el par (2,5).
1.31.1. Forma reducida de un número racional
Ejercicio 1.31.1. Definir la función formaReducida tal que (formaReducida x) es la forma
reducida del número racional x. Por ejemplo,
formaReducida (4,10) == (2,5)
-}

formaReducida (a,b) = (a `div` c, b `div` c)
  where c = gcd a b