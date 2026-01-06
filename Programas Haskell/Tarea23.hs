-- Equipo: 01
-- Maria del Carmen Bracho Felix-221000121
-- Erick Gabriel Perez Castro- 22130850
-- Alfredo Puentes Vargas- 22130803
-- Alexis Eliazith Guerrero Contreras- 21130559

-- Tarea 23 Listas Infifnitas.

-- 7.5 Aplicación iterada de una función a un elemento
-- 7.5.1. Definir, por recursión, la función tal que (itera f x) es la lista cuyo primer elemento es x y los siguientes elementos se calculan aplicando la función f al elemento anterior. 
itera :: (a -> a) -> a -> [a]
itera f x = x : itera f (f x)

-- 7.6 Agrupacion de elementos consecutivos
-- 7.6.1. Definir, por recursión, la función tal que (agrupa n xs) es la lista formada por listas de n elementos consecutivos de la lista xs.
agrupa :: Int -> [a] -> [[a]]
agrupa n [] = []
agrupa n xs = take n xs : agrupa n (drop n xs)

-- 7.6.2. Definir, de manera no recursiva, la función tal que (agrupa' n xs) es la lista formada por listas de n elementos consecutivos de la lista xs
agrupa' :: Int -> [a] -> [[a]]
agrupa' n = takeWhile (not null)
 map (take n)
 iterate (drop n)

-- 7.6.3. Definir, y comprobar, con QuickCheck las dos propiedades que caracterizan a la función agrupa
prop_AgrupaLongitud :: Int -> [Int] -> Property
prop_AgrupaLongitud n xs = n > 0 && not (null gs) ==> and [length g == n | g <- init gs] && 0 < length (last gs) && length (last gs) <= n where gs = agrupa n xs

prop_AgrupaCombina :: Int -> [Int] -> Property
prop_AgrupaCombina n xs = n > 0 ==> concat (agrupa n xs) == xs

-- 7.7 La sucesión de Collatz
-- 7.7.1. Definir la función tal que (siguiente n) es el siguiente de n en la sucesión de Collatz.
siguiente n | even n = n `div` 2
    | otherwise = 3*n+1

-- 7.7.2. Definir, por recursión, la función tal que (collatz n) es la órbita de Collatz de n hasta alcanzar el 1.
collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n = n : collatz (siguiente n)

-- 7.7.3. Definir, sin recursión, la función tal que (collatz' n) es la órbita de Collatz de n hasta alcanzar el 1.
collatz' :: Integer -> [Integer]
collatz' n = (takeWhile (/=1) (iterate siguiente n)) ++ [1]

-- 7.7.4. Definir la función tal que (menorCollatzMayor x) es el menor número cuya órbita de Collatz tiene más de x elementos. 
menorCollatzMayor :: Int -> Integer
menorCollatzMayor x = head [y | y <- [1..], length (collatz y) > x]

-- 7.7.5. Definir la función tal que (menorCollatzSupera x) es el menor número cuya órbita de Collatz tiene algún elemento mayor que x. 
menorCollatzSupera :: Integer -> Integer
menorCollatzSupera x = head [y | y <- [1..], maximum (collatz y) > x]

menorCollatzSupera' :: Integer -> Integer
menorCollatzSupera' x = head [n | n <- [1..], t <- collatz' n, t > x]

-- 7.8 Números primos
-- 7.8.1. Definir la constante tal que primos es la lista de los primos mediante la criba de Eratóstenes
primos :: Integral a => [a]
primos = criba [2..]
    where
        criba [] = [] 
        criba (n:ns) = n : criba (elimina n ns) 
        elimina n xs = [x | x <- xs, x `mod` n /= 0]

-- 7.8.2. Definir la funcion tal que (primo x) se verifica si x es primo
primo :: Integral a => a -> Bool
primo x = x == head (dropWhile (<x) primos)

-- 7.9 Descomposiciones como suma de dos primos
-- 7.9.1. Definir la función tal que (sumaDeDosPrimos n) es la lista de las distintas descomposiciones de n como suma de dos números primos.
sumaDeDosPrimos :: Int -> [(Int,Int)]
sumaDeDosPrimos n = [(x,n-x) | x <- primosN, x < n-x, elem (n-x) primosN]
    where primosN = takeWhile (<=n) primos

-- 7.10 Números expresables como producto de dos primos
-- 7.10.1. Definir la función tal que (esProductoDeDosPrimos n) se verifica si n es el producto de dos primos distintos
esProductoDeDosPrimos :: Int -> Bool
esProductoDeDosPrimos n = [x | x <- primosN, mod n x == 0, div n x /= x, elem (div n x) primosN] /= []
    where primosN = takeWhile (<=n) primos

-- 7.11 Números muy compuestos
-- 7.11.1 Definir la función tal que (esMuyCompuesto x) se verifica si x es un número muy compuesto.
esMuyCompuesto :: Int -> Bool
esMuyCompuesto x = and [numeroDivisores y < n | y <- [1..x-1]]
    where n = numeroDivisores x

numeroDivisores :: Int -> Int
numeroDivisores = length . divisores

-- 7.11.3. Definir la función tal que (muyCompuesto n) es el n–ésimo número muy compuesto.
muyCompuesto :: Int -> Int
muyCompuesto n = [x | x <- [1..], esMuyCompuesto x] !! n

-- 7.12 Suma de numeros primos truncables
-- 7.12.1 Definir la función tal que (primoTruncable x) se verifica si x es un primo truncable.
primoTruncable :: Int -> Bool
primoTruncable x
    | x < 10 = primo x
    | otherwise = primo x && primoTruncable (x `div` 10)

-- 7.12.2. Definir la función tal que (sumaPrimosTruncables n) es la suma de los n primeros primos truncables.
sumaPrimosTruncables :: Int -> Int
sumaPrimosTruncables n = sum (take n [x | x <- primos, primoTruncable x])

-- 7.13 Primos permutables
-- 7.13.1. Definir la función tal que (primoPermutable x) se verifica si x es un primo permutable
primoPermutable :: Int -> Bool
primoPermutable x = and [primo y | y <- permutacionesN x]

permutacionesN :: Int -> [Int]
permutacionesN x = [read ys | ys <- permutaciones (show x)]

-- 7.14 Ordenación  de los números enteros
-- 7.14.1. Definir, por comprensión, la constante tal que enteros es la lista de los enteros con la ordenación anterior
enteros :: [Int]
enteros = 0 : concat [[-x,x] | x <- [1..]]

-- 7.14.2. Definir, por iteración, la constante tal que enteros' es la lista de los enteros con la ordenación anterior.
enteros' :: [Int]
enteros' = iterate siguiente 0
    where siguiente x | x >= 0 = -x-1
                      | otherwise = -x

-- 7.14.3. Definir, por selección con takeWhile, la función tal que (posicion x) es la posición del entero x en la ordenación anterior. 
posicion :: Int -> Int
posicion x = length (takeWhile (/=x) enteros)

-- 7.14.4. Definir, por recursión, la función tal que (posicion1 x) es la posición del entero x en la ordenación anterior.
posicion1 :: Int -> Int
posicion1 x = aux enteros 0
    where aux (y:ys) n | x == y = n
            | otherwise = aux ys (n+1)

-- 7.14.5. Definir, por comprension, la función tal que (posicion2 x) es la posición del entero x en la ordenación anterior
posicion2 :: Int -> Int
posicion2 x = head [n | (n,y) <- zip [0..] enteros, y == x]

-- Definir, sin búsqueda, la función tal que (posicion3 x) es la posición del entero x en la ordenación anterior. 
posicion3 :: Int -> Int
posicion3 x | x >= 0 = 2*x
        | otherwise = 2*(-x)-1

-- 7.15. La sucesión de Hamming
-- 7.15.1. Definir la constante tal que hamming es la sucesión de Hamming.
hamming :: [Int] hamming = 1 : mezcla3 [2*i 
    | i <- hamming] [3*i 
    | i <- hamming] [5*i 
    | i <- hamming] 

-- funciones auxiliares
mezcla3 :: [Int] -> [Int] -> [Int] -> [Int] 
mezcla3 xs ys zs = mezcla2 xs (mezcla2 ys zs)
mezcla2 :: [Int] -> [Int] -> [Int] 
mezcla2 p@(x:xs) q@(y:ys) | x < y = x:mezcla2 xs q 
    | x > y = y:mezcla2 p ys 
    | otherwise = x:mezcla2 xs ys 
mezcla2 [] ys = ys 
mezcla2 xs [] = xs 

-- 7.15.2. Definir la función tal que (divisoresEn x ys) se verifica si x puede expresarse como un producto de potencias de elementos de ys. 
divisoresEn :: Int -> [Int] -> Bool 
divisoresEn 1 _ = True 
divisoresEn x [] = False 
divisoresEn x (y:ys) | mod x y == 0 = divisoresEn (div x y) (y:ys) 
    | otherwise = divisoresEn x ys 

-- 7.15.3. Definir, usando divisoresEn, la constante tal que hamming’ es la sucesión de Hamming
hamming' :: [Int] 
hamming' = [x | x <- [1..], divisoresEn x [2,3,5]] 

-- 7.15.4. Definir la función tal que (cantidadHammingMenores x) es la cantidad de números de Hamming menores que x.
cantidadHammingMenores :: Int -> Int
cantidadHammingMenores x = length (takeWhile (<x) hamming')

-- 7.15.5. Definir la función tal que (siguienteHamming x) es el menor número de la sucesión de Hamming mayor que x. 
siguienteHamming :: Int -> Int 
siguienteHamming x = head (dropWhile (<=x) hamming') 

-- 7.15.6. Definir la función tal que (huecoHamming n) es la lista de pares de números consecutivos en la sucesión de Hamming cuya distancia es mayor que n
huecoHamming :: Int -> [(Int,Int)]
huecoHamming n = [(x,y) | x <- hamming', 
    let y = siguienteHamming x, y-x > n] 

-- 7.15.7. Comprobar con QuickCheck que para todo n, existen pares de números consecutivos en la sucesión de Hamming cuya distancia es mayor o igual que n.
prop_Hamming :: Int -> Bool 
prop_Hamming n = huecoHamming n' /= [] where n' = abs n 

-- 7.16. Suma de los primos menores que n
-- 7.16.1 (Problema 10 del Proyecto Euler). Definir la función tal que (sumaPrimoMenores n) es la suma de los primos menores que n.
sumaPrimoMenores :: Int -> Int 
sumaPrimoMenores n = sumaMenores n primos 0 
    where sumaMenores n (x:xs) a
            | n <= x = a 
            | otherwise = sumaMenores n xs (a+x)

-- 7.17. Menor número triangular con más de n divisores
-- 7.17.1 Definir la función tal que (euler12 n) es el menor número triangular con más de n divisores
euler12 :: Int -> Integer 
euler12 n = head [x | x <- triangulares, nDivisores x > n] 

-- funciones auxiliares
triangulares :: [Integer] 
triangulares = 1:[x+y | (x,y) <- zip [2..] triangulares] 

divisores :: Integer -> [Integer] 
divisores x = [y | y <- [1..x], mod x y == 0]

nDivisores :: Integer -> Int 
nDivisores x = length (divisores x) 

-- 7.18. Números primos consecutivos con dígitos con igual media
-- 7.18.1. Definir la función tal que (primosEquivalentes n) es la lista de las sucesiones de n números primos consecutivos con la media de sus dígitos iguales. 
primosEquivalentes :: Int -> [[Integer]] 
primosEquivalentes n = aux primos 
    where aux (x:xs)
            | relacionados equivalentes ys = ys : aux xs 
            | otherwise = aux xs 
            where ys = take n (x:xs) 

-- 7.19. Decisión de pertenencia al rango de una función creciente
-- 7.19.1. Definir la función tal que (perteneceRango x f) se verifica si x pertenece al rango de la función f, suponiendo que f es una función creciente cuyo dominio es el conjunto de los números naturales. 
perteneceRango :: Int -> (Int -> Int) -> Bool 
perteneceRango y f = y `elem` takeWhile (<=y) (imagenes f) 
    where imagenes f = [f x | x <- [0..]] 

-- 7.20. Pares ordenados por posición
-- 7.20.1. Definir, por recursión, la función tal que (paresOrdenados xs) es la lista de todos los pares de elementos (x,y) de xs, tales que x ocurren en xs antes que y
paresOrdenados :: [a] -> [(a,a)] 
paresOrdenados [] = [] 
paresOrdenados (x:xs) = [(x,y) | y <- xs] ++ paresOrdenados xs 

-- 7.20.2. Definir, por plegado, la función tal que (paresOrdenados2 xs) es la lista de todos los pares de elementos (x,y) de xs, tales que x ocurren en xs antes que y. 
paresOrdenados2 :: [a] -> [(a,a)] 
paresOrdenados2 [] = [] 
paresOrdenados2 (x:xs) = foldr (\y ac -> (x,y):ac) (paresOrdenados2 xs) xs

-- 7.20.3. Definir, usando repeat, la función tal que (paresOrdenados3 xs) es la lista de todos los pares de elementos (x,y) de xs, tales que x ocurren en xs antes que y
paresOrdenados3 :: [a] -> [(a,a)] 
paresOrdenados3 [] = [] 
paresOrdenados3 (x:xs) = zip (repeat x) xs ++ paresOrdenados3 xs

-- 7.21. Aplicación iterada de una función 
-- 7.21.1. Definir, por recursión, la función tal que (potenciaFunc n f x) es el resultado de aplicar n veces la función f a x. 
potenciaFunc :: Int -> (a -> a) -> a -> a 
potenciaFunc 0 _ x = x 
potenciaFunc n f x = potenciaFunc (n-1) f (f x)

-- 7.21.2. Definir, sin recursión, la función tal que (potenciaFunc2 n f x) es el resultado de aplicar n veces la función f a x.
potenciaFunc2 :: Int -> (a -> a) -> a -> a 
potenciaFunc2 n f x = last (take (n+1) (iterate f x))

-- 7.22 Expresión de un número como suma de dos de una lista
-- 7.22.1 Definir, por recursión, la función tal que (sumaDeDos x ys) decide si x puede expresarse como suma de dos elementos de ys y, en su caso, devuelve un par de elementos de ys cuya suma es x
sumaDeDos :: Int -> [Int] -> Maybe (Int,Int) 
sumaDeDos _ [] = Nothing 
sumaDeDos _ [_] = Nothing 
sumaDeDos y (x:xs)
    | y-x `elem` xs = Just (x,y-x) 
    | otherwise = sumaDeDos y xs

-- 7.22.2. Definir, usando la función paresOrdenados tal que (sumaDeDos' x ys) decide si x puede expresarse como suma de dos elementos de ys y, en su caso, devuelve un par de elementos de ys cuya suma es x.
sumaDeDos' :: Int -> [Int] -> Maybe (Int,Int)
sumaDeDos' x xs | null ys = Nothing 
    | otherwise = Just (head ys) 
    where ys = [(a,b) | (a,b) <- paresOrdenados xs , a+b == x] 

-- –7.23 La bicicleta de Turing
-- 7.23.1 Definir la función tal que (eslabones i d n) es la lista con los números de eslabones que tocan el radio doblado en cada vuelta en una bicicleta de tipo (i,d,n).
eslabones :: Int -> Int -> Int -> [Int] 
eslabones i d n = [(i+d*j) `mod` n | j <- [0..]]

-- 7.23.2 Definir la función tal que (numeroVueltas i d n) es el número de vueltas que pasarán hasta que la cadena se rompa en una bicicleta de tipo (i,d,n). 
numeroVueltas :: Int -> Int -> Int -> Int 
numeroVueltas i d n = length (takeWhile (/=0) (eslabones i d n)) 

-- 7.24 Sucesion de Golomb
-- 7.24.1 Definir la función tal que (golomb n) es el n–ésimo término de la sucesión de Golomb
golomb :: Int -> Int
golomb :: Int -> Int golomb n = sucGolomb !! (n-1) 

-- 7.24.2. Definir la función tal que sucGolomb es la lista de los términos de la sucesión de Golomb.
sucGolomb :: [Int]
sucGolomb :: [Int] sucGolomb = subSucGolomb 1

-- 7.24.3. Definir la función tal que (subSucGolomb x) es la lista de los términos de la sucesión de Golomb a partir de la primera ocurrencia de x.
subSucGolomb :: Int -> [Int] 
subSucGolomb 1 = [1] ++ subSucGolomb 2 
subSucGolomb 2 = [2,2] ++ subSucGolomb 3 
subSucGolomb x = (replicate (golomb x) x) ++ subSucGolomb (x+1)

-- 7.24. Sucesión de Golomb
sucGolomb' :: [Int] 
sucGolomb' = 1 : 2 : 2 : g 3 
    where g x = replicate (golomb x) x ++ g (x+1) 




