-- cuadrado :: Int -> Int
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant if" #-}
cuadrado n = n * n

maximo :: Int -> Int -> Int
maximo n m
    | n >= m  = n
    | n < m   = m

-- Calcula el doble de un numero
doble x = x + x

-- Calacula la suma de los numeros de 1 a n
-- suma n = sum [1..n]

-- Calcula el cuadruple de un numero
cuadruple x = doble (doble x)

-- Calcula el factorial de un numero
factorial n = product [1..n]

-- Calcula la media de una lista de numeros
media ns = sum ns `div` length ns

-- Suma dos numeros enteros
-- suma2 :: (Int,Int) -> Int
-- suma2 (x,y) = x+y

-- deCeroA n es la lista de numeros desde 0 hasta n
deCeroA :: Int -> [Int]
deCeroA n = [0..n]

-- Parcializacion
suma' :: Int -> (Int -> Int)
suma' x y = x+y

-- Parcializacion con tres argumentos
mult :: Int -> (Int -> (Int -> Int))
mult x y z = x*y*z

-- Sucesor de un numero
suc :: Int -> Int
suc = suma' 1

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- par :: (Integral a) => a -> Bool
-- par n = n `rem` 2 == 0

-- splitAt :: Int -> [a] -> ([a],[a])
-- splitAt n xs = (take n xs, drop n xs)

-- Definición sin lambda
suma x y = x+y

-- Definición con lambda
-- suma' = \x -> (\y -> x+y)

-- (impares n) es la lista de los n primeros números impares.
-- Definición sin lambda:
-- impares n = map f [0..n-1]
--     where f x = 2*x+1

-- Definición con lambda:
-- impares' n = map (\x -> 2*x+1) [0..n-1]

-- Definición recursiva del factorial
-- factorial :: Integer -> Integer
-- factorial 0 = 1
-- factorial n = n * factorial (n-1)

-- longitud :: [a] -> Int
-- longitud [] = 0
-- longitud (_:xs) = 1 + longitud xs

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

-- (++) :: [a] -> [a] -> [a]
-- [] ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)

inserta :: Ord a => a -> [a] -> [a]
inserta e [] = [e]
inserta e (x:xs) | e <= x = e : (x:xs)
    | otherwise = x : inserta e xs

-- ordena_por_insercion :: Ord a => [a] -> [a]
-- ordena_por_insercion [] = []
-- ordena_por_insercion (x:xs) = inserta x (ordena_por_insercion xs)

emparejar :: [a] -> [b] -> [(a, b)]
emparejar [] _ = []
emparejar _ [] = []
emparejar (x:xs) (y:ys) = (x,y) : emparejar xs ys

eliminar :: Int -> [a] -> [a]
eliminar 0 xs = xs
eliminar n [] = []
eliminar n (x:xs) = eliminar (n-1) xs

-- fibonacci :: Int -> Int
-- fibonacci 0 = 0
-- fibonacci 1 = 1
-- fibonacci n = fibonacci (n-2) + fibonacci (n-1)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort mayores
    where 
        menores = [a | a <- xs, a <= x]
        mayores = [b | b <- xs, b > x]

-- Par e impar por recursión mutua:
par :: Int -> Bool
par 0 = True
par n = impar (n-1)

impar :: Int -> Bool
impar 0 = False
impar n = par (n-1)

concatenar :: [[a]] -> [a]
concatenar xss = [x | xs <- xss, x <- xs]

primeros :: [(a, b)] -> [a]
primeros ps = [x | (x,_) <- ps]

longitud :: [a] -> Int
longitud xs = sum [1 | _ <- xs]

factores :: Int -> [Int]
factores n = [x | x <- [1..n], n `mod` x == 0]

primo :: Int -> Bool
primo n = factores n == [1, n]

-- busca c t es la lista de los valores de la lista de asociación t cuyas claves valen c. 
busca :: Eq a => a -> [(a, b)] -> [b]
busca c t = [v | (c', v) <- t, c' == c]

-- busca1 v t es la lista de las claves de la lista de asociación t cuyos valores son v.
busca1 :: Eq b => b -> [(a, b)] -> [a]
busca1 v t = [c | (c, v') <- t, v' == v]

-- Generar pares de elementos adyacentes en una lista
adyacentes :: [a] -> [(a, a)]
adyacentes xs = zip xs (tail xs)

-- Saber si una lista está ordenada
ordenada :: Ord a => [a] -> Bool
ordenada xs = and [x <= y | (x,y) <- adyacentes xs]

-- Lista de las posiciones ocupadas por el elemento x en la lista xs. 
posiciones :: Eq a => a -> [a] -> [Int]
posiciones x xs =
    [i | (x',i) <- zip xs [0..n], x == x']
    where n = length xs - 1

-- Lista de las letras minúsculas en una cadena
minusculas :: String -> String
minusculas xs = [x | x <- xs, elem x ['a'..'z']]

-- Número de ocurrencias de un carácter en una cadena
ocurrencias :: Char -> String -> Int
ocurrencias x xs = length [x' | x' <- xs, x == x']

-- Definición de map por comprensión:
mapComp :: (a -> b) -> [a] -> [b]
mapComp f xs = [f x | x <- xs]

-- Definición de map por recursión:
mapRec :: (a -> b) -> [a] -> [b]
mapRec _ [] = []
mapRec f (x:xs) = f x : mapRec f xs

-- Implementa una funcion que determine si un número es divisible por 3 y por 5.
divisiblePor3y5 :: Int -> Bool
divisiblePor3y5 n = (n `mod` 3 == 0) && (n `mod` 5 == 0)

-- Defina utilizando condicionales if una función que determine si un año es bisiesto.
esBisiesto :: Int -> Bool
esBisiesto n = if (n `mod` 4 == 0 && n `mod` 100 /= 0) || (n `mod` 400 == 0)
                then True
                else False

-- Utilice guardas para definir una función que clasifique un triangulo según sus lados: equilátero, isósceles o escaleno.
clasificarTriangulo :: Int -> Int -> Int -> String
clasificarTriangulo a b c
    | a == b && b == c = "equilatero"
    | a == b || b == c || a == c = "isosceles"
    | otherwise = "escaleno"

-- Elabore la funcion impares n, que devuelva la lista de los n primeros numeros impares
impares n = map f [0..n-1]
    where f x = 2*x+1

-- Definición con lambda:
impares' n = map (\x -> 2*x+1) [0..n-1]

-- main:: IO ()
-- main = do
--     print(divisiblePor3y5 15)
--     print(divisiblePor3y5 9)
--     print(divisiblePor3y5 10)
--     print(divisiblePor3y5 13)

nombres :: [String]
nombres = ["Ana", "Juan", "Pedro", "Sofia"]
-- main = do
--     print (head nombres) -- "Ana" (Primer elemento)
--     print (last nombres) -- "Sofía" (Último elemento)
--     print (nombres !! 2) -- "Pedro" (Índice 2)

naturales :: [Int]
naturales = [1..]

-- main = print (take 10 naturales)

matriz :: [[Int]]
matriz = [[1,2,3],
         [4,5,6],
         [7,8,9]]
-- main = print matriz

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)