-- ----------------------------------------------------------------
-- Tema: Cifrado César en Haskell
-- Equipo #1
-- Integrantes:
-- - Erick Gabriel Perez Castro – 22130850 
-- - María Del Carmen Bracho Félix – 221000121
-- - Alfredo Puentes Vargas - 22130803 
-- - Alexis Eliazith Guerrero Contreras – 21130559
-- Fecha: 31/10/2025
-- ----------------------------------------------------------------

import Data.Char ( ord, chr ) -- para funciones de manipulación de caracteres

-- ----------------------------------------------------------------
-- Cifrado César
-- ----------------------------------------------------------------

-- Convierte una letra minúscula a un número entero
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- Convierte un número entero a su letra correspondiente
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

-- Desplaza una letra n posiciones en el alfabeto
desplaza :: Int -> Char -> Char
desplaza n c
    | elem c ['a'..'z'] = int2let ((let2int c + n) `mod` 26)
    | otherwise          = c

-- Cifra una cadena desplazando cada letra n posiciones
codifica :: Int -> String -> String
codifica n xs = [desplaza n x | x <- xs]

-- ----------------------------------------------------------------
-- QuickCheck
-- ----------------------------------------------------------------

-- Propiedad: Al desplazar −n un carácter desplazado n, se obtiene el carácter inicial.
prop_desplaza :: Int -> Char -> Bool
prop_desplaza n xs = desplaza (-n) (desplaza n xs) == xs

-- Propiedad: Al codificar con −n una cadena codificada con n, se obtiene la cadena inicial.
prop_codifica :: Int -> String -> Bool
prop_codifica n xs = codifica (-n) (codifica n xs) == xs

-- ----------------------------------------------------------------
-- Frecuencias
-- ----------------------------------------------------------------

-- Tabla de la frecuencias de las letras en castellano, Por ejemplo, la frecuencia de la ’a’ es del 12.53 %, la de la ’b’ es 1.42 %.
tabla :: [Float]
tabla = [12.53, 1.42, 4.68, 5.86, 13.68, 0.69, 1.01, 0.70, 6.25, 0.44, 0.01, 4.97, 3.15, 6.71, 8.68, 2.51, 0.88, 6.87, 7.98, 4.63, 3.93, 0.90, 0.02, 0.22, 0.90, 0.52]

-- Calcula el porcentaje de n respecto a m
porcentaje :: Int -> Int -> Float
porcentaje n m = (fromIntegral n / fromIntegral m) * 100

-- Calcula las frecuencias de las letras minúsculas en una cadena
frecuencias :: String -> [Float]
frecuencias xs = [porcentaje (ocurrencias x xs) n | x <- ['a'..'z']]
    where n = length (minusculas xs)

-- ----------------------------------------------------------------
-- Definiciones sobre cadenas
-- ----------------------------------------------------------------

-- Lista de las letras minúsculas en una cadena
minusculas :: String -> String
minusculas xs = [x | x <- xs, elem x ['a'..'z']]

-- Número de ocurrencias de un carácter en una cadena
ocurrencias :: Char -> String -> Int
ocurrencias x xs = length [x' | x' <- xs, x == x']

-- Lista de las posiciones ocupadas por el elemento x en la lista xs. 
posiciones :: Eq a => a -> [a] -> [Int]
posiciones x xs =
    [i | (x',i) <- zip xs [0..n], x == x']
    where n = length xs - 1

-- ----------------------------------------------------------------
-- Descifrado
-- ----------------------------------------------------------------

-- Medida chi cuadrado de las distribuciones os y es
chiCuad :: [Float] -> [Float] -> Float
chiCuad os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

-- Rota una lista n posiciones a la izquierda
rota :: Int -> [a] -> [a]
rota n xs = drop n xs ++ take n xs

-- Descifra una cadena cifrada con el cifrado César utilizando análisis de frecuencias
descifra :: String -> String
descifra xs = codifica (-factor) xs
    where
    factor = head (posiciones (minimum tabChi) tabChi)
    tabChi = [chiCuad (rota n tabla') tabla | n <- [0..25]]
    tabla' = frecuencias xs


--EJEMPLOS UTILIZADOS
-- > codifica 3 "en todo la medida"
-- "hq wrgr od phglgd"
-- > descifra 3 "hq wrgr od phglgd"
-- "en todo la medida"
-- > prop_desplaza 5 'a'
-- True
-- > prop_codifica 10 "hola mundo"
-- True
