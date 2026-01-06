{- 
Equipo: 01
Maria del Carmen Bracho Felix-221000121
Erick Gabriel Perez Castro- 22130850
Alfredo Puentes Vargas- 22130803
Alexis Eliazith Guerrero Contreras- 21130559

Ejercicio 21 Validación de tarjetas de credito.
-}

-- Parte 1: Obtener el ultimo digito de un numero y eliminarlo
lastDigit :: Integer -> Integer
lastDigit n = abs n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = abs n `div` 10

-- Parte 2: Convertir un numero en una lista de sus digitos
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = reverse (toDigitsRev n)

-- Auxiliar: para reversa
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = lastDigit n : toDigitsRev (dropLastDigit n)

-- Parte 3: Duplicar cada segundo numero de derecha a izquierda
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleFromRight (reverse xs))

-- Auxiliar: duplica cada segundo número de izquierda a derecha
doubleFromRight :: [Integer] -> [Integer]
doubleFromRight [] = []
doubleFromRight [x] = [x]
doubleFromRight (x:y:zs) = x : (2 * y) : doubleFromRight zs

-- Parte 4: Sumar todos los digitos de una lista de numeros
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
    | x < 10    = x + sumDigits xs
    | otherwise = sumDigits (toDigits x) + sumDigits xs

-- Parte 5: Validar un numero de tarjeta de credito
validate :: Integer -> Bool
validate n =
    let digits     = toDigits n
        doubled    = doubleEveryOther digits
        total      = sumDigits doubled
    in total `mod` 10 == 0
