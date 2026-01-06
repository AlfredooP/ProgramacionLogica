-- Equipo: 01
-- Maria del Carmen Bracho Felix-221000121
-- Erick Gabriel Perez Castro- 22130850
-- Alfredo Puentes Vargas- 22130803
-- Alexis Eliazith Guerrero Contreras- 21130559

-- PRACTICA 3- DEFINICION DE FUNCIONES    

-- 3.1. Listas pares impares.
-- Elabore una función que reciba una lista de 20 números enteros y la divida en dos listas una 
-- que contenga los números pares y la otra los nones.
paresImpares :: [Int] -> ([Int], [Int])
paresImpares xs = ([x | x <- xs, even x], [x | x <- xs, odd x])

-- 3.2. Números.
-- Que pida 3 números y los muestre en pantalla de mayor a menor en líneas distintas. En caso
-- de haber números iguales se muestran en la misma línea.
ordenar3 :: Int -> Int -> Int -> [Int]
ordenar3 a b c
  | a >= b && a >= c = a : ordenar2 b c
  | b >= a && b >= c = b : ordenar2 a c
  | otherwise        = c : ordenar2 a b

ordenar2 :: Int -> Int -> [Int]
ordenar2 x y
  | x >= y    = [x, y]
  | otherwise = [y, x]

mostrarOrdenados :: Int -> Int -> Int -> IO ()
mostrarOrdenados a b c = mapM_ print (ordenar3 a b c)

-- 3.3. Valida la entrada
-- Elabore una función que sólo permita introducir los caracteres S y N
validaEntrada :: Char -> Bool
validaEntrada c = c `elem` "SsNn"

-- 3.4. Propiedad triangular
-- Las longitudes de los lados de un triángulo no pueden ser cualesquiera. Para que pueda
-- construirse el triángulo, tiene que cumplirse la propiedad triangular; es decir, longitud de cada
-- lado tiene que ser menor que la suma de los otros dos lados. Definir la función triangular tal que
-- (triangular a b c) se verifica si a, b y c cumplen la propiedad triangular. 
triangular :: Int -> Int -> Int -> Bool
triangular a b c = a + b > c && a + c > b && b + c > a

-- 3.5. Mayor de edad
-- Escribir un programa que pregunte al usuario su edad y muestre por pantalla si es mayor de
-- edad o no.
mayorEdad :: Int -> String
mayorEdad edad = if edad >= 18 then "Es mayor de edad" else "No es mayor de edad"

-- 3.6. Contraseña
-- Escribir un programa que almacene la cadena de caracteres “Contraseña” en una variable,
-- pregunte al usuario por la contraseña e imprima por pantalla si la contraseña introducida por el
-- usuario coincide con la guardada en la variable sin tener en cuenta mayúsculas y minúsculas.
verificaContrasena :: String -> String -> Bool
verificaContrasena [] [] = True
verificaContrasena (x:xs) (y:ys)
  | toMin x == toMin y = verificaContrasena xs ys
  | otherwise          = False
verificaContrasena _ _ = False

-- Función auxiliar para convertir mayúsculas a minúsculas sin usar Data.Char
toMin :: Char -> Char
toMin c
  | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise             = c


-- 3.7. Primo
-- Escriba la función esPrimo :: Int -> Bool tal que, dado un número natural diga si es un número
-- primo o no.
esPrimo :: Int -> Bool
esPrimo n = n > 1 && all (\x -> n `mod` x /= 0) [2..n-1]

-- 3.8. Número de cero finales.
-- Definir, por recursión, la función ceros :: Int -> Int tal que (ceros n) es el número de ceros en los
-- que termina el número n
ceros :: Int -> Int
ceros n
  | n `mod` 10 /= 0 = 0
  | otherwise       = 1 + ceros (n `div` 10)
