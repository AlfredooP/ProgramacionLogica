--Elabore un programa Haskell para leer e imprimir una lista de numeros
-- main :: IO ()
-- main = do
--     putStrLn "Introduce los numeros de la lista separados por espacios:"
--     entrada <- getLine
--     let numeros = map read (words entrada) :: [[char]]
--     putStrLn "La lista de numeros es:"
--     print numeros

--Elabore un programa Haskell para leer e imprimir una lista de strings
main :: IO ()
main = do
    putStrLn "Ingrese una lista de strings separados por comas:"
    entrada <- getLine
    let listaStrings = words [if char == ',' then ' ' else char | char <- entrada]
    putStrLn "La lista de strings es:"
    print listaStrings