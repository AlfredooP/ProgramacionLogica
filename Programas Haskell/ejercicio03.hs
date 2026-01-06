-- Elabore un programa que pregunte el nombre y muestre un saludo
main = do
    putStrLn "Hola, Cual es tu nombre?"
    name <- getLine
    putStrLn ("Hola " ++ name)