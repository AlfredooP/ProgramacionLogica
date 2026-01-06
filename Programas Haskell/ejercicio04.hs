-- Escriba un programa que pregunte el nombre, edad, trabajo y localidad y lo muestre en pantalla
main = do
    putStrLn "Nombre:"
    name <- getLine
    putStrLn "Edad:"
    age <- getLine
    putStrLn "Trabajo:"
    job <- getLine
    putStrLn "Localidad:"
    city <- getLine
    putStrLn "-------"
    putStrLn ("EL nombre es " ++ name)
    putStrLn ("La edad es " ++ age)
    putStrLn ("El trabajo es " ++ job)
    putStrLn ("La localidad es " ++ city)
    putStrLn "-------"