{- Escriba un programa que calcule el indice de masa corporal (IMC). 
Pregunte el peso y la altura, entonces BMI = weight (kg) / height * height -}
getDouble :: IO Double
getDouble = do
    s <- getLine
    return (read s)

main = do
    putStrLn "Peso (kg):"
    weight <- getDouble
    putStrLn "Altura (m):"
    height <- getDouble
    let bmi = weight / (height * height)
    putStrLn  "Tu indice de masa corporal es: " -- ++ (show bmi)
    print bmi