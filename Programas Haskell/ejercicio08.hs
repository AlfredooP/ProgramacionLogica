-- Elabore un programa que solicite el ingreso por hora y el numero de horas trabajadas, luego muestre el total a pagar.
getDouble :: IO Double
getDouble = do
    s <- getLine
    return (read s)

main = do
    putStrLn "Cual es el pago por hora?"
    pago <- getDouble
    putStrLn "Cuantas horas trabajaste?"
    horas <- getDouble
    putStrLn  "Pago total: "
    let total = pago * horas
    print total