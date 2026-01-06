{- Elabore un programa Haskell que calcule la tarifa de envio de un paquete de acuerdo a su peso en kg, si es
menor o igual a 0 kg el costo es 0, si es entre 0 y 1 kg el costo es 99, si es entre 1 y 5 kg el costo es 149 y si es mayor a 5 kg el costo es 199 -}

tarifaEnvio::Double->Double
tarifaEnvio kg =
    if kg <= 0 then 0
    else if kg <= 1 then 99
    else if kg <= 5 then 149
    else 199

