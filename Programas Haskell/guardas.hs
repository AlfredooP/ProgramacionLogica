{- Elabore un programa Haskell que calcule el nivel de riesgo de una inversion de acuerdo a su score, si es menor
a 500 el riesgo es alto, si es entre 580 y 670 el riesgo es medio y si se encuentra entre 670 y 740 es bajo
finalmente, si se encuentra en un valor superior se considera bajo -}

riesgo::Int->String
riesgo score
    | score < 580 = "Riesgo alto"
    | score < 670 = "Riesgo medio"
    | score < 740 = "Riesgo bajo"
    | otherwise   = "Riesgo muy bajo"

{-Elabore un programa que determine el estado de una tarea, si esta completada o pendiente de acuerdo al valor recibido -}
estadoTarea::Bool->String
estadoTarea True  = "Tarea Completada"
estadoTarea False = "Tarea Pendiente"

test1 :: [Char] -> Bool
test1 ['a',_,_] = True
test1 _         = False

test2 :: [Char] -> Bool
test2 ('a':_) = True
test2 _       = False