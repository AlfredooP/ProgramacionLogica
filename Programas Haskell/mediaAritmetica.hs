-- Elabore un programa Haskell para generar una lista de numeros aleatorios de tipo entero,
-- y calcule su media aritmetica, el programa debera preguntar la cantidad de numeros a generar
-- y los valores minimo y maximo para el rango.

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Text.Printf (printf)

-- Funcion que calcula la media aritmetica de Doubles
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

main :: IO ()
main = do
    putStrLn "Introduce cuantos numeros tendra la lista: "
    nStr <- getLine
    let n = read nStr :: Int
    
    putStrLn "Introduce el valor minimo del rango: "
    loStr <- getLine
    let lo = read loStr :: Int
    
    putStrLn "Introduce el valor maximo del rango: "
    hiStr <- getLine
    let hi = read hiStr :: Int

    -- Generar n numeros aleatorios en el rango [lo, hi]
    numeros <- replicateM n (randomRIO (lo, hi))
    putStrLn $ "\nLista generada: (" ++ show n ++ " numeros): "
    print numeros

    --Calcular la media aritmetica
    let media = mean (map fromIntegral numeros)
    printf "\nMedia aritmetica: %.4f\n" media