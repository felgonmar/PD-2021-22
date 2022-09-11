module Genetico(
  generaPoblacion,
)where
import System.Random
import Data.List


mutaciones = 9
probabilidad = 0.8

-- FUNCIONES PRINCIPALES
generaPoblacion :: Int -> IO ()
generaPoblacion n = do
    individuo <- generaIndividuo (n*n)
    putStrLn ("gen: " ++ show ((parteLista individuo n)))
    s <- solucion (parteLista individuo n)
    putStrLn ("solucion: ")
    print s
    if(s > 0)
      then do
        putStrLn "NO ES SOLUCION. Empezando mutaciones: "
        mutacion (parteLista individuo n)        
    else 
      do
        putStrLn "SOLUCION CORRECTA"

    return ()



-- Funcion si fallamos el primer gen
mutacion :: [[Double]] -> IO()
mutacion xs = do
    s <- solucion (muta xs)
    putStrLn "MUTACION COMPLETA"
    putStrLn "SOLUCION NUEVA: "
    print s

        
    return ()


-- FUNCIONES AUXILIARES
generaIndividuo :: Int -> IO [Double]
generaIndividuo n = do
  gen <- newStdGen
  let xs = randomRs (0,1) gen
  return (transforma (take n xs) probabilidad)

transforma :: [Double] ->Double -> [Double]
transforma xs n = [if (x > n) then 1 else 0 | x <- xs] 
  
parteLista :: [a] -> Int -> [[a]]
parteLista [] _ = []
parteLista xs n = (take n xs) : parteLista (drop n xs) n


muta::[[Double]]->[[Double]]
muta [] = []
muta (x:xs) = (mutaAux x):(muta xs)

mutaAux:: [Double] -> [Double]
mutaAux xs  
  | (sum xs) == 1 =  xs 
  | otherwise = transforma (take (length xs) xs) 0.7
 
-- Definicion de la funcion solucion, le pasamos transpose para comprobar los horizontales también
solucion xs =
  return (fitness xs (transpose xs))

fitness :: [[Double]] ->  [[Double]] ->  Double
fitness [] _ = 0 
fitness (x:xs) (ts:tss) = fitness' x + fitness' ts +  fitness xs tss


fitness':: [Double] ->  Double
fitness' xs 
    | r ==1 = 0
    | otherwise = 928348939244.00
    where r = sum xs
-- si hay 2 en una fila, penaliza   
