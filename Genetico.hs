import System.Random

-- tipo solucionesReinas será lo que valga la función fitness + el tablero 
type SolucionReinas = (Double, Tablero)

probabilidad = 0.8
-- el tablero constiturá de un array de arrays de double (porque lo necesitamos para random) donde si es 1, es la posición de la reina
type Tablero = [[Double]]


generaPoblacion :: Int -> IO [[Double]]
generaPoblacion n = do
    individuo <- generaIndividuo (n*n)
    return (parteLista individuo n)

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
 

solucion :: Tablero -> SolucionReinas
solucion xs = (fitness xs, xs)

fitness :: [[Double]] -> Double
fitness [] = 0
fitness (x:xs) = fitness' x + fitness xs 

fitness':: [Double] -> Double
fitness' xs 
    | r ==1 = r
    | otherwise = 928348939244.00
    where r = sum xs
-- si hay 2 en una fila, penaliza mucho, si hay coincidencia de reinas, no tanto  
