import System.Random


type SolucionReinas = (Int, Tablero)

type Tablero = [[Int]]

generaPoblacion :: Int -> IO [[Double]]
generaPoblacion n = do
    individuo <- generaIndividuo (n*n)
    return (parteLista individuo n)

generaIndividuo :: Int -> IO [Double]
generaIndividuo n = do
  gen <- newStdGen
  let xs = randomRs (0,1) gen
  return (transforma (take n xs))

transforma :: [Double] -> [Double]
transforma xs = [if (x > 0.9) then 1 else 0 | x <- xs]
  
parteLista :: [a] -> Int -> [[a]]
parteLista [] _ = []
parteLista xs n = (take n xs) : parteLista (drop n xs) n


muta::[[Int]]->[[Int]]
muta [] = []
muta (x:xs) = (mutaAux x):(muta xs)

mutaAux:: [Int] -> [Int]
mutaAux xs 
            | (sum xs) == 1 = []
            | (sum xs) == 0 = []
 -- No completado

fitness:: [Int] -> Int
fitness xs 
    | r < 45 = r
    | otherwise = 9283489
    where r = sum xs
-- si hay 2 en una fila, penaliza mucho, si hay coincidencia de reinas, no tanto
