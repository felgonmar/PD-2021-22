import System.IO
import System.Random
main = do
    putStrLn "Escriba el fichero que desea leer, NO INCLUYA \" \""
    fichero <-getLine
    contents <- readFile fichero
    let x = "5"
    --Para cuando queramos añadir la solucion 
    --writeFile "solmoch.txt" contents
    putStrLn ("Imbecil " ++ x)


randomNumber :: IO Double
randomNumber = getStdRandom $ randomR (0.001, 1)

prob_aceptacion:: Double->Double->Double->Bool
prob_aceptacion delta temp random =  (op > random)
    where op = (exp alt) 
          delta' = 0 - delta 
          alt = ( delta' / temp) 
        
