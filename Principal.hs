--Los valores de cada cosa se pediran por pantalla desde la clase aplicacion:
import SolEnfriamientoSim
import Genetico


funcionGenetico :: IO ()
funcionGenetico = do 
    putStrLn "\nFuncion Algoritmo genetico seleccionado"
    putStrLn "\n indique el numero de reinas"
    reinas <- getLine
    let n = read reinas :: Int
    generaPoblacion n

    return ()

main :: IO ()
main = do 
    putStrLn "\n Seleccione el problema que desea resolver: "
    putStrLn "\t1. Problema mochila"
    putStrLn "\t2. N reinas"
    putStrLn "\t3 Salir"
    putStr "\nEscriba el número asociado a la selección: "
    o <- getLine
    case o of "1" -> comienzo
              "2" -> funcionGenetico
              "3" -> return ()
              _ -> main
