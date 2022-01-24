--Los valores de cada cosa se pediran por pantalla desde la clase aplicacion:
funcionMoch :: IO ()
funcionMoch = do
    putStrLn "\nFunción Mochila seleccionada"
    putStr "\nIndique temperatura actual (se recomienda 1000): "
    temperatura <- getLine
    putStr "\nIndique número de enfriamientos: "
    enfriamiento <- getLine
    putStr "\nIndique número de iteraciones: "
    iteracion <- getLine
    putStrLn "\nEjecutando algoritmo..."
    let t = read temperatura :: Int
    let nEnf = read enfriamiento :: Int
    let nIte = read iteracion :: Int
    return ()

funcionGenetico :: IO ()
funcionGenetico = do 
    putStrLn "\nFuncion Algoritmo genetico seleccionado"
    putStrLn "\n indique el numero de reinas"
    reinas <- getLine
    let n = read reinas :: Int
    return ()

main :: IO ()
main = do 
    putStrLn "\n Seleccione el problema que desea resolver: "
    putStrLn "\t1. Problema mochila"
    putStrLn "\t2. N reinas"
    putStrLn "\t3 Salir"
    putStr "\nEscriba el número asociado a la selección: "
    o <- getLine
    case o of "1" -> funcionMoch
              "2" -> funcionGenetico
              "3" -> return ()
              _ -> putStrLn "\nSeleccione un problema"