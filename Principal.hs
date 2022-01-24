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
    