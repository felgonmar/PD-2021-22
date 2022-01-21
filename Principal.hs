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
    {-solucion <- algoritmoParaSolucionar: p g t f cr 4
    let final = nuevaSolucionCF6 solucion
    let last_gen = selecciona_evaluaciones_de_generacionCF6 (length (solucion)) final
    siguiente_accion_cf6 last_gen
    return ()-}