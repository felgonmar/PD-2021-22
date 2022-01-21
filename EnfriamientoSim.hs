module EnfriamientoSim
(

)where

import System.Random



--Calculo de la poblacion inicial
{-
Al tomar varios elementos 
-}
{-generaPoblacion :: Int -> IO [(Double,Double)]
generaPoblacion n = do
  --El newStdGen es un generador de numeros aleatorios
  gen <- newStdGen   
  let xs = randomRs () gen
  return ()  -}
    --necesitamos el return con las monadas
  

--Funcion sorteo
--Funcion experimento
--Mejor es nuestra funcion fitness que compara los valores que le demos de entrada
--La funcion de aceptacion devolvera un bool que en el caso de que sea true actualizara los 
--  valores de valor_actual.
--Funcion aceptar_e_s (valor_candidata, valor_actual, Temper, mejor)
--  return (mejor(valor_candidata,valor actual)or
---               sorteo(math.exp(-abs(valor_candidata-valorActual)/Temper)))

--Funcion para descubrir una vecino a la solucion

-- ===================================
--      FUNCIONES AUXILIARES
-- ===================================
-- le pasaremos la lista de objetos de la compra
vecinos :: [a] -> IO [a]
vecinos xs = do
  --la lista de vecinos vendra dada por vecinosAux
    let vecinoInicial = []
    let listaInicial = xs 
    let vecinoFinal = vecinosAux vecinoInicial listaInicial
    -- si la lista no supera el limite del peso de la mochila buscar otro objeto 
    --(hasta un maximo del numero de indices que queden en la lista para evitar un bucle infinito)
    
    --de la lista ire extrayendo indices aleatorios. se añadiran a elegidos y se tendran que quitar de lista
    --debo sacar de lista el ultimo elemento añadido a vecinoFinal
    --extraigo el objeto i de la lista y se la añado a elegidos
    
    let lista = delete (head vecinoFinal) listaInicial
    return elegidos

--Pasaremos la lista de vecinos y la lista de objetos de compra, al final de la iteracion 
--el objeto seleccionado debe añadirse a vecinos y a fuera de objetos de compra
vecinosAux :: [a]->[a]->[a]
vecinosAux elegidos lista = (lista !! indice): elegidos
          where indice = randomIndice lista



randomIndice :: [a] -> IO Int
randomIndice [] = error "Lista vacia"
randomIndice list = getStdRandom $ randomR (0, length list - 1)


