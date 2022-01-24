module EnfriamientoSim
(

)where
import Data.List
import System.Random
import SolEnfriamientoSim

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
vecinos :: Int->[Objeto] -> IO [Objeto]
vecinos pesoMoch[] = error "lista vacia"
vecinos pesoMoch xs = do
  --la lista de vecinos vendra dada por vecinosAux
    let vecinoInicial = []
    let listaInicial = xs 
 
    return xs

--Pasaremos la lista de vecinos y la lista de objetos de compra, al final de la iteracion 
--el objeto seleccionado debe añadirse a vecinos y a fuera de objetos de compra

--individuo me permite sacar un individuo de toda la lista para comprobar despues si lo puedo meter junto a los elegidos
individuo :: [Objeto]->IO Objeto
individuo lista = do 
    indice <- randomIndice lista
    let res = (lista !! indice)
    return res

--comenzar_elegidos pretende introducir el individuo escogido de la lista e introducirlo en la lista que devolveremos
--debemos comenzar comprobando si puedo introducir el individuo en la bolsa o no
--en caso de que si: introducirlo en la bolsa, quitarlo de la lista y actualizar el peso disponible
--en caso de que no:quitarlo de la lista
--caso base cuando la lista este vacia y devolveremos bolsa
comenzar_elegidos::Int->[Objeto]->[Objeto]->IO [Objeto]
comenzar_elegidos pesoMoch [] bolsa = do
  return bolsa
comenzar_elegidos pesoMoch lista bolsa = do
  indi <- individuo lista
  let listaAct = delete indi lista
  let pesoIndi = sacar_peso' indi
  --actualizar pesoSobrante
  if cabe_objeto pesoMoch indi  then comenzar_elegidos (pesoMoch - pesoIndi) (indi:bolsa) listaAct
    else comenzar_elegidos pesoMoch bolsa listaAct
  return bolsa
    




randomIndice :: [a] -> IO Int
randomIndice [] = error "Lista vacia"
randomIndice list = getStdRandom $ randomR (0, length list - 1)


