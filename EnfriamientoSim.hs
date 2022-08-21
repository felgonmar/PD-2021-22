module EnfriamientoSim
(

)where
import Data.List
import System.Random
import SolEnfriamientoSim
import Control.Monad
import System.IO
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
--      VARIABLES AUXILIARES
-- ===================================
--ESTAS VARIABLES DEBERAN SER LLAMADAS DESDE LA PARTE DE SOL AL COMIENZO DEL ALGORITMO
pesoMochila::Int
pesoMochila = 5
--En cada iteracion se generara el vecino 
nIteraciones:: Int
nIteraciones = 10
type Temperatura = Double
tempInicial::Temperatura
tempInicial=100
tempFinal::Temperatura
tempFinal=1
coolingRate::Temperatura
coolingRate=0.1
valoracionInicial::Int
valoracionInicial= -1


-- ===================================
--      FUNCIONES PRINCIPAL
-- ===================================
enfriamiento_simulado::Double->Double->Double --tInicial, tFinal, coolingRate
                        ->Int --nIter
                             ->Int --valoracionInicial 0
                                  ->[Objeto]->[Objeto] --poblacion inicial/lista de la compra/solucion inicial
                                            ->Int --Peso limite de la mochila
                                                  ->IO() --nuestra solucion la guardamos en un fichero, por tanto return()
enfriamiento_simulado tInicial tFinal cRate nIter val lista solInicial pesoMoch  = do
  if (tInicial < tFinal)
    then do
      if (nIter /= 0)
        then do
          --la expresion <- me sirve para trabajar con datos no IO
          let nIter=nIter - 1
          vecino <- comenzar_elegidos pesoMoch lista [] 
          let valorVecino = fromIntegral $ funcion_valor vecino
          let delta = fromIntegral $ val - valorVecino 
          random<-randomNumber
          if (delta < 0 ||prob_aceptacion delta tempInicial random)
            then do
                --let val = valorVecino
                --let solInicial = vecino
                enfriamiento_simulado tInicial tFinal cRate nIter val lista solInicial pesoMochila

            else enfriamiento_simulado tInicial tFinal cRate nIter val lista solInicial pesoMochila
       --en el siguiente else tenemos que hacer tInicial=tInicial*(1-cRAte)       
        else do
          let tInicial'=tInicial*(1-cRate)
          enfriamiento_simulado tInicial' tFinal cRate nIteraciones val lista solInicial pesoMochila
    else return ()
-- ===================================
--      FUNCIONES AUXILIARES
-- ===================================

--comenzar_elegidos pretende introducir el individuo escogido de la lista e introducirlo en la lista que devolveremos
--debemos comenzar comprobando si puedo introducir el individuo en la bolsa o no
--en caso de que si: introducirlo en la bolsa, quitarlo de la lista y actualizar el peso disponible
--en caso de que no:quitarlo de la lista
--caso base cuando la lista este vacia y devolveremos bolsa
comenzar_elegidos::Int->[Objeto]->[Objeto]->IO [Objeto]
--la lista de la compra esta vacia por tanto devolvemos bolsa de la compra(res)
comenzar_elegidos pesoMoch [] bolsa = do
  return bolsa
comenzar_elegidos pesoMoch lista bolsa = do
  --sacamos un objeto random de la lista
  indi <- individuo lista
  let listaAct = delete indi lista
  let pesoIndi = sacar_peso' indi
  --actualizar pesoSobrante
  if (pesoMoch > pesoIndi)   --cabe_objeto pesoMoch indi  
    --llamada recursiva actualizando valores en caso de si
    then comenzar_elegidos (pesoMoch - pesoIndi) listaAct (indi:bolsa)
    --llamada recursiva actualizando la lista, quitando el objeto que no cabia y por tanto descartandolo sin tocar nuestra bolsa
    --mala optimizacion porque no terminara de iterar hasta que acabe con toda la lista de la compra
    else comenzar_elegidos pesoMoch listaAct bolsa 
 
    
--individuo me permite sacar un individuo de toda la lista para comprobar despues si lo puedo meter junto a los elegidos
individuo :: [Objeto]->IO Objeto
individuo [] = error "lista vacia al sacar individuo"
individuo lista = do 
    indice <- randomIndice lista
    --saco el elemento indice de la lista
    let res = (lista !! indice)
    return res

randomIndice :: [a] -> IO Int
randomIndice [] = error "Lista vacia"
randomIndice list = getStdRandom $ randomR (0, length list - 1)

randomNumber :: IO Double
randomNumber = getStdRandom $ randomR (0.001, 1)

--La formula es exp^(-delta/temperatura)
prob_aceptacion:: Double->Double->Double->Bool
prob_aceptacion delta temp random =  (op > random)
    where op = (exp alt) 
          delta' = 0 - delta 
          alt = ( delta' / temp) 


