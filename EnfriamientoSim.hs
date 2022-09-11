module EnfriamientoSim
  (enfriamiento_simulado,
   funcion_valor,
     sacar_valor,
     sacar_valor',
     sacar_peso,
     sacar_peso',
     caben_objetos,
     cabe_objeto
  )where
import Data.List
import System.Random
--import SolEnfriamientoSim
import Control.Monad
import System.IO

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
type Objeto = (Int,Int,String)


-- ===================================
--      FUNCIONES PRINCIPAL
-- ===================================
enfriamiento_simulado::Temperatura->Temperatura->Temperatura --tInicial, tFinal, coolingRate
                        ->Int --nIter
                             ->Int --valoracionInicial 0
                                  ->[Objeto]->[Objeto] --poblacion inicial/lista de la compra/solucion inicial
                                            ->Int --Peso limite de la mochila
                                                  ->IO() --nuestra solucion la guardamos en un fichero, por tanto return()
enfriamiento_simulado tInicial tFinal cRate nIter val lista solInicial pesoMoch  = do
  if (tInicial > tFinal)
    then 
      if (nIter /= 0)
        then do
          --la expresion <- me sirve para trabajar con datos no IO
          
          vecino <- comenzar_elegidos pesoMoch lista [] 
          print vecino
          let valorVecino = fromIntegral $ funcion_valor vecino
          print valorVecino
          let delta = fromIntegral $ val - valorVecino 
          random<-randomNumber
          if (delta < 0 || prob_aceptacion delta tempInicial random)
            then do
              putStrLn "Se ha aceptado el valor y procedemos a actualizar"
              --PROBLEMA CON LA RECURSIVIDAD ME THINKS
              enfriamiento_simulado tInicial tFinal cRate (nIter-1) valorVecino lista vecino pesoMochila

            else do
             
              enfriamiento_simulado tInicial tFinal cRate nIter val lista solInicial pesoMochila
              
       --en el siguiente else tenemos que hacer tInicial=tInicial*(1-cRAte)       
      else do
        let tInicial'=tInicial*(1-cRate)
        enfriamiento_simulado tInicial' tFinal cRate nIteraciones val lista solInicial pesoMochila
    else do
    let sol = show solInicial
    putStrLn ("LA SOLUCION ES: " ++ sol)
    return ()
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

--Sacar el valor total de nuestra solucion
funcion_valor:: [(Int,Int,String)]->Int
funcion_valor [] = 0
funcion_valor (x:xs) = (sacar_valor x) + funcion_valor xs

funcion_valorFL ::[Objeto]-> Int
funcion_valorFL xs = foldr (\x acc -> acc + (sacar_valor' x)  ) 0 xs

--Para saber el valor de un objeto
sacar_valor :: (a,b,c) -> a
sacar_valor (x,_,_) = x 
sacar_valor' :: Objeto -> Int
sacar_valor' (x,_,_) = x

--Para saber el peso de un objeto
sacar_peso :: (a,b,c) -> b
sacar_peso (_,y,_) = y 
sacar_peso' :: Objeto -> Int
sacar_peso' (_,y,_) = y

--Esta funcion sirve para detectar si aun cabe algun objeto de la bolsa
caben_objetos::Int ->[Objeto]->Bool
caben_objetos pesoMoch [] = False
caben_objetos pesoMoch lista 
    | pesoMoch < pesoObjeto = caben_objetos pesoMoch (tail lista)
    | pesoMoch >= pesoObjeto = True
    where pesoObjeto = sacar_peso' objeto
          objeto = head lista

cabe_objeto::Int->Objeto->Bool
cabe_objeto pesoMoch objeto
    |pesoMoch < (sacar_peso' objeto) = False
    |pesoMoch >= (sacar_peso' objeto) = True   


caben_objetosFL :: Int -> [Objeto]->Bool
caben_objetosFL peso xs = foldr (\x acc -> if (sacar_peso' x)>peso then False else True ) False xs 
    

--parseList::[a,b,c]

