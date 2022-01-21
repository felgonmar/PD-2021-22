{-
Aqui buscaremos la solucion especifica para el problema con el que estemos tratando que sera
el problema de la mochila.
-}
module SolEnfriamientoSim
    (SolucionMoch,
     Objeto,
     funcion_valor,
     sacar_valor,
     sacar_valor',
     sacar_peso,
     sacar_peso',
     caben_objetos
    ) where



--Definimos nuestro tipo de solucion, que vendra dado por la optimizacion del valor total
type SolucionMoch = (Int, [Objeto])

--Nuestro objetos vendran con los atributos (id,valor,peso) voy a dejarlo todo en INT para simplificarlo
type Objeto = (Int,Int,Int)

funcion_valor:: [(Int,Int,Int)]->Int
funcion_valor [] = 0
funcion_valor (x:xs) = (sacar_valor x) + funcion_valor xs


sacar_valor :: (a,b,c) -> b
sacar_valor (_,y,_) = y

sacar_peso :: (a,b,c) -> c
sacar_peso (_,_,z) = z

caben_objetos :: Int->Int->Bool
caben_objetos pesoMoch pesoObjeto = (pesoMoch > pesoObjeto)


sacar_valor' :: Objeto -> Int
sacar_valor' (_,y,_) = y
sacar_peso' :: Objeto -> Int
sacar_peso' (_,_,z) = z

