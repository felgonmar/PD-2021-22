{-
Aqui buscaremos la solucion especifica para el problema con el que estemos tratando que sera
el problema de la mochila.
-}



module SolEnfriamientoSim
    (comienzo
    ) where 
import System.IO
import EnfriamientoSim


--Definimos nuestro tipo de solucion, que vendra dado por la optimizacion del valor total
type SolucionMoch = (Int, [Objeto])

--Nuestro objetos vendran con los atributos (valor,peso,nombre) voy a dejarlo todo en INT para simplificarlo
type Objeto = (Int,Int,String)

pesoMochila::Int
pesoMochila = 5
solmoch::[Objeto]
solmoch = []
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


comienzo = do 
    putStrLn " Seleccione entre las distintas opciones: "
    putStrLn "\t1. Crear objetos para la compra"
    putStrLn "\t2. Salir"
    putStrLn "Escriba el número asociado a la selección: "
    o <- getLine
    --Necesitamos aunque sea de una lista vacia para comenzar la creacion de objetos
    --esto nos permitira tener una buena actualizacion de la lista
    case o of "1" -> crear_obj []
              "2" -> return ()
              _ -> comienzo


--Cada vez que vengamos sera con una lista a la que le iremos añadiendo productos
crear_obj ::[Objeto]-> IO()
crear_obj lista= do
    --Creamos la lista de la compra vacia
    putStrLn "CREA TU OBJETO PARA AÑADIR A LA LISTA!!!"
    putStrLn "Indique valor del objeto: "
    valor <-getLine
    putStrLn "Indique peso del objeto: "
    peso <-getLine
    putStrLn "Indique nombre del objeto: "
    nombre <-getLine
    --AHORA ASIGNEMOS BIEN LOS TIPOS Y CREEMOS EL OBJETO UNA VEZ CONSEGUIDO LOS DATOS
    let v = read valor ::Int
    let p = read peso :: Int
    let obj = [(v,p,nombre)]
    --OBJETO CONSEGUIDO
    --PASAREMOS A LA SIGUIENTE ACCION CON EL OBJETO YA AÑADIDO A LA LISTA
    siguiente_accion (obj ++ lista)
    return ()

siguiente_accion :: [Objeto]->IO()
siguiente_accion lista = do
    let x = show lista
    let valor_lista = show (funcion_valor lista)
    a <- putStrLn ("Fuimos a comprar con " ++ x)
    b <- putStrLn ("El valor total de nuestra lista seria: " ++ valor_lista)
    putStrLn " Seleccione entre las distintas opciones: "
    putStrLn "\t1. Crear objetos para la compra"
    putStrLn "\t2. Ir a comprar"
    putStrLn "\t3. Salir"
    putStrLn "Escriba el número asociado a la selección: "
   
    o <- getLine
    case o of "1" -> crear_obj lista
              "2" -> enfriamiento_simulado tempInicial tempFinal coolingRate nIteraciones valoracionInicial lista solmoch pesoMochila
              "3" -> return ()
              _ -> siguiente_accion lista




