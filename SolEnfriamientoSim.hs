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
     caben_objetos,
     cabe_objeto
    ) where
import System.IO


--Definimos nuestro tipo de solucion, que vendra dado por la optimizacion del valor total
type SolucionMoch = (Int, [Objeto])

--Nuestro objetos vendran con los atributos (valor,peso,nombre) voy a dejarlo todo en INT para simplificarlo
type Objeto = (Int,Int,String)
data Lista =  Lista [Objeto] deriving (Show)


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
              _ -> putStrLn "\nSeleccione un problema"


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
              "2" -> putStrLn ("Fuimos a comprar con " ++ x 
                                ++"\nEl valor total de nuestra lista seria: " ++ valor_lista) 
              "3" -> return ()
              _ -> putStrLn "\nSeleccione un problema"



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

