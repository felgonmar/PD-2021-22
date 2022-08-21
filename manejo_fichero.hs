import System.IO

main = do
    putStrLn "Escriba el fichero que desea leer, NO INCLUYA \" \""
    fichero <-getLine
    contents <- readFile fichero
    let x = "5"
    --Para cuando queramos añadir la solucion 
    --writeFile "solmoch.txt" contents
    putStrLn ("Imbecil " ++ x)
 
