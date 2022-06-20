-- Programación Declarativa 2020/21
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Septiembre                                     9 de Septiembre de 2021
-- ----------------------------------------------------------------------
-- Lee el fichero instrucciones.pdf adjunto en la carpeta material para 
-- ver cómo entregar el examen.
-- ----------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- ----------------------------------------------------------------------

import Data.Array
import Data.List
import Control.Exception (catch, SomeException)

-- ----------------------------------------------------------------------
-- Ejercicio 1. (3,5 puntos)
-- Una cadena de ADN se puede descomponer en K-mers, es decir, todas las
-- subcadenas de longitud k. Por ejemplo, los 3-mers de la cadena
-- AATGAG son: AAT, ATG, TGA, GAG. El concepto de K-mer se utiliza
-- para algoritmos como los de ensamblado de ADN, donde dos lecturas
-- cortas de una cadena se unen para recomponer la cadena original.

-- Ejercicio 1.1. (1 punto) Define la función (kmers k cs), tal que
-- devuelva todos los k-mers (subcadenas de tamaño k) de la cadena cs.
-- Usa tan solo funciones de orden superior para recorrer las cadenas,
-- si usas listas por comprensión o recursión, se valorará con 0,5 puntos.
-- Se permiten usar funciones de Data.List.
-- Por ejemplo,
-- > kmers 3 "ATAC"       
-- ["ATA","TAC"]
-- > kmers 2 "ATAC"
-- ["AT","TA","AC"]
-- > kmers 4 "ATACCGTAA"       
-- ["ATAC","TACC","ACCG","CCGT","CGTA","GTAA"]

kmers :: Int -> [a] -> [[a]]
kmers k = takeWhile ((>=k).length) . map (take k) . tails

-- Otra solución
kmers' :: Int -> [a] -> [[a]]
kmers' k cs = filter (\cs -> length cs == k) $ subsequences cs


-- Ejercicio 1.2. (1 punto) Define la función (reconstruye css) tal 
-- reciba una lista de k-mers y devuelva la cadena original 
-- correspondiente. Pista: observa que el primer elemento de cada
-- k-mer forma parte de la cadena original, menos el del último.
-- Puedes usar recursión, comprensión u orden superior.
-- Por ejemplo,
-- > reconstruye ["ATA","TAC"]                              
-- "ATAC"
-- > reconstruye ["AT","TA","AC"]
-- "ATAC"
-- > reconstruye ["ATAC","TACC","ACCG","CCGT","CGTA","GTAA"]
-- "ATACCGTAA"

reconstruye :: [[a]] -> [a]
reconstruye css = map head (init css) ++ (last css)

reconstruye' :: [[a]] -> [a]
reconstruye' css = [head cs | cs <- (init css)] ++ (last css)

-- Ejercicio 1.3 (1,5 puntos) Define la función (ensambla k cs1 cs2),
-- tal que ensamble en una sola cadena las cadenas cs1 y cs2.
-- El proceso es el siguiente:
--   * si k<=2 devolver Nothing
--   * si k>3, hacer lo siguiente:
--      + buscar un k-mer en cs1 que sea igual al primer k-mer de cs2
--      + si no se encuentra ninguno, repetir el proceso con k-1
--      + si se ha encontrado un k-mer enla posición i de cs1, devolver
--        simplemente la reconstrucción de la cadena usando los i-1 primeros
--        k-mers de cs1 y los k-mers de cs2
-- Por ejemplo,     
-- > ensambla 3 "ATTA" "TTAC"
-- Just "ATTAC"
-- > ensambla 5 "ATTAGAT" "TAGATAA"
-- Just "ATTAGATAA"
-- > ensambla 4 "ATTAT" "ATCGATAC"  
-- Nothing

ensambla :: Eq a => Int -> [a] -> [a] -> Maybe [a]
ensambla k cs1 cs2 
  | k <= 2 = Nothing 
  | null cks = ensambla (k-1) cs1 cs2 
  | otherwise = Just (reconstruye ks)
    where kmers1 = kmers k cs1
          kmers2 = kmers k cs2
          cks = [ i | (i,ks1) <- zip [1..] kmers1, ks1== head kmers2] 
          i = head cks 
          ks = take (i-1) (kmers1) ++ kmers2
          
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 2. (2 puntos) Dada la definición de tipo de dato para
-- expresiones booleanas siguiente:

data Expr = V String      -- variable
          | N Expr        -- operador NO (NOT)
          | O Expr Expr   -- operador O (OR)
          | Y Expr Expr   -- operador Y (AND)
          deriving (Eq, Show)

-- define la función (disyuntivoMorgan expr) tal que reemplace todas las
-- conjunciones (operador Y) por disyunciones (operador O) usando las
-- siguientes leyes de Morgan:
--   * no (A y B)      --> (no A) o (no B)      -- ley 1
--   * (no A) y (no B) --> no (A o B)           -- ley 2
-- La expresión devuelta por esta función no debe contener dobles 
-- negaciones; es decir (N (N e)) --> e
-- Por ejemplo,
-- > disyuntivoMorgan (N (Y (V "A") (V "B")))            -- ley 1
-- O (N (V "A")) (N (V "B"))
-- > disyuntivoMorgan (Y (N (V "A")) (N (V "B")))        -- ley 2
-- O (V "A") (V "B")
-- > disyuntivoMorgan (N (Y (N (V "A")) (V "B")))        -- ley 1
-- O (V "A") (N (V "B"))
-- > disyuntivoMorgan (Y (V "A") (N (V "B")))            -- ley 2
-- O (N (V "A")) (V "B")
-- > disyuntivoMorgan expr                          -- ver árbol abajo
-- O (N (O (O (N (V "A")) (N (V "B"))) (N (V "C")))) 
--   (N (O (V "A") (O (N (V "C")) (N (V "B")))))


-- Este ejemplo es el definido en expr, y su transformación después
-- de llamar a disyuntivoMorgan:
--
--             Y                      O
--           /  \                    / \
--          O    O                  N   N
--         / \   | \               /     \
--        N   N "A" N    -->      O       O
--        |   |     |            / \      | \
--        Y  "C"    Y           O   N    "A" O
--       / \       / \         / \   \      / \
--     "A" "B"   "C" "B"      N   N  "C"   N   N
--                            |   |        |   |
--                           "A" "B"      "C" "B"
--                                       
--                                     
expr :: Expr
expr = Y (O (N (Y (V "A") (V "B"))) (N (V "C"))) (O (V "A") (N (Y (V "C") (V "B"))))


disyuntivoMorgan :: Expr -> Expr
disyuntivoMorgan e = elimDobleNot (disyMorgan e)

disyMorgan :: Expr -> Expr
disyMorgan (V s) = V s
disyMorgan (O e1 e2) = O (disyMorgan e1) (disyMorgan e2)
disyMorgan (N (Y e1 e2)) = O (N (disyMorgan e1)) (N (disyMorgan e2))
disyMorgan (N e) = N (disyMorgan e)
disyMorgan (Y e1 e2) = O (N (disyMorgan e1)) (N (disyMorgan e2))

elimDobleNot :: Expr -> Expr
elimDobleNot (N (N e)) = elimDobleNot e
elimDobleNot (N e) = N (elimDobleNot e)
elimDobleNot (Y e1 e2) = Y (elimDobleNot e1) (elimDobleNot e2)
elimDobleNot (O e1 e2) = O (elimDobleNot e1) (elimDobleNot e2)
elimDobleNot (V s) = V s

-- ----------------------------------------------------------------------


-- ----------------------------------------------------------------------
-- Ejercicio 3. (2 puntos) Una matriz cuadrada de dimensión n>1 es
-- Hessenberg superior si todos los elementos bajo la diagonal -1 (es 
-- decir, la diagonal debajo de la principal) son nulos. También se dice
-- que es Hessenberg inferior si todos los elementos sobre la diagonal 1 
-- (por encima de la diagonal encima de la principal) son nulos. Por 
-- ejemplo, msup es Hessenberg superior mientras que minf es inferior:
-- 
--         | 1 2 3 4 |            | 1 2 0 0 |
--  msup = | 5 6 7 8 |     minf = | 3 4 5 0 |
--         | 0 9 1 2 |            | 6 7 8 9 |
--         | 0 0 3 4 |            | 1 2 3 4 |

-- define las funciones (hessSup m) y (hessInf m) tal que indiquen si
-- la matriz m es Hessenberg superior o inferior, respectivamente. Usar
-- un tipado en las funciones lo más genérico posible para el problema.
-- Por ejemplo,
-- > hessSup msup
-- True
-- > hessSup minf
-- False

msup, minf :: Array (Int,Int) Int 
msup = listArray ((1,1),(4,4)) [1, 2, 3, 4, 
                                5, 6, 7, 8, 
                                0, 9, 1, 2, 
                                0, 0, 3, 4]
minf = listArray ((1,1),(4,4)) [1, 2, 0, 0, 
                                3, 4, 5, 0, 
                                6, 7, 8, 9, 
                                1, 2, 3, 4]                                

hessSup :: (Eq a, Num a) => Array (Int,Int) a -> Bool 
hessSup m = n==nc && all rangoElemsCero (zip [3..n] [1..n-2])
  where (n,nc) = (snd . bounds) m
        rangoElemsCero (i,j) = and [m!(i,j')==0 | j' <- [1..j]]

hessInf :: (Eq a, Num a) => Array (Int,Int) a -> Bool 
hessInf m = n==nc && all rangoElemsCero (zip [1..n-2] [n-2,n-1..1])
  where (n,nc) = (snd . bounds) m
        rangoElemsCero (i,j) = and [m!(i,j')==0 | j' <- [j..n]]

-- otra solución

hessSup' :: (Eq a, Num a) => Array (Int,Int) a -> Bool 
hessSup' m = a==b && and [m!(i,j) == 0 | i <- [1..a], j <- [1..b], j < i-1]
    where (_,(a,b)) = bounds m

hessInf' :: (Eq a, Num a) => Array (Int,Int) a -> Bool 
hessInf' m = a==b && and [m!(i,j) == 0 | i <- [1..a], j <- [1..b], j > i+1]
    where (_,(a,b)) = bounds m

-- otra solución
hessSup'' :: Array (Int,Int) Int -> Bool
hessSup'' m = all (==0) [m ! i | i <- indices]
   where 
      n = fst (snd (bounds m))
      indices = [(i, j) | i <- [3..(n)], j <- [1..(i -2)]]

-- solo hizo falta invertir i y j en las listas de indices
hessInf'' :: Array (Int,Int) Int -> Bool
hessInf'' m = all (==0) [m ! i | i <- indices]
   where 
      n = fst (snd (bounds m))
      indices = [(j, i) | i <- [3..(n)], j <- [1..(i -2)]]

-- ----------------------------------------------------------------------


-- ----------------------------------------------------------------------
-- Ejercicio 4. (2,5 puntos) Define un pequeño programa (con la función
-- main) tal que solicite el nombre de un fichero por pantalla al usuario,
-- y una vez introducido:
--    * si el nombre del fichero no acaba en ".csv" debe mostrar un mensaje
--      de error y volver a solicitar el nombre.
--    * si el fichero no existe, debe mostrar un mensaje de error y repetir
--    * en otro caso, se debe comprobar cuantas filas se pueden fitrar 
--      porque los valores en la primera columna estén repetidos. Este
--      filtrado debería dejar tan solo una fila, por lo que solo el resto
--      de filas suman a la cuenta final.
--    * mostrar por pantalla cuantas filas se pueden filtrar por valores
--      repetidos en la primera columna, y acabar
-- Nota: los valores del fichero están separados por espacios, no por comas,
-- por lo que se puede usar la función estándar de lectura de ficheros.
-- 
-- Ejemplos de interacción (usa los ficheros adjuntos):
-- > main
-- Nombre de fichero csv: fichero.txt       <-- introducido como ejemplo
-- El fichero no es un .csv
-- Nombre de fichero csv: fichero.csv       <-- introducido como ejemplo
-- fichero.csv: openFile: does not exist (No such file or directory)
-- El fichero no existe
-- Nombre de fichero csv: fichero1.csv      <-- introducido como ejemplo
-- Se pueden filtrar 3 filas
-- > main
-- Nombre de fichero csv: fichero2.csv      <-- introducido como ejemplo
-- Se pueden filtrar 0 filas
-- > main
-- Nombre de fichero csv: fichero3.csv      <-- introducido como ejemplo
-- Se pueden filtrar 470 filas

main :: IO ()
main = do
  putStr "Nombre de fichero csv: "
  fs <- getLine
  if (take 4 (reverse fs) /= "vsc.") then do
    putStrLn "El fichero no es un .csv"
    main 
  else do 
    csv <- catch (readFile fs)
                 (\err -> print (err::SomeException) >> return "")
    if (null csv) then do 
      putStrLn "El fichero no existe"
      main 
    else do
      let lss = tail (lines csv)
      let n = filtraFilas lss
      putStrLn $ "Se pueden filtrar " ++ (show n) ++ " filas"

filtraFilas :: [String] -> Int
filtraFilas lss = repetidos (map (head.words) lss)
  where repetidos [] = 0
        repetidos (cs:css) | elem cs css = 1 + repetidos css 
                           | otherwise = repetidos css