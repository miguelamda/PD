import Control.Exception
import System.Environment
import Control.Parallel.Strategies
import Control.DeepSeq

-- --------------------------------------------------------------
-- Práctica paralelismo
-- 
-- Instrucciones:
--   Para comprobar el rendimiento obtenido en cada solución,
--   hay que compilar el fichero. Para ello abrir una
--   terminal, navegar hasta la carpeta, y ejecutar el
--   siguiente comando:
--     ghc -O2 practica.hs -rtsopts -threaded
--   Si se compila correctamente, ejecutar el binario objtenido:
--     ./practica +RTS -NX -s
--   siendo X el número de procesadores, o nada para detectar
--   los procesadores disponibles automáticamente.
-- -------------------------------------------------------------

-- -------------------------------------------------------------
-- Ejercicio 1.1. Definir la función fibSer, tal que sea la
-- la función de fibonacci definida por recursión. Es decir:
--   fib(n) = fib(n-1) + fib(n-2), fib(0) = 1 y fib(1) = 1
-- -------------------------------------------------------------
  
fibSer = undefined

-- -------------------------------------------------------------
-- Ejercicio 1.2. Definir la función fibPar1, tal que sea la
-- la función de fibonacci definida por recursión empleando la
-- mónada Eval para obtener paralelismo en cada llamada recursiva.
-- Evaluar la función con distintos parámetros de paralelismo
-- y analizar el resultado.
-- -------------------------------------------------------------

fibPar1 = undefined

-- -------------------------------------------------------------
-- Ejercicio 1.3. Definir la función fibPar2, tal que sea la
-- la función de fibonacci definida por recursión empleando la
-- mónada Eval para obtener paralelismo en la evaluación
-- secuencial de la primera llamada recursiva.
-- Evaluar la función con distintos parámetros de paralelismo
-- y analizar el resultado.
-- -------------------------------------------------------------

fibPar2 = undefined
        
-- -------------------------------------------------------------
-- Ejercicio 1.4. Definir la función fibPar3 tal que sea la
-- la función de fibonacci definida por recursión empleando la
-- mónada Eval para obtener paralelismo en las llamadas recursivas,
-- hasta llegar a cierto umbral, a partir del cual se evalua de
-- forma secuencial. Evaluar la función con distintos parámetros
-- de paralelismo y analizar el resultado.
-- -------------------------------------------------------------

cutoff = undefined

fibPar3 = undefined

-- Descomentar el siguiente main para probar las funciones de fib
{-main :: IO (Int)
main = do
	let fib = fibPar3 40 
	print fib
	return fib
-}
  
-- -------------------------------------------------------------
-- Ejercicio 2.1. Definir mergesort, que reciba una lista de
-- elementos y ordene sus elementos mediante mezcla ordenada.
-- Es decir: dada una lista, partirla por la mitad, ordenarlas
-- y mezclarlas de forma ordenada.
-- -------------------------------------------------------------
 
seqMergeSort = undefined

-- -------------------------------------------------------------
-- Ejercicio 2.2. Definir mergesort, que reciba una lista de
-- elementos y ordene sus elementos mediante mezcla ordenada,
-- aplicando paralelismo mediante la mónada eval. Buscar el
-- umbral óptimo a partir del cual no merece la pena aplicar
-- paralelismo.
-- -------------------------------------------------------------

parMergeSort xs = undefined

-- Descomentar el siguiente main para probar las funciones de 
-- mergesort
{-main :: IO ([Int])
main = do
    let xs = [1000000,999999..1]
    let oxs = parMergeSort xs 
    print (length oxs)
    return (oxs)
-}

-- -------------------------------------------------------------
-- Ejercicio 3.1. Redefinir la función parMap, tal que reciba
-- una función y una lista, y devuelva una evaluación paralela
-- de f sobre los elementos de la lista (ver tema de teoría).
-- -------------------------------------------------------------

parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' = undefined

-- -------------------------------------------------------------
-- Ejercicio 3.2. Definir la función parallelMap, tal que reciba
-- una función y una lista, y devuelva una lista con la evaluación
-- de f sobre los elementos de la lista, empleando parMap'.
-- -------------------------------------------------------------

parallelMap :: (a -> b) -> [a] -> [b]
parallelMap = undefined

-- -------------------------------------------------------------
-- Ejercicio 4.1. Definir la función agrupa, tal que reciba
-- un entero n y una lista xs, y devuelva una lista de listas con
-- agrupaciones de n elementos de xs. Por ejemplo,
--   agrupa 3 [1..10] == [[1,2,3],[4,5,6],[7,8,9],[10]]
-- -------------------------------------------------------------

agrupa :: Int -> [a] -> [[a]]
agrupa = undefined

-- -------------------------------------------------------------
-- Ejercicio 5.1. Definir la función hist, tal que reciba
-- un entero bins y una lista xs, y devuelva el histograma de
-- bins intervalos sobre los elementos de la lista xs. Es decir,
-- la frecuencia de aparición de los elementos de cada intervalo
-- (un total de bins) en la lista xs. Por ejemplo,
--   hist 2 [1,2,3,2,1,2,3,4] == [5,3]
-- -------------------------------------------------------------

hist :: Int -> [ Int ] -> [ Int ]
hist = undefined

-- -------------------------------------------------------------
-- Ejercicio 5.2. Definir la función parHist, tal que reciba
-- un entero bins y una lista xs, y devuelva el histograma de
-- bins intervalos sobre los elementos de la lista xs. Definir
-- varias versiones según donde se aplique el paralelismo.
-- Evaluar qué versión es más eficiente.
-- -------------------------------------------------------------

parHist :: Int -> [ Int ] -> [ Int ]
parHist = undefined

-- Descomentar el siguiente main para probar las funciones de hist
{-
main :: IO ([Int])
main = do
    let xs = concat (replicate 10000 [1..200])
    let oxs = parHist 25 xs
    print (oxs)
    return (oxs)

-}

