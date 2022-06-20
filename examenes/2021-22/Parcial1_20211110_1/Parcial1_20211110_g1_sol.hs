-- -----------------------------------------------------------------------------
-- Programación Declarativa 2021/22
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Parcial 1 (grupo 1)                                   10 de Noviembre de 2021
-- -----------------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- -----------------------------------------------------------------------------

import Test.QuickCheck
import Data.Char
import Data.List

-- -----------------------------------------------------------------------------
-- Ejercicio 1 (2 puntos)
-- Definir el operador infijo (/++) tal que reciba dos listas xs e ys, y devuelva 
-- el resultado de concatenarlas, pero quitando el último elemento de xs y el 
-- el primero de ys. Debe tener la misma precedencia que el operador (++), el cual
-- se puede usar para la solución de este ejercicio. Por ejemplo:
-- > [] /++ [3,4]
-- [4]
-- > [1] /++ []
-- []
-- > [2,3] /++ [4,3,5] /++ [6,7]
-- [2,3,7]
-- > [1,2] /++ [3,4] ++ [5,6]
-- [1,4,5,6]
-- -----------------------------------------------------------------------------

(/++) :: [a] -> [a] -> [a]   
[] /++ [] = []
[] /++ ys = tail ys
xs /++ [] = init xs
xs /++ ys = init xs ++ tail ys
     
infixr 5 /++   

-- -----------------------------------------------------------------------------


-- -----------------------------------------------------------------------------
-- Ejercicio 2.1 (1 punto) Define la función (mediaDeMediasR xss) tal que reciba
-- una lista de listas de números, y calcule la media de las medias de las 
-- sublistas. No se calculará la media para sublistas vacías, aunque sí contará
-- para la media total; es decir, la media de las sublistas vacías es 0.
-- Define la función empleando solo RECURSIÓN para xss. Por ejemplo,
--  > mediaDeMediasR [[2,4]]
--  3.0
--  > mediaDeMediasR [[2,4],[]]
--  1.5
--  > mediaDeMediasR [[2,4],[2,3],[3,4,5,6]]
--  3.3333333333333335

mediaDeMediasR :: Floating a => [[a]] -> a 
mediaDeMediasR xss = sumaDeMediasR xss / genericLength xss

sumaDeMediasR :: Floating a => [[a]] -> a 
sumaDeMediasR [] = 0
sumaDeMediasR ([]:xss) = sumaDeMediasR xss
sumaDeMediasR (xs:xss) = (media xs) + sumaDeMediasR xss

media :: Floating a => [a] -> a 
media xs = sum xs / genericLength xs
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Ejercicio 2.2 (1 punto) Define la función (mediaDeMediasO xss) como la anterior,
-- pero usando ORDEN SUPERIOR para recorrer los elementos de xss. Por ejemplo,
--  > mediaDeMediasO [[2,4]]
--  3.0
--  > mediaDeMediasO [[2,4],[]]
--  1.5
--  > mediaDeMediasO [[2,4],[2,3],[3,4,5,6]]
--  3.3333333333333335
-- -----------------------------------------------------------------------------

mediaDeMediasO :: Floating a => [[a]] -> a 
mediaDeMediasO xss = sum (map media (filter (not.null) xss)) / genericLength xss

-- -----------------------------------------------------------------------------
-- Ejercicio 2.3 (1 punto) Comprueba con quickCheck que para cualquier lista de
-- listas no vacía y que no incluya ninguna sublista vacía, si la media de medias
-- es igual a la media de concatenación de sus listas. Si hay un contraejemplo, 
-- copia y pégalo como un comentario.
-- -----------------------------------------------------------------------------

prop_medias :: (Ord a,Floating a) => [[a]] -> Property
prop_medias xss = not (null xss) && notElem [] xss ==> mediaDeMediasO xss == media (concat xss)

-- > quickCheck prop_medias 
-- *** Failed! Falsified (after 3 tests and 3 shrinks):    
-- [[0.0,0.0],[1.0]]

-- -----------------------------------------------------------------------------


-- -----------------------------------------------------------------------------
-- Ejercicio 3 (2,5 puntos). El horario de clase se puede representar como una
-- lista de tuplas donde:
--   * la primera componente es el día de la semana, 
--   * la segunda componente una lista de pares, donde:
--      - la primera componente es un entero que representa la hora
--      - la segunda componente es una lista con nombres abreviados de 
--        asignaturas (es posible que se nos pise el horario, por eso pueden 
--        haber varias asignaturas a la misma hora).

horario :: [([Char], [(Int, [[Char]])])]
horario =  [ ("Lun",[(8,[]),(9,[]),(10,[]),(11,[]),(12,[]),(13,["TAI"]),(14,["TAI"])]),
             ("Mar",[(8,[]),(9,["IA"]),(10,["IA"]),(11,["CIMSI"]),(12,["CIMSI"]),(13,["TAI"]),(14,["TAI"])]),
             ("Mie",[(8,[]),(9,[]),(10,[]),(11,["GSI"]),(12,["GSI"]),(13,["PD"]),(14,["PD"])]),
             ("Jue",[(8,[]),(9,[]),(10,[]),(11,[]),(12,[]),(13,["IA"]),(14,["IA"])]),
             ("Vie",[(8,[]),(9,["CIMSI","GSI"]),(10,["CIMSI","GSI"]),(11,["CIMSI","GSI"]),(12,["CIMSI","GSI"]),(13,["PD"]),(14,["PD"])])]

-- Define la función (horarioAsignatura hss a), tal que reciba un horario hss
-- como el anterior y el nombre abreviado de una asignatura , y devuelva el
-- el horario de clases de dicha asignatura con el siguiente formato: una lista
-- de ternas donde la primera componente es el día, y la segunda y tercera
-- componente son la hora de inicio y hora de fin (en rangos de una hora).
-- Por ejemplo,
--  > horarioAsignatura horario "IA"
--  [("Mar",9,10),("Jue",13,14)]
--  > horarioAsignatura horario "PD"
--  [("Mie",13,14),("Vie",13,14)]
--  > horarioAsignatura horario "GSI"
--  [("Mie",11,12),("Vie",9,10),("Vie",10,11),("Vie",11,12)]
-- -----------------------------------------------------------------------------

horarioAsignatura :: [(String, [(Int, [String])])] -> String -> [(String, Int, Int)]
horarioAsignatura hss a =  [ (d1,h1,h2)  | ((d1,h1),(d2,h2))  <- zip horas (tail horas), d1==d2 ]
      where horas = [ (d,h) | (d,hs) <- hss, (h,as) <- hs, elem a as ]

-- -----------------------------------------------------------------------------


-- -----------------------------------------------------------------------------
-- Ejercicio 4 (2,5 puntos) El siguiente problema está basado en el 425
-- del proyecto Euler. Dos números positivos A y B se dice que están
-- conectados (denotado por "A ↔ B"), si se cumple una de las siguientes
-- condiciones:
--   1) A y B tienen la misma cantidad de dígitos y difieren en exactamente
--      un dígito. Por ejemplo, 123 ↔ 173
--   2) Al añadir un dígito a la izquierda de A (o B) conseguimos B (o A).
--      Por ejemplo, 23 ↔ 223 y 123 ↔ 23
-- Diremos que dos primos q y p están relacionados si existe una cadena 
-- de primos conectados entre ambos y están comprendidos entre q y p. Por
-- ejemplo, si q=2 y p=127, entonces 2 ↔ 3 ↔ 13 ↔ 113 ↔ 103 ↔ 107 ↔ 127.
-- Define el predicado (cadenaPrimosConectados xs) tal que indique si la
-- lista xs es una cadena correcta de primos conectados. Por ejemplo,
--   > cadenaPrimosConectados [2,3,13,113,103,107,127]
--   True
--   > cadenaPrimosConectados [2,3,13,22,1,127]
--   False
--   > cadenaPrimosConectados [2,3,13,113,183,107,127]
--   False
-- -----------------------------------------------------------------------------

cadenaPrimosConectados :: [Int] -> Bool
cadenaPrimosConectados ns = 
  and[ primo n && n >= head ns && n <= last ns | n <- ns ] 
  &&
  and [ conectados n m |  (n,m) <- zip ns (tail ns) ]

primo :: Integral a => a -> Bool
primo n = length (divisores n) == 2

divisores :: Integral a => a -> [a]
divisores n = [i | i <- [1..n], rem n i == 0]

conectados :: Int -> Int -> Bool
conectados x y = (length xs == length ys && dif == 1) ||
                 (extiendeX || extiendeY)
    where xs = cifras x
          ys = cifras y
          dif = sum [ 1 | (n,m) <- zip xs ys, n/=m ]
          extiendeX = (head ys:xs) == ys
          extiendeY = (head xs:ys) == xs          

cifras :: Int -> [Int]
cifras n | n < 9 = [n]
         | otherwise = cifras (div n 10) ++ [rem n 10]

-- -----------------------------------------------------------------------------

