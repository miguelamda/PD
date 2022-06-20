-- -----------------------------------------------------------------------------
-- Programación Declarativa 2021/22
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Parcial 1 (grupo 2)                                   10 de Noviembre de 2021
-- -----------------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- Laboratorio/Puesto:
-- -----------------------------------------------------------------------------
-- INSTRUCCIONES PARA LA ENTREGA
-- 1. CAMBIA EL NOMBRE de este archivo por:       <uvus>.hs
--    donde "<uvus>" es tu UVUS.
-- 2. COMENTA LAS LÍNEAS CON ERRORES hasta que se pueda cargar el fichero
--    sin problemas. ESCRIBE tu nombre y apellidos en la cabecera.
-- 3. COMPRIME este archivo en un único fichero llamado EXACTAMENTE:
--      ENTREGA-<uvus>.tar.gz      (o bien)       ENTREGA-<uvus>.tar.xz
--    donde "<uvus>" es tu UVUS. No te olvides del guión después de
--    ENTREGA, y NO lo comprimas en un fichero .zip.
-- 4. REINICIA el equipo. En el menú de selección del sistema (con fondo
--    blanco), HAZ CLICK SOBRE "Enviar examen" al lado de sistema Ubuntu.
-- 5. Pregunta al profesor si ha llegado tu correo correctamente, si es
--    así, ya puedes dejar tu puesto SIN APAGAR EL EQUIPO.
-- ----------------------------------------------------------------------
-- ORIENTACIONES
-- · Escribe la solución de cada ejercicio en el hueco reservado para
--   ello.
-- · Se valorará el uso correcto de tipados para cada función definida.
-- · Puedes añadir tantas funciones auxiliares como necesites (incluyendo su 
--   signatura adecuadamente).
-- · Puedes usar otros módulos de Haskell que estén ya importados.
-- -----------------------------------------------------------------------------

import Test.QuickCheck
import Data.Char
import Data.List

-- -----------------------------------------------------------------------------
-- Ejercicio 1 (2 puntos)
-- Definir el operador infijo (/^) tal que reciba dos números reales x e 
-- y, y devuelva el exponente obtenido empleando como base el menor de x
-- e y, y como exponente el mayor de x e y.
-- Debe tener la misma precedencia que el operador **, y la asociatividad
-- requerida para que los siguientes ejemplos sean correctos. Por ejemplo:
-- > 3 /^ 2
-- 8.0
-- > 2 /^ 3
-- 8.0
-- > 2 /^ 3 /^ 4
-- 2.4178516392292583e24
-- > (2 /^ 3) /^ 4
-- 65536.0
-- -----------------------------------------------------------------------------



-- -----------------------------------------------------------------------------


-- -----------------------------------------------------------------------------
-- Ejercicio 2.1 (1 punto) Define la función (gmaxR g xs) tal que reciba una 
-- función g y una lista xs, y devuelva todo elemento x de xs que cumpla con lo
-- siguiente: si x1, x, x2 son elementos consecutivos, (g x1) es menor que (g x),
-- y (g x) es mayor que (g x2). Define la función empleando RECURSIÓN.
-- Por ejemplo,
-- > gmaxR (^2) [2,3]
-- []
-- > gmaxR (^2) [2,3,2]
-- [3]
-- > gmaxR (*(-1)) [1,2,3,1,2,35,4]
-- [1]
-- > gmaxR (/2) [1,2,3,1,2,35,4]
-- [3.0,35.0]
-- > gmaxR toUpper "hola"
-- "o"
-- -----------------------------------------------------------------------------

gmaxR = undefined

-- -----------------------------------------------------------------------------
-- Ejercicio 2.2 (1 punto) Define la función (gmaxO g xs) como la anterior,
-- pero usando ORDEN SUPERIOR para recorrer los elementos.
-- Por ejemplo,
-- > gmaxO (^2) [2,3]
-- []
-- > gmaxO (^2) [2,3,2]
-- [3]
-- > gmaxO (*(-1)) [1,2,3,1,2,35,4]
-- [1]
-- > gmaxO (/2) [1,2,3,1,2,35,4]
-- [3.0,35.0]
-- > gmaxO toUpper "hola"
-- "o"
-- -----------------------------------------------------------------------------

gmaxO = undefined

-- -----------------------------------------------------------------------------
-- Ejercicio 2.3 (1 punto) Comprueba con quickCheck para todo número x
-- positivo, y cualquier lista xs no vacía, el resultado de gmax con (g = *x)
-- y xs, tiene como mucho un tercio de elementos de xs. Si hubiese un
-- contraejemplo, copia y pégalo como un comentario.
-- -----------------------------------------------------------------------------

prop_gmax = undefined

{- La comprobación es:

-}

-- -----------------------------------------------------------------------------


-- -----------------------------------------------------------------------------
-- Ejercicio 3 (2,5 puntos). El horario de clase se puede representar como una
-- lista de tuplas donde:
--   * la primera componente es el nombre abreviado de la asignatura, 
--   * la segunda componente una lista de ternas, donde:
--      - la primera componente es el nombre del día abreviado
--      - la segunda componente es un par que indica hora y minuto de comienzo
--      - la tercera componente es un par que indica hora y minuto de finalización

horario :: [ ( String, [(String, (Int,Int), (Int,Int))] ) ]
horario = [ ("PD",    [("Mie",(12,30),(14,30)),("Mie",(15,30),(17,30)),("Vie",(12,30),(14,30))]),
            ("IA",    [("Mar",(8,30),(10,30)), ("Jue",(12,30),(14,30))]),
            ("TAI",   [("Lun",(12,30),(14,30)),("Mar",(12,30),(14,30)),("Mar",(15,30),(17,30)), ("Mar",(17,30),(19,30))]),
            ("CIMSI", [("Mar",(10,30),(12,30)),("Vie",(8,30),(10,30)), ("Vie",(10,30),(12,30))]),
            ("GSI",   [("Mie",(10,30),(12,30)),("Vie",(8,30),(10,30)), ("Vie",(10,30),(12,30))])
          ]

-- Define la función (invierteHorario hss), tal que reciba un horario hss
-- como el anterior, y devuelva el calendario de mañana (de 8:00 a 13:00)
-- indicando para cada día de la semana y cada hora, las asignaturas que tenemos
-- programadas. Si no hay asignaturas en un horario, se pone la lista vacía.
-- En concreto, se debe devolver una lista de pares, donde el primero es el día de la
-- semana, y el segundo es una lista de pares (hora,as), siendo as la lista de las 
-- asignaturas que hay en ese momento. Por ejemplo,
-- > invierteHorario horario
--    [("Lun",[(8,[]),(9,[]),(10,[]),(11,[]),(12,[]),(13,["TAI"]),(14,["TAI"])]),
--     ("Mar",[(8,[]),(9,["IA"]),(10,["IA"]),(11,["CIMSI"]),(12,["CIMSI"]),(13,["TAI"]),(14,["TAI"])]),
--     ("Mie",[(8,[]),(9,[]),(10,[]),(11,["GSI"]),(12,["GSI"]),(13,["PD"]),(14,["PD"])]),
--     ("Jue",[(8,[]),(9,[]),(10,[]),(11,[]),(12,[]),(13,["IA"]),(14,["IA"])]),
--     ("Vie",[(8,[]),(9,["CIMSI","GSI"]),(10,["CIMSI","GSI"]),(11,["CIMSI","GSI"]),(12,["CIMSI","GSI"]),(13,["PD"]),(14,["PD"])])]
-- -----------------------------------------------------------------------------

invierteHorario :: [(String,[(String,(Int,Int),(Int,Int))])]-> [(String,[(Int,[String])])]
invierteHorario = undefined
  
-- -----------------------------------------------------------------------------


-- -----------------------------------------------------------------------------
-- Ejercicio 4 (2,5 puntos) El siguiente problema está basado en el 414
-- del proyecto Euler. La rutina de Kaprekar comienza con un número n que
-- contiene 4 o menos cifras, y se calcula otro número como sigue:
--   * Si n tiene menos de 4 cifras, se extiende añadiendo ceros
--     a la izquierda hasta tener 4 cifras.
--   * Se obtienen dos números, uno ordenando las cifras de mayor a menor
--     y otro ordenando de menor a mayor. El nuevo número es la resta
--     del primero con el segundo.
-- Por ejemplo, si n=0837, entonces el siguiente es 8730-0378=8352.
-- El proceso acaba siempre con el 0 o con la constante de Kaprekar. Esta
-- constante es el 6174, ya que 7641-1467=6174.
-- Define la función (kaprekar n) tal que devuelva el número de pasos en
-- la rutina de Kaprekar comenzando con n hasta llegar al 0 o al 6174.
-- Por ejemplo,
-- > kaprekar 6174
-- 1
-- > kaprekar 837
-- 2
-- > kaprekar 64
-- 4
-- -----------------------------------------------------------------------------

kaprekar :: Int -> Int
kaprekar = undefined

-- -----------------------------------------------------------------------------
