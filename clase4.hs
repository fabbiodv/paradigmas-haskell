-- Composicion de funciones
-- (.)

-- (>5).(3*)

-- even.(+1).(4*)

-- Currificacion
-- Aplicacion Parcial
-- Permite poder crear nuevas funciones

-- Expresiones Lambda
-- Crear una funcion anonima (/x -> x+1)

-- Tipos de datos propios
-- Usamos data

-- Funciones constante
-- pedro = Persona 20 "pedro"

-- FUNCIONES

-- Ejercicio 1
-- busca el primer elemento en una lista que cumple con un criterio de búsqueda específico.
-- Listas por comprension
-- find' :: (a -> Bool) -> [a] -> a
-- find' criterio lista = head [elem | elem <- lista, criterio elem]

find' :: (a -> Bool) -> [a] -> a
find' criterio lista = (head . filter criterio) lista

data Politico = Politico {proyectosPresentados :: [String], sueldo :: Integer, edad :: Int} deriving (Show)

politicos =
  [ Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81,
    Politico ["tratar de reconquistar luchas sociales"] 10000 63,
    Politico ["tolerancia 100 para delitos"] 15500 49
  ]

pedro :: Politico
pedro = Politico ["tolerancia 100 para delitos"] 15500 49

{-
a) ghci> find' ((<50).edad) politicos Politico
{proyectosPresentados = ["tolerancia 100 para delitos"], sueldo = 15500, edad = 49}
-}

-- b)  alguno que haya presentado más de 3 proyectos
-- find' ((>3).length.proyectosPresentados) politicos

-- c) alguno que haya presentado algún proyecto que tenga más de 3 palabras
-- find' (any ((>3).length.words).proyectosPresentados) politicos

type Nombre = String

type Notas = [Int]

data Persona = Alumno {nombre :: Nombre, notas :: Notas}

-- 2. Definir la función promediosAlumnos/1, que dada una lista de alumnos devuelve una lista de tuplas que tenga el alumno y el promedio (Consideramos la división entera para el promedio y usamos la funcion div).
promediosAlumnos :: [Persona] -> [(Nombre, Int)]
promediosAlumnos alumnos = map (\unAlumno -> (nombre unAlumno, (promedio . notas) unAlumno)) alumnos

promedio :: Notas -> Int
promedio notas = (sum notas) `div` (length notas)

-- 3. Definir la función promediosSinAplazos/1, que dada una lista de listas, devuelve la lista de los promedios de cada lista-elemento, excluyendo los que sean menores a 6 que no se cuentan.
promediosSinAplazos :: [Notas] -> [Int]
promediosSinAplazos listaNotas = map (promedio . filter (>= 6)) listaNotas

-- 4. Definir la función aprobó/1, que dado un alumno devuelve True si el alumno aprobó. Aclaración: se dice que un alumno aprobó si todas sus notas son 6 o más.
aprobo :: Persona -> Bool
aprobo (Alumno _ notas) = all (>= 6) notas

-- 5. Definir la función aprobaron/1, que dada una lista de alumnos, devuelve los nombres de los alumnos que aprobaron.
aprobaron :: [Persona] -> [Nombre]
aprobaron alumnos = (map nombre . filter aprobo) alumnos

-- 6. Definir la función productos que dado una lista de nombres  de productos y una lista de precios, devuelve una lista de tuplas.
productos :: [Nombre] -> [Integer] -> [(Nombre, Integer)]
productos nombres precios = zip nombres precios

productos' :: [Nombre] -> [Integer] -> [(Nombre, Integer)]
productos' nombres precios = zipWith (\nom prec -> (nom, prec)) nombres precios