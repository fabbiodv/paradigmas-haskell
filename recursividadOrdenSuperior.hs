-- CLASE 5

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial(n-1)

cantidad :: [Integer] -> Integer
cantidad [] = 0
cantidad (_:lista) = 1 + cantidad lista 

ultimo :: [Integer] -> Integer
ultimo [x] = x
ultimo( _ : xs ) = ultimo xs 

pertenece _ [] = False
pertenece elem (cab:cola) = elem == cab || pertenece elem cola 

maximo [elem] = elem
maximo (x:xs) = x `max` maximo xs

map' _ [] = []
map' f (x:xs) = f x : map' f xs

seleccionar _ [] = []
seleccionar f (x:xs) 
  | f x = x : seleccionar f xs 
  | otherwise = seleccionar f xs

all' _ [] = True
all' f (x:xs) = f x && all' f xs

any' _ [] = False
any' f (x:xs) = f x || any' f xs 

-- https://docs.google.com/document/d/1Ub9r_MAKtLHchrzKsau9Qnm1Ak5aiT04pgkCg-HFTqg/edit

-- 1)

data Flor = Flor { nombreFlor :: String, aplicacion:: String, cantidadDeDemanda:: Integer } deriving Show

rosa = Flor "rosa" "decorativo" 120
jazmin =  Flor "jazmin" "aromatizante" 100
violeta =  Flor "violeta" "infusión" 110
orquidea =  Flor "orquidea" "decorativo" 90

maximaFlorSegun :: ( Flor -> Integer ) -> [ Flor ] -> String

maximaFlorSegun _ [ flor ] = flor
maximaFlorSegun f flores = ( nombreFlor.maximaFlor f ) flores

flores = [ orquidea, rosa, violeta, jazmin ]

maximaFlor :: ( Flor -> Int ) -> [ Flor ] -> Flor
maximaFlor _ [flor] = flor
maximaFlor f ( flor:flores )
  | f flor >= ( f.maximaFlor f ) flores = flor
  | otherwise = maximaFlor f flores

-- {-a
-- ghci> maximaFlorSegun cantidadDeDemanda flores
-- "Rosa"
-- }

-- CLASE 6 -> 12/5

-- 1b)

ordenadas :: [ Flor ] -> Bool
ordenadas[_] = True
ordenadas ( unaFlor:otraFlor:flores ) = cantidadDeDemanda unaFlor > cantidadDeDemanda otraFlor && ordenadas ( otraFlor:flores )