module Exploradores (Explorador, AB(Nil,Bin), RoseTree(Rose), foldNat, foldRT, foldAB, expNulo, expId, expHijosRT, expHijosAB, expTail, ifExp, singletons, sufijos, inorder, preorder, postorder, dfsRT, ramasRT, hojasRT, listasQueSuman, listasDeLongitud, (<.>), (<^>), (<++>), (<*>)) where

import Prelude hiding ((<*>))

--Definiciones de tipos

type Explorador a b = a -> [b]

data AB a = Nil | Bin (AB a) a (AB a) deriving Eq

data RoseTree a = Rose a [RoseTree a] deriving Eq

-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show x = concatMap (++"\n") (padTree 0 x)

padTree :: Show a => Int -> RoseTree a -> [String]
padTree i (Rose x ys) =  ((pad i) ++  (show x) ) : (concatMap (padTree (i + 4)) ys)

pad :: Int -> String
pad i = replicate i ' '


instance Show a => Show (AB a) where
  show = padAB 0 0
  
padAB _ _ Nil = ""
padAB n base (Bin i x d) = pad n ++ show x ++ padAB 4 (base+l) i ++ "\n" ++ padAB (n+4+base+l) base d where l = length $ show x


--Ejercicio 1
expNulo :: Explorador a b
expNulo = const []

expId :: Explorador a a
expId = pure

expHijosRT :: Explorador (RoseTree a) (RoseTree a)
expHijosRT (Rose a xs) = xs

expHijosAB :: Explorador(AB a) (AB a)
expHijosAB Nil = []
expHijosAB (Bin i v d) = [i, d]

expTail :: Explorador [a] a
expTail [] = []
expTail (x:xs) = xs

--Ejercicio 2
foldNat :: b -> (Integer -> b -> b) -> Integer -> b
foldNat base f 0 = base
foldNat base f m = f m (foldNat base f (m-1))

foldRT :: (a -> b) -> (a -> [b] -> b) -> (RoseTree a) -> b
foldRT f g (Rose a []) = f a
foldRT f g (Rose a xs) = g a $ map (foldRT f g) xs

foldAB :: b -> (b -> a -> b -> b) -> (AB a) -> b
foldAB base f Nil = base
foldAB base f (Bin izq v der) = f (foldAB base f izq) v (foldAB base f der)

--Ejercicio 3
singletons :: Explorador [a] [a]
singletons = foldr (\a ys -> [a]:ys) []

sufijos :: Explorador [a] [a]
sufijos = foldr (\a (y:ys) -> (a:y):y:ys) [[]]

--Ejercicio 4
listasQueSuman :: Explorador Integer [Integer]
listasQueSuman 0 = [[]]
listasQueSuman n = [(n - x):ys | x <- [0..n-1], ys <- listasQueSuman x]

-- El esquema foldNat no es adecuado para resolver este problema porque son necesarios resultados
-- no solo para n-1, sino tambien para n-x (concretamente, no es posible simplemente colocar el valor
-- acumulado que se le pasa a la funcion de fold en el lugar de "listasQueSuman x", pues el x es variable)
-- La forma de implementar esto con folds es utilizando un fold cuyo valor acumulado sea
-- una funcion capaz de computar listas que suman hasta el n, junto con todas las anteriores.

--Ejercicio 5
preorder :: Explorador (AB a) a
preorder = foldAB [] (\l v r -> v:l ++ r)

inorder :: Explorador (AB a) a
inorder = foldAB [] (\l v r -> l ++ v:r)

postorder :: Explorador (AB a) a
postorder = foldAB [] (\l v r -> l ++ r ++ [v])

--Ejercicio 6
dfsRT :: Explorador (RoseTree a) a
dfsRT = foldRT pure (\a bs -> a : concat bs)

hojasRT :: Explorador (RoseTree a) a
hojasRT = foldRT pure (\_ -> concat)

ramasRT :: Explorador (RoseTree a) [a]
ramasRT = foldRT (pure . pure) (\a -> (map (a:)) . concat)

--Ejercicio 7
ifExp :: (a->Bool) -> Explorador a b -> Explorador a b -> Explorador a b
ifExp cond f g a
    | cond a = f a
    | otherwise = g a

--Ejercicio 8
(<++>) :: Explorador a b -> Explorador a b -> Explorador a b
(<++>) f g a = f a ++ g a 

--Ejercicio 9
(<.>) :: Explorador b c -> Explorador a b -> Explorador a c
(<.>) g f = concat . (map g) . f
 
--Ejercicio 10
(<^>) :: Explorador a a -> Integer -> Explorador a a
(<^>) f = foldNat pure (\_ g -> (<.>) f g)

--Ejercicio 11 (implementar al menos una de las dos)
listasDeLongitud :: Explorador Integer [Integer]
listasDeLongitud = undefined

(<*>) :: Explorador a a -> Explorador a [a] 
(<*>) f a = takeWhile (not . null) $ iterate (concat . (map f)) [a]