module Exploradores (Explorador, AB(Nil,Bin), RoseTree(Rose), foldNat, foldRT, foldAB, expNulo, expId, expHijosRT, expHijosAB, expTail, ifExp, singletons, sufijos, inorder, preorder, postorder, dfsRT, ramasRT, hojasRT, listasQueSuman, listasDeLongitud, (<.>), (<^>), (<++>), (<*>)) where

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
expTail = tail

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

listasQueSuman :: Explorador Integer [Integer]
listasQueSuman = foldNat [[]] (\n b -> [(n - x):ys | x <- [0..n-1], ys <- listasQueSuman x])

--Ejercicio 5
--preorder :: undefined
preorder = undefined

--inorder :: undefined
inorder = undefined

--postorder :: undefined
postorder = undefined

--Ejercicio 6
dfsRT :: Explorador (RoseTree a) a
dfsRT = undefined

hojasRT :: Explorador (RoseTree a) a
hojasRT = undefined

ramasRT :: Explorador (RoseTree a) [a]
ramasRT = undefined

--Ejercicio 7
ifExp :: (a->Bool) -> Explorador a b -> Explorador a b -> Explorador a b
ifExp = undefined

--Ejercicio 8
(<++>) :: Explorador a b -> Explorador a b -> Explorador a b
(<++>) = undefined

--Ejercicio 9
(<.>) :: Explorador b c -> Explorador a b -> Explorador a c
(<.>) = undefined
 
--Ejercicio 10
(<^>) :: Explorador a a -> Integer -> Explorador a a
(<^>) = undefined

--Ejercicio 11 (implementar al menos una de las dos)
listasDeLongitud :: Explorador Integer [Integer]
listasDeLongitud = undefined

--(<*>) :: Explorador a a -> Explorador a [a] 
--(<*>) = undefined