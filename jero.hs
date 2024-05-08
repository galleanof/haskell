
divisors :: Int -> [Int]
divisors x = [xs|xs <- [1..x] , mod x xs == 0] 

matches :: Int -> [Int] -> [Int]
matches x xs = [t|t<-xs,t == x]

cuadrupla :: Int -> [(Int,Int,Int,Int)]
cuadrupla x = [(a,b,c,d)|a <- [0..x] , b <- [0..x],c <- [0..x],d <- [0..x] , a^2 + b^2 == c^2 + d^2 ]

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]

suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

alguno :: [Bool] -> Bool
alguno [] = False
alguno (x:xs) = if x==True then True else alguno xs

todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) = if x==True then todos xs else False

serie [] = [[]]
serie (x:xs) = [] : map (x :) (serie xs)

collect :: Eq k => [(k,v)] -> [(k,[v])]
collect [] = []
collect ((k,v):xs) = (k , coll k ((k,v):xs)) : (collect (borrar k xs))

borrar ::Eq k => k -> [(k,v)] -> [(k,v)]
borrar k [] = []
borrar k ((c,v):xs) = if k /= c then (c,v) : borrar k xs else borrar k xs 

coll :: Eq k => k -> [(k, v)] -> [v]
coll _ [] = []
coll k ((c,v):xs) = if k == c then v : coll k xs else coll k xs

--ror :: Int -> [Int] -> [Int]
--ror _ [] = []
--ror 0 xs = xs
--ror n xs = ror (n-1) () 

paresIguales :: Int -> Int -> Int -> Int -> Bool
paresIguales a b c d = (a == b && c == d) || (a == c && b == d) || (a == d && b == c)

isosceles :: Int -> Int -> Int -> Bool
isosceles a b c = case (a,b,c) of        
                (a,b,c) | (a==b) || (a==c) || (b==c)-> True
                        | otherwise -> False

upto :: Int -> Int -> [Int]
upto n m 
      | (n>m) = []
      | otherwise = n : upto (n+1) m

eco :: [Char] -> [Char]
eco [] = []
eco xs = eco' 1 xs

eco' :: Int -> [Char] -> [Char]
eco' n [] = []
eco' n (x:xs) = replicate' n x ++ eco' (n+1) xs

replicate' :: Int -> Char -> [Char]
replicate' 0 _ = []
replicate' n c = c : replicate' (n-1) c

cambios :: Eq a => [a] -> [Int]
cambios [] = []
cambios xs = [ i | (i,c) <- zip [0..] xs, i <=  length xs - 1 && c /= xs !! (i + 1) ] 
--tiene error

oblongo :: [Int]
oblongo = [x*(x+1)|x<-[1..5]]

abundantes :: [Int]
abundantes = [x|x <- [1..60] , x < sum (divsNro x)]

divsNro :: Int -> [Int]
divsNro 0 = []
divsNro x = [n|n <- [1..(x-1)] , x `mod` n == 0 ]

--eco :: [Char] -> [Char]
--eco [] = []
--eco xs =  [replicate i x|(x,i) <- zip xs [1..length xs]] 

euler :: Int -> Int
euler x = sum ([h*3|h <- [1..(x-1)], (h*3) <  x] ++ [ t*5 |t <- [1..(x-1)] , (t*5) < x ])
--se buguea si dejo del [1..] aunque ponerle un limite no tenga sentido

mapMio f xs = foldr (\y ys -> f y : ys ) [] xs

filterMio f xs = foldr (\y ys -> if f y == True then y : ys else ys) [] xs

unZipMio xs = foldr (\( x , y ) (xs', ys') -> (x : xs',y : ys')) ([],[]) xs

pair2ListMio (x,xs) = foldr (\ y ys -> (x,y) : ys) [] xs

maxL (a,b) (c,d) = if b-a >= d-c then (a,b) else (c,d)

maxSecMio xs = foldr (\ (x,y) (xs',ys') -> maxL (x,y) (xs',ys') ) (0,0) xs

funcion1 :: Bool -> (Int -> Bool)
funcion1 b = (\x -> if x < 10 && b == True then True else False)

funcion2 :: Char -> Char
funcion2 a = a

funcion3 :: Int -> (Int -> Bool) -> [Int]
funcion3 x f = if f x then x:[x] else [x]

funcion4 :: [a] -> (a -> [b]) -> [b]
funcion4 [] _ = [] 
funcion4 [x] f = f x
funcion4 (x:y:xs) f = f x ++ f y 

import Data.Maybe (fromJust)

type Color =  (Int,Int,Int) 

mezclar :: Color -> Float
mezclar (x,y,z) = fromIntegral (x+y+z) / 3.0

data Linea =  Linea { contenido :: String, cursor :: Int } deriving (Show)

vacia :: Linea
vacia = Linea "" 0 

moverIzq :: Linea -> Linea
moverIzq (Linea x 0) = Linea x 0
moverIzq (Linea x y) = if (y >= length x || y < 0) then moverIni (Linea x y) else Linea x (y-1)

moverDer :: Linea -> Linea 
moverDer (Linea x 0) = Linea x 0
moverDer (Linea x y) = if (y >= length x || y < 0) then moverIni (Linea x y) else Linea x (y+1)

moverIni :: Linea -> Linea
moverIni (Linea x y) = Linea x 0

moverFin :: Linea -> Linea
moverFin (Linea x y) = Linea x (length x) 

insertar :: Char -> Linea -> Linea
insertar c (Linea xs y) = Linea (ins c y xs) (y+1)
 
ins :: Char -> Int -> [Char] -> [Char]
ins c 0 xs = c:xs
ins c n (x:xs) = x : ins c (n-1) xs

borrar :: Linea -> Linea
borrar (Linea xs y) = Linea (bor y xs) (y-1)

bor :: Int -> [Char] -> [Char]
bor 0 (x:xs) = xs
bor 2 (x:y:xs) = x:xs 
bor y (x:xs) = x : (bor (y-1) xs)


data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

headCL :: CList a -> Maybe a
headCL (CUnit x) = Just x 
headCL (Consnoc x y z) = Just x 

tailCL :: CList a -> Maybe (CList a)
tailCL (CUnit _) = Nothing
tailCL (Consnoc _ EmptyCL y) = Just (CUnit y)
tailCL (Consnoc _ (CUnit z) y) = Just (Consnoc z (EmptyCL) y)
tailCL (Consnoc _ xs y) = Just (Consnoc (fromJust (headCL xs)) (fromJust (tailCL xs)) y)

isEmpty :: CList a -> Bool
isEmpty (EmptyCL) = True
isEmpty _ = False

isCUnit :: CList a -> Bool
isCUnit (CUnit x) = True
isCUnit _ = False

reverseCL :: CList a -> CList a
reverseCL (EmptyCL) = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x (EmptyCL) y) = Consnoc y (EmptyCL) x
reverseCL (Consnoc x (CUnit y) z) = Consnoc z (CUnit y) x
reverseCL (Consnoc x xs z) = Consnoc z (reverseCL xs) x

concatCL :: CList (CList a) -> CList a
