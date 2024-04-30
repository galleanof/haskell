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

