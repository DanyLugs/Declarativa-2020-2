module Color where
 
 import Data.List


 data Color = Rojo | Amarillo | Verde | Azul | SinColor deriving (Eq, Show)

 data Balcanes = Albania 
               | Bulgaria
               | BosniayHerzegovina
               | Kosovo 
               | Macedonia
               | Montenegro
             deriving (Eq, Show)

 type Ady = [(Balcanes, Balcanes)]

 adyacencias :: Ady
 adyacencias = [( Albania , Montenegro ) , ( Albania , Kosovo ) , ( Albania , Macedonia ), ( Bulgaria , Macedonia ) , ( BosniayHerzegovina , Montenegro ), ( Kosovo , Macedonia ) , ( Kosovo , Montenegro )]

 type Coloracion = [(Color, Balcanes)]

 colores :: [Color]
 colores = [Rojo, Amarillo, Verde, Azul]

 getColor :: Coloracion -> Balcanes -> Color
 getColor [(SinColor,_)] _ = SinColor
 getColor [(col,bal)] a = col 


 esBuena :: Ady -> Coloracion -> Bool
 esBuena [] _ = True
 esBuena ((a,b) : xs) col
   | (getColor col a) == (getColor col b) = False
   | otherwise = esBuena xs col


 esCompleta :: Ady -> Coloracion -> Bool
 esCompleta [] _ = True
 esCompleta ady [(SinColor,_)] = False
 esCompleta ady [(col,_)] = True
 esCompleta (x:xs) col
   | getColor col (fst x) == SinColor || getColor col (snd x) == SinColor = False
   | otherwise = esCompleta xs col


 aplana :: [(a,a)] -> [a]
 aplana = foldr (\x y -> (fst x) : (snd x) : y) []


 delRepPar :: (Eq b) => [(a,b)] -> [(a,b)]
 delRepPar [] = []
 delRepPar (x:xs) = x:(delRepPar (filter (\y -> (snd x) == (snd y)) xs))


 allColor :: [Balcanes] -> [Coloracion]
 allColor bal = nub $ map delRepPar $ permutations [(x,y) | x <- colores, y <- bal]


 coloraciones :: Ady -> [Coloracion]
 coloraciones ad = [x | x <- allColor $ aplana ad, esBuena ad x, esCompleta ad x]