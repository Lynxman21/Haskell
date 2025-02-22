import Data.ByteString (elemIndexEnd)
--rekursja strukturalna
data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"

--Zadania
depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

flattenBT :: BinTree a -> [a]
flattenBT EmptyBT = []
flattenBT (NodeBT n lt rt) = [n] ++ flattenBT lt ++ flattenBT rt

mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT (f n) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a -- insert element into BinTree
insert a EmptyBT = NodeBT a EmptyBT EmptyBT
insert a (NodeBT b lt rt) =
    if a < b then (NodeBT b (insert a lt) rt)
    else (NodeBT b lt (insert a rt))

occurs :: Eq a => a -> BinTree a -> Int -- liczba wystąpień elementu w drzewie binarnym
occurs _ EmptyBT = 0
occurs x (NodeBT y lt rt) =
    if x == y then 1 + (occurs x lt) + (occurs x rt)
    else (occurs x lt) + (occurs x rt)

elemOf :: Eq a => a -> BinTree a -> Bool -- sprawdzenie, czy element znajduje się w drzewie
elemOf _ EmptyBT = False
elemOf x (NodeBT y lt rt) =
    if x == y then True
    else (elemOf x lt) || (elemOf x rt)

reflect :: BinTree a -> BinTree a -- 'odbicie lustrzane' drzewa binarnego
reflect EmptyBT = EmptyBT
reflect (NodeBT x lt rt) = NodeBT x (reflect rt) (reflect lt)

maxElemOf :: (Ord a,Num a) => BinTree a -> a
maxElemOf EmptyBT = 0
maxElemOf (NodeBT x lt rt) = 
    if x > maxi then x
    else maxi
    where maxi = max (maxElemOf lt) (maxElemOf rt)

data Expr' a = Lit' a | -- literal/value a, e.g. Lit 2 = 2
              Add' (Expr' a) (Expr' a) |
              Sub (Expr' a) (Expr' a)

eval' :: Num a => Expr' a -> a
eval' (Lit' n) = n
eval' (Add' x y) = eval' x + eval' y
eval' (Sub x y) = eval' x - eval' y

show'' :: Show a => Expr' a -> String
show'' (Lit' n) = show n
show'' (Add' x y) = "(" ++ show'' x ++ "+" ++ show'' y ++ ")"
show'' (Sub x y) = "(" ++ show'' x ++ "-" ++ show'' y ++ ")"