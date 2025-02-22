--product type example
--Wywołania za pomocą konstruktora
data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

--uniwersalne nie tylko dla Int
data Cart2DVec' a = MkCart2DVec' a a -- konwencja: prefix 'Mk' dla konstruktora

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

--inny zapis konstruktoraX
--automatrycznie dla pól definiuje dostęp do pól x i y
data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

-- xCoord'' :: Cart2DVec'' a -> a
-- xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

-- yCoord'' :: Cart2DVec'' a -> a
-- yCoord'' = y -- uwaga na kolejność x,y

-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

-- enum type example (special case of sum type)
data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

--Zadania
{-
uwaga: ta sama nazwa* dla:
 - konstruktora typu (po lewej)
 - konstruktora danych/wartości (po prawej)

 * druga (obok omówionej poprzednio -- z prefiksem 'Mk') powszechna konwencja w Haskellu!
-}
data Cart3DVec a = Cart3DVec a a a

xCoord3 :: Cart3DVec a -> a
xCoord3 (Cart3DVec x _ _) = x

data Cart3DVec' a = MkCart3DVec' {x'::a, y'::a, z'::a}

data Polar2DVect = MkPolar2DVect {r::Float,phi::Float} deriving Show
data Cart2DVect = MkCart2DVect {x''::Float,y''::Float} deriving Show

polarToCart :: Polar2DVect -> Cart2DVect
polarToCart (MkPolar2DVect r phi) = MkCart2DVect (r * cos phi) (r * sin phi)

data Shape = Circle Float |
             Rectangle Float Float
area :: Shape -> Float
area (Circle r) = pi*r*r
area (Rectangle a b) = a*b

data Tree a = EmptyT |
              Node a (Tree a) (Tree a)
              deriving Show

rootValue :: Tree a -> a
rootValue EmptyT = error"Glupi jestes"
rootValue (Node a lt rt) = a

data TrafficLights = Red2 | Green | Yellow

actionFor :: TrafficLights -> String
actionFor Red2 = "Halt!"
actionFor Yellow = "Redy?"
actionFor Green = "Ryby na promocji!!!"