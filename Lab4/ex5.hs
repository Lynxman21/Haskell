data MyInt = MkMyInt Int

instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromIntegral int)

instance Show MyInt where
  show (MkMyInt i) = "MkMyInt " ++ show i
--dla newtype tak samo

--Zadania
data BinTree a = EmptyBt | Node a (BinTree a) (BinTree a) deriving (Show)

instance Eq a => Eq (BinTree a) where
    (==) (EmptyBt) (EmptyBt) = True
    (==) (EmptyBt) _ = False
    (==) _ (EmptyBt) = False
    (==) (Node x lt1 rt1) (Node y lt2 rt2) = x == y && lt1 == lt2 && rt1 == rt2

data Fraction a = Fraction {num::a, denom::a} -- num - numerator, denom - denominator

instance Show a => Show (Fraction a) where
    show (Fraction x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Eq a => Eq (Fraction a) where
    (==) (Fraction x y) (Fraction x' y') = x==x' && y==y'

instance Ord a => Ord (Fraction a) where
    (<=) (Fraction x y) (Fraction x' y') = 
        if x == x' then y <= y'
        else x <= x'

instance Num a => Num (Fraction a) where
    (+) (Fraction x y) (Fraction x' y') = Fraction (x+x') (y+y')
    (-) (Fraction x y) (Fraction x' y') = Fraction (x-x') (y-y')
    (*) (Fraction x y) (Fraction x' y') = Fraction (x*x') (y*y')
    negate (Fraction x y) = Fraction (negate x) y
    abs (Fraction x y) = Fraction (abs x) y
    signum (Fraction x y) = Fraction (signum x) y
    fromInteger int = Fraction (fromInteger int) (fromInteger int)
    