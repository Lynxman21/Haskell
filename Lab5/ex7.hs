newtype Box a = MkBox a deriving Show

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w

-- ghci> pure (*2) <*> MkBox 3
-- ghci> (*2) <$> MkBox 3
-- ghci> (+) <$> MkBox 1 <*> MkBox 2
-- ghci> (++) <$> MkBox "abc" <*> MkBox "def"
-- ghci> (\x y z -> (z,y,x)) <$> MkBox (Just 1) <*> MkBox (Just 2) <*> MkBox (Just 3)

--tasks
newtype MyTriple a = MyTriple (a,a,a) deriving Show

instance Functor MyTriple where
    fmap f (MyTriple (x,y,z)) = MyTriple (f x,f y,f z)

instance Applicative MyTriple where
    pure x = MyTriple (x,x,x)
    (MyTriple (f,g,h)) <*> (MyTriple (x,y,z)) = MyTriple (f x,g y,h z)
