module Queue
  ( Queue
  , emptyQ   -- :: Queue a
  , isEmptyQ -- :: Queue a -> Bool
  , addQ     -- :: a -> Queue a -> Queue a
  , remQ     -- :: Queue a -> (a, Queue a)
  ) where

emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
addQ :: a -> Queue a -> Queue a
remQ :: Queue a -> (a, Queue a)

newtype Queue a = MkQueue [a] deriving Show

emptyQ = MkQueue []
isEmptyQ (MkQueue q) = null q
addQ x (MkQueue q) = MkQueue (q ++ [x])
remQ (MkQueue (x:q)) = (x,MkQueue q)