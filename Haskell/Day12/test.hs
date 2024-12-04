{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}

import Control.Monad (ap, join)
import Data.Functor
import Control.Monad.Fix

data Tree = Branch Tree Tree | Leaf Int
 deriving Show

replaceMax :: Tree -> Tree
replaceMax t = let (max, t') = replace' t max in t'


replace' :: Tree -> Int -> (Int, Tree)
replace' (Leaf i)       j = (i, Leaf j)
replace' (Branch t1 t2) j = let (m1, t1') = replace' t1 j
                                (m2, t2') = replace' t2 j
                            in (max m1 m2, Branch t1' t2')

test = Branch (Branch (Leaf 4) (Leaf 5)) (Leaf 4)

newtype RState s a = RState { runRState :: s -> (s, a) }

instance Functor (RState s) where
    fmap f (RState sf) = RState (fmap f . sf)

instance Applicative (RState s) where
    pure x = RState (, x)
    (<*>) = ap

instance Monad (RState s) where
    (>>=) :: RState s a -> (a -> RState s b) -> RState s b
    r >>= f = RState $ \s -> let (s'', a) = runRState r s'
                                 (s', b) = runRState (f a) s
                             in (s'', b)

instance MonadFix (RState s) where
    mfix :: (a -> RState s a) -> RState s a
    mfix f = RState $ \s -> let ~(s', a) = runRState (f a) s in (s', a)

get :: RState s s
get = RState $ join (,)

put :: s -> RState s ()
put s = RState $ const (s, ())

g :: [Int] -> [Int]
g xs = snd $ runRState (f 0 xs) 0

f :: Int -> [Int] -> RState Int [Int]
f pm []     = return []
f pm (x:xs) = mdo 
    put (max x sm)
    sm <- get
    (x `max` pm `max` sm :) <$> f (max pm x) xs