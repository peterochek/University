module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import qualified Control.Monad as Monad
import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S g) = S (mapAnnotated f . g)

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState (S outer) = S compose where
  inner (x :# a) = runS x a
  compose = inner . outer


modifyState :: (s -> s) -> State s ()
modifyState f = S (\x -> ():#f x)


instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  a <*> b = Monad.ap a b

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  a + b = Op (Add a b)
  a - b = Op (Sub a b)
  a * b = Op (Mul a b)
  abs a = Op (Abs a)
  signum a = Op (Sgn a)
  fromInteger a = Val (fromInteger a)

instance Fractional Expr where
  a / b = Op (Div a b)
  fromRational a = Val (fromRational a)

eval :: Expr -> State [Prim Double] Double
eval (Val x) = pure x
eval (Op (Add a b)) = do
  x <- eval a
  y <- eval b
  modifyState (Add x y :)
  return (x + y)
eval (Op (Sub a b)) = do
  x <- eval a
  y <- eval b
  modifyState (Sub x y :)
  return (x - y)
eval (Op (Mul a b)) = do
  x <- eval a
  y <- eval b
  modifyState (Mul x y :)
  return (x * y)
eval (Op (Div a b)) = do
  x <- eval a
  y <- eval b
  modifyState (Div x y :)
  return (x / y)
eval (Op (Abs a)) = do
  x <- eval a
  modifyState (Abs x :)
  return (abs x)
eval (Op (Sgn a)) = do
  x <- eval a
  modifyState (Sgn x :)
  return (signum x)
