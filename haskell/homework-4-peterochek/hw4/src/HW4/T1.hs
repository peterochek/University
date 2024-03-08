module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import qualified Control.Monad as Monad

import HW4.Types (Annotated (..), Except (..), Expr (..), Prim (..), mapAnnotated, mapExcept)

newtype ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES g) = ES (mapExcept (mapAnnotated f) . g)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState x = ES (\s -> Success (x :# s))

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES outer) = ES { runES = compose } where
  compose s = case outer s of
    Success ((ES inner) :# a) -> inner a
    Error e                   -> Error e

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\x -> Success (() :# f x))

throwExceptState :: e -> ExceptState e s a
throwExceptState x = ES (\_ -> Error x)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  a <*> b = Monad.ap a b

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero
  deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval = evalExpr
  where
    evalBinaryExcept except op f a b = do
      x <- eval a
      y <- eval b
      _ <- except x y
      modifyExceptState (op x y :)
      return (f x y)

    evalBinary = evalBinaryExcept (\_ _ -> return ())

    evalUnary op f a = do
      x <- eval a
      modifyExceptState (op x :)
      return (f x)

    exceptDivZero _ b = Monad.when (b == 0) (throwExceptState DivideByZero)

    evalExpr (Val a)        = return a
    evalExpr (Op (Add a b)) = evalBinary Add (+) a b
    evalExpr (Op (Sub a b)) = evalBinary Sub (-) a b
    evalExpr (Op (Mul a b)) = evalBinary Mul (*) a b
    evalExpr (Op (Div a b)) = evalBinaryExcept exceptDivZero Div (/) a b
    evalExpr (Op (Abs a))   = evalUnary Abs abs a
    evalExpr (Op (Sgn a))   = evalUnary Sgn signum a
