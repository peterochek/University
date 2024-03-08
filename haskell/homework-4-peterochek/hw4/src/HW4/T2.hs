{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Control.Applicative
import Control.Monad
import Numeric.Natural (Natural)

import Data.Char (digitToInt, isDigit, isSpace)
import Data.Scientific
import HW4.T1 (ExceptState (..))
import HW4.Types

newtype ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P cur) string = case runES cur (0, string) of
  Success (output :# _) -> Success output
  Error   e             -> Error   e

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (P curA) <|> (P curB) = P $ ES $ \(pos, string) ->
    case runES curA (pos, string) of
      Success x -> Success x
      Error _   -> runES curB (pos, string)

instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \(pos, string) ->
  case string of
    [] -> Success (() :# (pos, string))
    _  -> Error $ ErrorAtPos pos

-- Classic arithmetic LL1 Grammar
-- parseExpr -> pExpr
-- pExpr     -> pTerm pExpr'
-- pExpr'    -> "+" pTerm pExpr' | "-" pTerm pExpr' | ""
-- pTerm     -> pFactor pTerm'
-- pTerm'    -> "*" pFactor pTerm' | "/" pFactor pTerm' | ""
-- pFactor   -> pNumber | "(" pExpr ")"
-- pNumber   -> pFrac | pInt

parseExpr :: String -> Except ParseError Expr
parseExpr = runP (pSkipSpace *> pExpr <* pSkipSpace <* pEof)

pExpr :: Parser Expr
pExpr = do
  term <- pSkipSpace *> pTerm <* pSkipSpace
  pExpr' term <|> return term

pExpr' :: Expr -> Parser Expr
pExpr' cur = do
  opChar <- pExactChar '+' <|> pExactChar '-'
  term <- pSkipSpace *> pTerm <* pSkipSpace
  acc <- case opChar of
           '+' -> return (Op (Add cur term))
           '-' -> return (Op (Sub cur term))
           _   -> parseError
  pExpr' acc <|> return acc

pTerm :: Parser Expr
pTerm = do
  factor <- pSkipSpace *> pFactor <* pSkipSpace
  pTerm' factor <|> return factor

pTerm' :: Expr -> Parser Expr
pTerm' cur = do
  opChar <- pExactChar '*' <|> pExactChar '/'
  factor <- pSkipSpace *> pFactor <* pSkipSpace
  acc <- case opChar of
           '*' -> return (Op (Mul cur factor))
           '/' -> return (Op (Div cur factor))
           _   -> parseError
  pTerm' acc <|> return acc

pFactor :: Parser Expr
pFactor = fmap Val pNumber <|> (pExactChar '(' *> pExpr <* pExactChar ')') -- par. have highest preced.

skipByFilter :: (Char -> Bool) -> Parser Char
skipByFilter filt = mfilter filt pChar

pExactChar :: Char -> Parser Char
pExactChar char = skipByFilter (char ==)

pSkipSpace :: Parser String
pSkipSpace = many (skipByFilter isSpace)

pNumeric :: Parser String
pNumeric = some (skipByFilter isDigit)

pNumber :: Parser Double
pNumber = do
  pFrac <|> pInt -- pInt is ok (if pFrac failed)

pInt :: Parser Double
pInt = do
  int <- pNumeric
  return $ parseDouble int 0

pFrac :: Parser Double
pFrac = do
  int <- pNumeric
  _ <- pExactChar '.'
  frac <- pNumeric
  return $ parseDouble (int ++ frac) (-length frac)


parseInt :: String -> Integer
parseInt = foldl (\cur digit -> toInteger (digitToInt digit) + 10 * cur) 0

parseDouble :: String -> Int -> Double
parseDouble str expon = toRealFloat $ scientific (parseInt str) expon
