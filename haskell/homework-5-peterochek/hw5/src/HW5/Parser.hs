module HW5.Parser
  ( parse
  ) where

import qualified Control.Monad.Combinators as C
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Data.ByteString as BS
import Data.Char (isAlpha, isAlphaNum)
import Data.Functor.Identity (Identity)
import Data.Text (pack)
import Data.Void
import HW5.Base
import Text.Megaparsec (ParseErrorBundle, Parsec, between, choice, empty, eof, many, manyTill,
                        notFollowedBy, runParser, satisfy, sepBy, try, (<?>), (<|>))
import Text.Megaparsec.Char (char, hexDigitChar, space1, string)
import Text.Megaparsec.Char.Lexer (scientific, signed, space, symbol)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec.Internal (ParsecT)

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme spaceConsumer

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser pLine ""

symbSpaceConsumer :: [Char] -> ParsecT Void String Identity [Char]
symbSpaceConsumer = symbol spaceConsumer

brackets :: Parser l -> Parser r -> Parser a -> Parser a
brackets leftBr = between (lexeme leftBr)

roundBrackets :: Parser a -> Parser a
roundBrackets = brackets (char '(') (char ')')

lexString :: String -> Parser String
lexString str = lexeme $ string str

pNumber :: Parser HiExpr
pNumber =
  HiExprValue . HiValueNumber . toRational <$> signed spaceConsumer scientific

pLine :: Parser HiExpr
pLine = symbSpaceConsumer "" *> lexeme pExpr <* eof

pExpr :: Parser HiExpr
pExpr = makeExprParser pOperand hiOps

pArgs :: Parser [HiExpr]
pArgs = pExpr `sepBy` lexeme (char ',')

pOperand :: Parser HiExpr
pOperand = do
  expression <- roundBrackets pExpr <|> term
  args <- lexeme pCall
  return $ eval expression args

eval :: HiExpr -> [[HiExpr]] -> HiExpr
eval fun []                   = fun
eval fun ([HiExprRun _]:rest) = eval (HiExprRun fun) rest
eval fun (first:rest)         = eval (HiExprApply fun first) rest

pCall :: Parser [[HiExpr]]
pCall =
  many
    (dot <|> try (symbSpaceConsumer "" *> roundBrackets pArgs) <|> exclamation)

dot :: Parser [HiExpr]
dot = do
  variableAccess <- char '.' *> pVariable
  return [(HiExprValue . HiValueString . pack) variableAccess]

pVariable :: Parser String
pVariable =
  let part = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
   in try ((++) <$> part <*> ((:) <$> char '-' <*> pVariable)) <|> part

exclamation :: Parser [HiExpr]
exclamation = [HiExprRun $ HiExprValue HiValueNull] <$ char '!'

term :: Parser HiExpr
term = choice [pBool, pNumber, pNull, pBytes, pFun, pString, pList, pDict]

pNull :: Parser HiExpr
pNull = HiExprValue HiValueNull <$ string "null"

pFun :: Parser HiExpr
pFun = do
  name <- pVariable
  case hiFunParse name of
    Nothing ->
      case hiActionParse name of
        Nothing  -> fail "Incorrect function name!"
        Just act -> return (HiExprValue (HiValueAction act))
    Just fun -> return (HiExprValue (HiValueFunction fun))

pBool :: Parser HiExpr
pBool =
  let trueInput = symbSpaceConsumer "true"
      falseInput = symbSpaceConsumer "false"
   in HiExprValue . HiValueBool <$> (True <$ trueInput <|> False <$ falseInput)

pString :: Parser HiExpr
pString =
  HiExprValue . HiValueString . pack <$>
  (char '"' >> manyTill Lex.charLiteral (char '"'))

pList :: Parser HiExpr
pList =
  HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$>
  (brackets (char '[') $ char ']') pArgs

pByteParser :: Parser BS.ByteString
pByteParser =
  BS.pack . (\a -> [toEnum a]) . read . ("0x" ++) <$>
  lexeme (C.count 2 hexDigitChar <* notFollowedBy hexDigitChar) <?>
  "single byte"

pBytesParser :: Parser BS.ByteString
pBytesParser =
  (\list ->
     if list /= []
       then foldl1 (<>) list
       else BS.empty) <$>
  lexeme (between (lexString "[#") (lexString "#]") (many pByteParser)) <?>
  "list of bytes"

pBytes :: Parser HiExpr
pBytes = HiExprValue . HiValueBytes <$> pBytesParser

pDict :: Parser HiExpr
pDict =
  brackets
    (char '{')
    (char '}')
    (HiExprDict <$>
     (((,) <$> (pExpr <* lexeme (char ':')) <*> pExpr) `sepBy` lexeme (char ',')))

hiFunApply :: HiFun -> HiExpr -> HiExpr -> HiExpr
hiFunApply fun x y = HiExprApply (HiExprValue $ HiValueFunction fun) [x, y]

hiFunInfN :: String -> HiFun -> Operator Parser HiExpr
hiFunInfN name f = InfixN (hiFunApply f <$ lexeme (string name))

hiFunInfL :: String -> HiFun -> Operator Parser HiExpr
hiFunInfL name f = InfixL (hiFunApply f <$ lexeme (string name))

hiFunInfR :: String -> HiFun -> Operator Parser HiExpr
hiFunInfR name f = InfixR (hiFunApply f <$ lexeme (string name))

hiOps :: [[Operator Parser HiExpr]]
hiOps =
  [ [ InfixL
        (hiFunApply HiFunDiv <$
         try (lexString "/" <* notFollowedBy (string "=")))
    , hiFunInfL "*" HiFunMul
    ]
  , [hiFunInfL "+" HiFunAdd, hiFunInfL "-" HiFunSub]
  , [ hiFunInfN ">=" HiFunNotLessThan
    , hiFunInfN "<=" HiFunNotGreaterThan
    , hiFunInfN "<" HiFunLessThan
    , hiFunInfN ">" HiFunGreaterThan
    , hiFunInfN "==" HiFunEquals
    , hiFunInfN "/=" HiFunNotEquals
    ]
  , [hiFunInfR "&&" HiFunAnd]
  , [hiFunInfR "||" HiFunOr]
  ]

hiFunParse :: String -> Maybe HiFun
hiFunParse "div"              = Just HiFunDiv
hiFunParse "mul"              = Just HiFunMul
hiFunParse "add"              = Just HiFunAdd
hiFunParse "sub"              = Just HiFunSub
hiFunParse "not"              = Just HiFunNot
hiFunParse "and"              = Just HiFunAnd
hiFunParse "or"               = Just HiFunOr
hiFunParse "less-than"        = Just HiFunLessThan
hiFunParse "greater-than"     = Just HiFunGreaterThan
hiFunParse "equals"           = Just HiFunEquals
hiFunParse "not-less-than"    = Just HiFunNotLessThan
hiFunParse "not-greater-than" = Just HiFunNotGreaterThan
hiFunParse "not-equals"       = Just HiFunNotEquals
hiFunParse "if"               = Just HiFunIf
hiFunParse "length"           = Just HiFunLength
hiFunParse "to-upper"         = Just HiFunToUpper
hiFunParse "to-lower"         = Just HiFunToLower
hiFunParse "reverse"          = Just HiFunReverse
hiFunParse "trim"             = Just HiFunTrim
hiFunParse "list"             = Just HiFunList
hiFunParse "range"            = Just HiFunRange
hiFunParse "fold"             = Just HiFunFold
hiFunParse "pack-bytes"       = Just HiFunPackBytes
hiFunParse "unpack-bytes"     = Just HiFunUnpackBytes
hiFunParse "encode-utf8"      = Just HiFunEncodeUtf8
hiFunParse "decode-utf8"      = Just HiFunDecodeUtf8
hiFunParse "zip"              = Just HiFunZip
hiFunParse "unzip"            = Just HiFunUnzip
hiFunParse "serialise"        = Just HiFunSerialise
hiFunParse "deserialise"      = Just HiFunDeserialise
hiFunParse "read"             = Just HiFunRead
hiFunParse "write"            = Just HiFunWrite
hiFunParse "mkdir"            = Just HiFunMkDir
hiFunParse "cd"               = Just HiFunChDir
hiFunParse "parse-time"       = Just HiFunParseTime
hiFunParse "rand"             = Just HiFunRand
hiFunParse "echo"             = Just HiFunEcho
hiFunParse "count"            = Just HiFunCount
hiFunParse "keys"             = Just HiFunKeys
hiFunParse "values"           = Just HiFunValues
hiFunParse "invert"           = Just HiFunInvert
hiFunParse _                  = Nothing

hiActionParse :: String -> Maybe HiAction
hiActionParse "cwd" = Just HiActionCwd
hiActionParse "now" = Just HiActionNow
hiActionParse _     = Nothing
