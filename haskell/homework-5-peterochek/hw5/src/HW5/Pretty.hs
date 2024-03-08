module HW5.Pretty
  ( prettyValue
  ) where

import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Ratio (denominator, numerator)
import Data.Text as Text (unpack)
import HW5.Base
import Prettyprinter (Doc, annotate, pretty)
import Prettyprinter.Internal (viaShow)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, colorDull)
import Text.Printf (printf)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueString s) = viaShow s
prettyValue (HiValueFunction fun) =
  annotate (colorDull Red <> bold) $ pretty $ funToString fun
prettyValue other = annotate (colorDull Black) $ pretty $ toString other

toString :: HiValue -> String
toString (HiValueBool x) = [toLower loweredString | loweredString <- show x]
toString (HiValueNumber x) =
  numberToString (numerator x) (denominator x) (fromRational x)
toString HiValueNull = "null"
toString (HiValueList list) = "[" ++ listToString (toList list) ++ " ]"
toString (HiValueBytes bytes) = "[# " ++ bytesToString bytes ++ "#]"
toString (HiValueTime time) = "parse-time(" ++ "\"" ++ show time ++ "\"" ++ ")"
toString (HiValueAction action) =
  case action of
    HiActionRead path -> "read(" ++ "\"" ++ path ++ "\"" ++ ")"
    HiActionWrite path string ->
      "write(" ++
      "\"" ++
      path ++ "\"" ++ ", " ++ show (prettyValue $ HiValueBytes string) ++ ")"
    HiActionMkDir path -> "mkdir(\"" ++ path ++ "\")"
    HiActionChDir path -> "cd(\"" ++ path ++ "\")"
    HiActionCwd -> "cwd"
    HiActionNow -> "now"
    HiActionRand lower upper ->
      "rand(" ++ show lower ++ ", " ++ show upper ++ ")"
    HiActionEcho string -> "echo(" ++ show string ++ ")"
toString (HiValueDict dict) = "{" ++ dictToString dict ++ " }"
toString (HiValueFunction fun) = funToString fun
toString (HiValueString string) = Text.unpack string

cancelFrac :: Integral a => a -> Bool
cancelFrac 1 = True
cancelFrac number
  | mod number 5 == 0 = cancelFrac (div number 5)
  | even number = cancelFrac (div number 2)
  | otherwise = False

numberToString :: (Show a, Integral a) => a -> a -> Double -> String
numberToString n d x
  | d == 1 = show n
  | cancelFrac d = printf "%f" (x :: Double)
  | otherwise =
    (if n > d
       then addFracToString n d " + "
       else if (-n) > d
              then "-" ++ addFracToString (-n) d " - "
              else show n) ++
    "/" ++ show d

addFracToString :: (Show a, Integral a) => a -> a -> [Char] -> [Char]
addFracToString n d sign = show (div n d) ++ sign ++ show (mod n d)

listToString :: [HiValue] -> String
listToString []     = ""
listToString [x]    = " " ++ show (prettyValue x)
listToString (x:xs) = " " ++ show (prettyValue x) ++ "," ++ listToString xs

bytesToString :: BS.ByteString -> String
bytesToString =
  BS.foldr (\word string -> printf "%02x" word ++ " " ++ string) ""

dictToString :: Map.Map HiValue HiValue -> String
dictToString dict =
  let struct [] = ""
      struct [(k, v)] =
        " " ++ show (prettyValue k) ++ ": " ++ show (prettyValue v)
      struct ((k, v):rest) =
        " " ++
        show (prettyValue k) ++
        ": " ++ show (prettyValue v) ++ "," ++ struct rest
   in struct $ Map.assocs dict

funToString :: HiFun -> [Char]
funToString fun
  | fun == HiFunDiv = "div"
  | fun == HiFunMul = "mul"
  | fun == HiFunAdd = "add"
  | fun == HiFunSub = "sub"
  | fun == HiFunNot = "not"
  | fun == HiFunAnd = "and"
  | fun == HiFunOr = "or"
  | fun == HiFunLessThan = "less-than"
  | fun == HiFunGreaterThan = "greater-than"
  | fun == HiFunEquals = "equals"
  | fun == HiFunNotLessThan = "not-less-than"
  | fun == HiFunNotGreaterThan = "not-greater-than"
  | fun == HiFunNotEquals = "not-equals"
  | fun == HiFunIf = "if"
  | fun == HiFunLength = "length"
  | fun == HiFunToUpper = "to-upper"
  | fun == HiFunToLower = "to-lower"
  | fun == HiFunReverse = "reverse"
  | fun == HiFunTrim = "trim"
  | fun == HiFunList = "list"
  | fun == HiFunRange = "range"
  | fun == HiFunFold = "fold"
  | fun == HiFunPackBytes = "pack-bytes"
  | fun == HiFunUnpackBytes = "unpack-bytes"
  | fun == HiFunEncodeUtf8 = "encode-utf8"
  | fun == HiFunDecodeUtf8 = "decode-utf8"
  | fun == HiFunZip = "zip"
  | fun == HiFunUnzip = "unzip"
  | fun == HiFunSerialise = "serialise"
  | fun == HiFunDeserialise = "deserialise"
  | fun == HiFunRead = "read"
  | fun == HiFunWrite = "write"
  | fun == HiFunMkDir = "mkdir"
  | fun == HiFunChDir = "cd"
  | fun == HiFunParseTime = "parse-time"
  | fun == HiFunRand = "rand"
  | fun == HiFunEcho = "echo"
  | fun == HiFunCount = "count"
  | fun == HiFunKeys = "keys"
  | fun == HiFunValues = "values"
  | fun == HiFunInvert = "invert"
  | otherwise = undefined
