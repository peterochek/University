{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module HW5.Evaluator
  ( eval
  ) where

import Prelude hiding (drop, null, take)

import Control.Monad (foldM, forM)
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Char (chr, ord)
import Data.Either (fromRight)
import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Ratio (numerator)
import Data.Semigroup (stimes)
import Data.Text as Text (pack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (addUTCTime, diffUTCTime)
import Text.Read (readMaybe)

import HW5.Base (HiAction (HiActionChDir, HiActionEcho, HiActionMkDir, HiActionRand, HiActionRead, HiActionWrite),
                 HiError (..), HiExpr (..), HiFun (..), HiMonad (runAction), HiValue (..),
                 Wrapper (..))
import HW5.Utils (isBetween, isInt, isInteger)

import qualified Codec.Compression.Zlib as ZLib (CompressParams (compressLevel), bestCompression,
                                                 compressWith, decompress, defaultCompressParams)
import qualified Codec.Serialise as Ser (deserialiseOrFail, serialise)
import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import qualified Data.ListLike as LL (ListLike, drop, empty, index, length, reverse, take)
import qualified Data.Map as Map (elems, fromList, fromListWith, keys, lookup, toList)
import qualified Data.Text as Text (strip, toLower, toUpper, unpack)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalImpl

evalImpl :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalImpl (HiExprValue val) = pure val
evalImpl (HiExprApply fun args) = do
  hiValue <- evalImpl fun
  case hiValue of
    HiValueString str  -> forM args evalImpl >>= view str
    HiValueList lst    -> forM args evalImpl >>= view lst
    HiValueBytes bytes -> forM args evalImpl >>= view bytes
    HiValueDict dict   -> forM args evalImpl >>= byKey dict
    HiValueFunction fn -> lazyHiFun fn args
    _                  -> throwError HiErrorInvalidFunction
evalImpl (HiExprRun expr) = do
  hiExpr <- evalImpl expr
  case hiExpr of
    HiValueAction act -> lift $ runAction act
    _                 -> throwError HiErrorInvalidArgument
evalImpl (HiExprDict struct) = do
  hiDict <- forM struct (bitraverse evalImpl evalImpl)
  wrapped $ Map.fromList hiDict

lazyHiFun :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
lazyHiFun HiFunOr [predl, predr] =
  evalImpl predl >>= \case
    HiValueBool False -> evalImpl predr
    HiValueNull       -> evalImpl predr
    other             -> return other
lazyHiFun HiFunAnd [predl, predr] =
  evalImpl predl >>= \case
    bool@(HiValueBool False) -> return bool
    null@HiValueNull         -> return null
    _                        -> evalImpl predr
lazyHiFun HiFunIf [predicate, true, false] =
  evalImpl predicate >>= \case
    HiValueBool val ->
      evalImpl $
      if val
        then true
        else false
    _ -> throwError HiErrorInvalidArgument
lazyHiFun fun args =
  if fun `elem` [HiFunOr, HiFunIf, HiFunIf]
    then throwError HiErrorArityMismatch
    else forM args evalImpl >>= evalHiFun fun

evalHiFun :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
evalHiFun HiFunDiv [num, denom] =
  case (num, denom) of
    (HiValueNumber a, HiValueNumber b) ->
      if b /= 0
        then wrapped $ a / b
        else throwError HiErrorDivideByZero
    (HiValueString a, HiValueString b) -> wrapped $ a <> Text.pack "/" <> b
    _ -> throwError HiErrorInvalidArgument
evalHiFun HiFunMul [lnum, rnum] =
  case (lnum, rnum) of
    (HiValueNumber a, HiValueNumber b) -> wrapped (a * b)
    (HiValueString a, HiValueNumber b) -> b `times` a
    (HiValueList a, HiValueNumber b)   -> b `times` a
    (HiValueBytes a, HiValueNumber b)  -> b `times` a
    _                                  -> throwError HiErrorInvalidArgument
  where
    times n x =
      if isInteger n && n > 0
        then wrapped $ numerator n `stimes` x
        else throwError HiErrorInvalidArgument
evalHiFun HiFunAdd [addl, addr] =
  case (addl, addr) of
    (HiValueString l, HiValueString r) -> wrapped $ l <> r
    (HiValueList l, HiValueList r)     -> wrapped $ l <> r
    (HiValueNumber l, HiValueNumber r) -> wrapped $ l + r
    (HiValueTime l, HiValueNumber r)   -> wrapped $ addUTCTime (fromRational r) l
    (HiValueNumber l, HiValueTime r)   -> wrapped $ addUTCTime (fromRational l) r
    (HiValueBytes l, HiValueBytes r)   -> wrapped $ l <> r
    _                                  -> throwError HiErrorInvalidArgument
evalHiFun HiFunSub [addl, subr] =
  case (addl, subr) of
    (HiValueTime l, HiValueTime r)     -> wrapped $ toRational $ diffUTCTime l r
    (HiValueNumber l, HiValueNumber r) -> wrapped $ l - r
    _                                  -> throwError HiErrorInvalidArgument
evalHiFun HiFunNot [neg] =
  case neg of
    (HiValueBool val) -> wrapped $ not val
    _                 -> throwError HiErrorInvalidArgument
evalHiFun HiFunLessThan [l, r] = wrapped $ l < r
evalHiFun HiFunGreaterThan [l, r] = wrapped $ l > r
evalHiFun HiFunEquals [l, r] = wrapped $ l == r
evalHiFun HiFunNotLessThan [l, r] = wrapped $ l >= r
evalHiFun HiFunNotGreaterThan [l, r] = wrapped $ l <= r
evalHiFun HiFunNotEquals [l, r] = wrapped $ l /= r
evalHiFun HiFunLength [sq] =
  case sq of
    (HiValueList val)   -> wrapped $ LL.length val
    (HiValueBytes val)  -> wrapped $ LL.length val
    (HiValueString val) -> wrapped $ LL.length val
    _                   -> throwError HiErrorInvalidArgument
evalHiFun HiFunToUpper [str] =
  case str of
    (HiValueString val) -> wrapped $ Text.toUpper val
    _                   -> throwError HiErrorInvalidArgument
evalHiFun HiFunToLower [str] =
  case str of
    (HiValueString val) -> wrapped $ Text.toLower val
    _                   -> throwError HiErrorInvalidArgument
evalHiFun HiFunReverse [sq] =
  case sq of
    (HiValueList val)   -> wrapped $ LL.reverse val
    (HiValueBytes val)  -> wrapped $ LL.reverse val
    (HiValueString val) -> wrapped $ LL.reverse val
    _                   -> throwError HiErrorInvalidArgument
evalHiFun HiFunTrim [str] =
  case str of
    (HiValueString val) -> wrapped $ Text.strip val
    _                   -> throwError HiErrorInvalidArgument
evalHiFun HiFunList sq = wrapped sq
evalHiFun HiFunRange [lower, upper] =
  case (lower, upper) of
    (HiValueNumber l, HiValueNumber r) -> wrapped [l .. r]
    (HiValueBool l, HiValueBool r)     -> wrapped [l .. r]
    _                                  -> throwError HiErrorInvalidArgument
evalHiFun HiFunFold [fun, arg] =
  case arg of
    (HiValueList val)   -> fold $ toList val
    (HiValueBytes val)  -> fold $ wrap . ord <$> C8.unpack val
    (HiValueString val) -> fold $ wrap <$> Text.unpack val
    _                   -> throwError HiErrorInvalidArgument
  where
    apply l r =
      evalImpl (HiExprApply (HiExprValue fun) $ HiExprValue <$> [l, r])
    fold (x:xs) = foldM apply x xs
    fold []     = return HiValueNull
evalHiFun HiFunPackBytes [bytes] =
  case bytes of
    (HiValueList sq) -> do
      numbers <- mapM fromNumber (toList sq)
      byteSeq <- mapM byte numbers
      wrapped $ C8.pack byteSeq
    _ -> throwError HiErrorInvalidArgument
  where
    fromNumber (HiValueNumber x) = return x
    fromNumber _                 = throwError HiErrorInvalidArgument
    byte val =
      if isBetween 0 255 val && isInteger val
        then return $ chr (fromEnum val)
        else throwError HiErrorInvalidArgument
evalHiFun HiFunUnpackBytes [bytes] =
  case bytes of
    (HiValueBytes val) -> wrapped $ ord <$> C8.unpack val
    _                  -> throwError HiErrorInvalidArgument
evalHiFun HiFunEncodeUtf8 [str] =
  case str of
    (HiValueString val) -> wrapped $ encodeUtf8 val
    _                   -> throwError HiErrorInvalidArgument
evalHiFun HiFunDecodeUtf8 [str] =
  case str of
    (HiValueBytes val) ->
      return $ either (const HiValueNull) HiValueString (decodeUtf8' val)
    _ -> throwError HiErrorInvalidArgument
evalHiFun HiFunZip [bytes] =
  case bytes of
    (HiValueBytes val) -> wrapped $ compress val
    _                  -> throwError HiErrorInvalidArgument
  where
    compress =
      toStrict .
      ZLib.compressWith
        ZLib.defaultCompressParams {ZLib.compressLevel = ZLib.bestCompression} .
      fromStrict
evalHiFun HiFunUnzip [bytes] =
  case bytes of
    (HiValueBytes val) -> wrapped $ decompress val
    _                  -> throwError HiErrorInvalidArgument
  where
    decompress = toStrict . ZLib.decompress . fromStrict
evalHiFun HiFunSerialise [val] = wrapped $ serialise val
  where
    serialise = toStrict . Ser.serialise
evalHiFun HiFunDeserialise [bytes] =
  case bytes of
    (HiValueBytes val) -> return $ fromRight HiValueNull $ deserialise val
    _                  -> throwError HiErrorInvalidArgument
  where
    deserialise = Ser.deserialiseOrFail . fromStrict
evalHiFun HiFunRead [str] =
  case str of
    (HiValueString val) -> wrapped $ HiActionRead $ Text.unpack val
    _                   -> throwError HiErrorInvalidArgument
evalHiFun HiFunWrite [str, raw] =
  case (str, raw) of
    (HiValueString path, HiValueBytes content) ->
      wrapped $ HiActionWrite (Text.unpack path) content
    (HiValueString path, HiValueString content) ->
      wrapped $ HiActionWrite (Text.unpack path) (C8.pack $ Text.unpack content)
    _ -> throwError HiErrorInvalidArgument
evalHiFun HiFunMkDir [str] =
  case str of
    (HiValueString path) -> wrapped $ HiActionMkDir $ Text.unpack path
    _                    -> throwError HiErrorInvalidArgument
evalHiFun HiFunChDir [str] =
  case str of
    (HiValueString path) -> wrapped $ HiActionChDir $ Text.unpack path
    _                    -> throwError HiErrorInvalidArgument
evalHiFun HiFunParseTime [str] =
  case str of
    (HiValueString time) ->
      return $ maybe HiValueNull HiValueTime (readMaybe $ Text.unpack time)
    _ -> throwError HiErrorInvalidArgument
evalHiFun HiFunRand [lnum, rnum] =
  case (lnum, rnum) of
    (HiValueNumber lower, HiValueNumber upper) -> uniRand lower upper
    _                                          -> throwError HiErrorInvalidArgument
  where
    uniRand lower upper =
      if isInt lower && isInt upper
        then wrapped $ HiActionRand (fromEnum lower) (fromEnum upper)
        else throwError HiErrorInvalidArgument
evalHiFun HiFunEcho [str] =
  case str of
    (HiValueString val) -> wrapped $ HiActionEcho val
    _                   -> throwError HiErrorInvalidArgument
evalHiFun HiFunCount [sq] =
  case sq of
    (HiValueString val) -> count $ Text.unpack val
    (HiValueBytes val)  -> count $ ord <$> C8.unpack val
    (HiValueList val)   -> count $ toList val
    _                   -> throwError HiErrorInvalidArgument
  where
    count xs = wrapped $ Map.fromListWith (+) ((, 1 :: Int) <$> xs)
evalHiFun HiFunKeys [dict] =
  case dict of
    (HiValueDict val) -> wrapped $ Map.keys val
    _                 -> throwError HiErrorInvalidArgument
evalHiFun HiFunValues [dict] =
  case dict of
    (HiValueDict val) -> wrapped $ Map.elems val
    _                 -> throwError HiErrorInvalidArgument
evalHiFun HiFunInvert [dict] =
  case dict of
    (HiValueDict val) ->
      wrapped $ Map.fromListWith (++) ((\(k, v) -> (v, [k])) <$> Map.toList val)
    _ -> throwError HiErrorInvalidArgument
evalHiFun _ _ = throwError HiErrorArityMismatch

view ::
     (HiMonad m, Wrapper l, Wrapper a, LL.ListLike l a)
  => l
  -> [HiValue]
  -> ExceptT HiError m HiValue
view list [num] =
  case num of
    (HiValueNumber idx)
      | isInt idx -> return $ index (fromEnum idx)
    _ -> throwError HiErrorInvalidArgument
  where
    index n =
      if isBetween 0 (LL.length list - 1) n
        then wrap (list `LL.index` n)
        else HiValueNull
view list [lnum, rnum] =
  case (lnum, rnum) of
    (HiValueNumber lidx, HiValueNumber ridx) -> wrap <$> slice list lidx ridx
    (HiValueNumber lidx, HiValueNull) ->
      wrap <$> slice list lidx (toRational $ LL.length list)
    (HiValueNull, HiValueNumber ridx) -> wrap <$> slice list 0 ridx
    (HiValueNull, HiValueNull) ->
      wrap <$> slice list 0 (toRational $ LL.length list)
    _ -> throwError HiErrorInvalidArgument
  where
    slice sq x y =
      let start = indexation (fromEnum x) len
          end = indexation (fromEnum y) len
          len = LL.length sq
       in if isInt x && isBetween 0 len start && isInt y && isBetween 0 len end
            then if start <= end
                   then return $ (LL.take (end - start) . LL.drop start) sq
                   else return LL.empty
            else throwError HiErrorInvalidArgument
    indexation idx len
      | idx < 0 = max (idx + len) 0
      | idx >= len = len
      | otherwise = idx
view _ _ = throwError HiErrorArityMismatch

byKey ::
     Monad m => Map HiValue HiValue -> [HiValue] -> ExceptT HiError m HiValue
byKey dict [k] = return $ fromMaybe HiValueNull (Map.lookup k dict)
byKey _ _      = throwError HiErrorInvalidArgument
