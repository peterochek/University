{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module HW5.Base
  ( HiAction (..)
  , HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiMonad (..)
  , HiValue (..)
  , Wrapper (..)
  ) where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map (Map, mapKeys)
import Data.Ratio ((%))
import Data.Sequence (Seq, fromList)
import Data.Text (Text, pack, singleton)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data HiValue
  = HiValueBool Bool -- bool is lt number (derived precedence)
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord, Generic)

data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Eq, Ord, Generic)

data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq, Ord)

data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Enum, Show, Eq, Ord, Generic)

data HiAction
  = HiActionRead FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Ord, Generic)

instance Serialise HiValue

instance Serialise HiFun

instance Serialise HiExpr

instance Serialise HiAction

class Monad m =>
      HiMonad m
  where
  runAction :: HiAction -> m HiValue

instance HiMonad (Either HiError) where
  runAction = error "Error occurred!"

class Wrapper v where
  wrap :: v -> HiValue
  wrapped :: Monad m => v -> m HiValue
  wrapped = return . wrap

instance Wrapper Rational where
  wrap = HiValueNumber

instance Wrapper Bool where
  wrap = HiValueBool

instance {-# OVERLAPPABLE #-} Integral a => Wrapper a where
  wrap = wrap . (% 1) . toInteger

instance Wrapper Text where
  wrap = HiValueString

instance Wrapper String where
  wrap = wrap . pack

instance Wrapper Char where
  wrap = wrap . singleton

instance Wrapper ByteString where
  wrap = HiValueBytes

instance Wrapper a => Wrapper (Seq a) where
  wrap = HiValueList . (wrap <$>)

instance {-# OVERLAPPABLE #-} Wrapper a => Wrapper [a] where
  wrap = wrap . fromList

instance Wrapper HiFun where
  wrap = HiValueFunction

instance Wrapper HiAction where
  wrap = HiValueAction

instance Wrapper UTCTime where
  wrap = HiValueTime

instance (Wrapper k, Wrapper v) => Wrapper (Map k v) where
  wrap = HiValueDict . (wrap <$>) . mapKeys wrap

instance Wrapper HiValue where
  wrap = id
