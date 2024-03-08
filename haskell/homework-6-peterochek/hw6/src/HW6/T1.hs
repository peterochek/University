{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HW6.T1
  ( BucketsArray
  , CHT(..)
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy (STM, atomically)
import Control.Concurrent.Classy.STM (TArray, TVar, modifyTVar', newTVar, readTVar, writeTVar)
import Control.Monad (forM_, when)
import Control.Monad.Conc.Class (MonadConc, readTVarConc)
import Data.Array.MArray (getBounds, newArray, readArray, writeArray)
import Data.Hashable (Hashable (hash))
import Data.Maybe (isNothing)

initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]

type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v =
  CHT
    { chtBuckets :: TVar stm (BucketsArray stm k v)
    , chtSize    :: TVar stm Int
    }

newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT =
  atomically $ do
    initSize <- newTVar 0
    rawArray <- newArray (0, initCapacity - 1) []
    bucketsArray <- newTVar rawArray
    return $ CHT bucketsArray initSize

getCHT :: (MonadConc m, Eq k, Hashable k) => k -> CHT (STM m) k v -> m (Maybe v)
getCHT key cht =
  atomically $ do
    bucketsArray <- readTVar $ chtBuckets cht
    (lIdx, rIdx) <- getBounds bucketsArray
    let index = hash key `mod` (rIdx - lIdx + 1)
    bucket <- readArray bucketsArray index
    return $ lookup key bucket

putCHT ::
     forall m k v. (MonadConc m, Eq k, Hashable k)
  => k
  -> v
  -> CHT (STM m) k v
  -> m ()
putCHT key value cht =
  atomically $ do
    bucketsArray <- readTVar $ chtBuckets cht
    (lIdx, rIdx) <- getBounds bucketsArray
    let index = hash key `mod` (rIdx - lIdx + 1)
    bucket <- readArray bucketsArray index
    let newBucket = (key, value) : filter ((/= key) . fst) bucket
    writeArray bucketsArray index newBucket
    when (isNothing $ lookup key bucket) $ do
      modifyTVar' (chtSize cht) (+ 1)
      size <- readTVar $ chtSize cht
      when (fromIntegral size >= fromIntegral (rIdx - lIdx + 1) * loadFactor) $
        resizeCHT @m cht

resizeCHT ::
     forall m k v. (MonadConc m, Hashable k)
  => CHT (STM m) k v
  -> STM m ()
resizeCHT cht = do
  oldBucketsArray <- readTVar $ chtBuckets cht
  (lIdx, rIdx) <- getBounds oldBucketsArray
  let newSize = (rIdx - lIdx + 1) * 2
  newBucketsArray <- newArray (0, newSize - 1) []
  forM_ [lIdx .. rIdx] $ \i -> do
    bucket <- readArray oldBucketsArray i
    forM_ bucket $ \(k, v) -> do
      let newIndex = hash k `mod` newSize
      newBucket <- readArray newBucketsArray newIndex
      writeArray newBucketsArray newIndex ((k, v) : newBucket)
  writeTVar (chtBuckets cht) newBucketsArray

sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT c = readTVarConc (chtSize c)
