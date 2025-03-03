{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}

module HW6.T1
  ( BucketsArray
  , CHT (..)

  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  , initCapacity
  , loadFactor
  ) where

import           Control.Concurrent.Classy     (MonadConc, MonadSTM (newTVar, readTVar, writeTVar),
                                                STM, atomically)
import           Control.Concurrent.Classy.STM (TArray, TVar)
import           Control.Monad                 (forM_)
import           Data.Array.Base               (MArray (getNumElements))
import           Data.Array.IO
import           Data.Hashable                 (Hashable, hash)

initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically $ do
  size <- newTVar 0
  arr <- newArray (0, initCapacity - 1) []
  arrTVar <- newTVar arr
  return $! CHT {
    chtBuckets = arrTVar,
    chtSize = size
  }

getCHT
  :: ( MonadConc m
      -- , Eq k
     , Hashable k
     )
  => k
  -> CHT (STM m) k v
  -> m (Maybe v)
getCHT k cht = atomically $ do
  table <- readTVar (chtBuckets cht)
  capacity <- getNumElements table
  let i = mod (hash k) capacity
  bucket <- readArray table i
  return $ lookup k bucket

putCHT
  :: ( MonadConc m
      -- , Eq k
     , Hashable k
     )
  => k
  -> v
  -> CHT (STM m) k v
  -> m ()
putCHT key value cht = atomically $ do
  buckets <- readTVar (chtBuckets cht)
  capacity <- getNumElements buckets
  size <- readTVar (chtSize cht)

  let idx = hash key `mod` capacity
  currentBucket <- readArray buckets idx

  case lookup key currentBucket of
      Just _ -> do
          let updatedBucket = map (\(k, v) -> if k == key then (k, value) else (k, v)) currentBucket
          writeArray buckets idx updatedBucket
      Nothing -> do
          let updatedBucket = (key, value) : currentBucket
          writeArray buckets idx updatedBucket
          writeTVar (chtSize cht) (size + 1)

  if fromIntegral (size + 1) / fromIntegral capacity > loadFactor then do
    oldBuckets <- readTVar (chtBuckets cht)
    oldSize <- readTVar (chtSize cht)

    let newCapacity = capacity * 2
    newBuckets <- newArray (0, newCapacity - 1) []

    forM_ [0 .. capacity - 1] $ \i -> do
        cb <- readArray oldBuckets i
        forM_ cb $ \(k, v) -> do
            let newIndex = hash k `mod` newCapacity
            existingEntries <- readArray newBuckets newIndex
            writeArray newBuckets newIndex ((k, v) : existingEntries)

    writeTVar (chtBuckets cht) newBuckets
    writeTVar (chtSize cht) oldSize
  else
    return ()

sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT cht = atomically $ readTVar $ chtSize cht
