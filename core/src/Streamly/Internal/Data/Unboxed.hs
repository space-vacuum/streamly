{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UnboxedTuples #-}

module Streamly.Internal.Data.Unboxed
    ( Storable
    , alignment
    , peek
    , poke
    , peekWith
    , pokeWith
    , sizeOf
    , ArrayContents(..)
    , castContents
    , touch
    , getInternalMutableByteArray
    ) where

#include "ArrayMacros.h"

import GHC.Base (IO(..), MutableByteArray#, RealWorld, touch#)
import GHC.Ptr (Ptr(..))

#ifdef USE_STORABLE
import Foreign.Storable (Storable(..))
import GHC.Exts (byteArrayContents#, unsafeCoerce#)
#else
import Data.Int (Int8)
import Data.Primitive.Types (Prim(..), sizeOf, alignment)
import GHC.Base (Int(..))
#endif

#ifdef USE_FOREIGN_PTR
newtype ArrayContents a = ArrayContents Addr# ForeignPtrContents
#define UNPACKIF
#else
-- XXX can use UnliftedNewtypes
data ArrayContents a = ArrayContents !(MutableByteArray# RealWorld)

{-# INLINE getInternalMutableByteArray #-}
getInternalMutableByteArray :: ArrayContents a -> MutableByteArray# RealWorld
getInternalMutableByteArray (ArrayContents mbarr) = mbarr

{-# INLINE castContents #-}
castContents :: ArrayContents a -> ArrayContents b
castContents (ArrayContents mbarr) = ArrayContents mbarr

{-# INLINE touch #-}
touch :: ArrayContents a -> IO ()
touch (ArrayContents contents) =
    IO $ \s -> case touch# contents s of s' -> (# s', () #)

#define UNPACKIF {-# UNPACK #-}
#endif

#ifndef USE_STORABLE
type Storable = Prim

{-# INLINE peek #-}
peek :: Prim a => Ptr a -> IO a
peek (Ptr addr#) = IO $ \s# -> readOffAddr# addr# 0# s#

{-# INLINE poke #-}
poke :: Prim a => Ptr a -> a -> IO ()
poke (Ptr addr#) a = IO $ \s# -> (# writeOffAddr# addr# 0# a s#, () #)

{-# INLINE peekWith #-}
peekWith :: Prim a => ArrayContents a -> Int -> IO a
peekWith (ArrayContents mbarr#) (I# i#) = IO $ \s# -> readByteArray# mbarr# i# s#

{-# INLINE pokeWith #-}
pokeWith :: Prim a => ArrayContents a -> Int -> a -> IO ()
pokeWith (ArrayContents mbarr#) (I# i#) a =
    IO $ \s# -> (# writeByteArray# mbarr# i# a s#, () #)

-- | Orphan Prim instance of Bool implemented using Int8
instance Prim Bool where
    {-# INLINE sizeOf# #-}
    sizeOf# _ = sizeOf# (undefined :: Int8)
    {-# INLINE alignment# #-}
    alignment# _ = alignment# (undefined :: Int8)
    {-# INLINE indexByteArray# #-}
    indexByteArray# arr# i# = indexByteArray# arr# i# /= (0 :: Int8)
    {-# INLINE readByteArray# #-}
    readByteArray# arr# i# s# =
        case readByteArray# arr# i# s# of
            (# s1#, i :: Int8 #) -> (# s1#, i /= 0 #)
    {-# INLINE writeByteArray# #-}
    writeByteArray# arr# i# a s# =
        case a of
            True -> writeByteArray# arr# i# (1 :: Int8) s#
            False -> writeByteArray# arr# i# (0 :: Int8) s#
    {-# INLINE setByteArray# #-}
    setByteArray# arr# off# len# a s# =
        case a of
            True -> setByteArray# arr# off# len# (1 :: Int8) s#
            False -> setByteArray# arr# off# len# (0 :: Int8) s#
    {-# INLINE indexOffAddr# #-}
    indexOffAddr# addr# i# = indexOffAddr# addr# i# /= (0 :: Int8)
    {-# INLINE readOffAddr# #-}
    readOffAddr# addr# i# s# =
        case readOffAddr# addr# i# s# of
            (# s1#, i :: Int8 #) -> (# s1#, i /= 0 #)
    {-# INLINE writeOffAddr# #-}
    writeOffAddr# addr# i# a s# =
        case a of
            True -> writeOffAddr# addr# i# (1 :: Int8) s#
            False -> writeOffAddr# addr# i# (0 :: Int8) s#
    {-# INLINE setOffAddr# #-}
    setOffAddr# addr# off# len# a s# =
        case a of
            True -> setOffAddr# addr# off# len# (1 :: Int8) s#
            False -> setOffAddr# addr# off# len# (0 :: Int8) s#

#else

{-# INLINE peekWith #-}
peekWith :: Storable a => ArrayContents a -> Int -> IO a
peekWith contents i =
    let !ptr =
            Ptr
                (byteArrayContents#
                     (unsafeCoerce# (getInternalMutableByteArray contents)))
     in do
       r <- peekElemOff ptr i
       touch contents
       return r

{-# INLINE pokeWith #-}
pokeWith :: Storable a => ArrayContents a -> Int -> a -> IO ()
pokeWith contents i a =
    let !ptr =
            Ptr
                (byteArrayContents#
                     (unsafeCoerce# (getInternalMutableByteArray contents)))
     in pokeElemOff ptr i a >> touch contents

#endif
