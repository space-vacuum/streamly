{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Data.StreamK
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Streams using Continuation Passing Style (CPS). See the @Stream vs StreamK@
-- section in the "Streamly.Data.Stream" module to know when to use this
-- module.
--
-- Please refer to "Streamly.Internal.Data.StreamK" for more functions
-- that have not yet been released.
--
-- Some useful idioms:
--
-- >>> concatFoldableWith f = Prelude.foldr f StreamK.nil
-- >>> concatMapFoldableWith f g = Prelude.foldr (f . g) StreamK.nil
-- >>> concatForFoldableWith f xs g = Prelude.foldr (f . g) StreamK.nil xs

-- Notes:
--
-- primitive/open loop operations that can be used recursively e.g. uncons,
-- foldBreak, parseBreak should not be converted from StreamD for use in
-- StreamK, instead native StreamK impl should be used.
--
-- Closed loop operations like repeat, replicate, iterate etc can be converted
-- from StreamD.
--
-- In the last phase any operation like (toStreamK . f . toStreamD) should be
-- rewritten to a K version of f.
-- XXX Need to add rewrite rules for all missing StreamD operations.
--
module Streamly.Data.StreamK
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Overview
    -- $overview

    -- * Type
      StreamK

    -- * Construction
    -- ** Primitives
    -- | Primitives to construct a stream from pure values or monadic actions.
    -- All other stream construction and generation combinators described later
    -- can be expressed in terms of these primitives. However, the special
    -- versions provided in this module can be much more efficient in some
    -- cases. Users can create custom combinators using these primitives.
    , nil
    , nilM
    , cons
    , consM

    -- ** From Values
    , fromPure
    , fromEffect

    -- ** From Stream
    , fromStream
    , toStream

    -- ** From Containers
    , fromFoldable

    -- * Elimination

    -- ** Primitives
    , uncons
    , drain

    -- -- ** Folding
    -- , foldBreak

    -- ** Parsing
    -- , parseBreak
    , parseBreakChunks
    , parseChunks

    -- * Transformation
    , mapM
    , dropWhile
    , take

    -- * Combining Two Streams
    -- ** Appending
    , append

    -- ** Interleaving
    , interleave

    -- ** Merging
    , mergeBy
    , mergeByM

    -- ** Zipping
    , zipWith
    , zipWithM

    -- ** Cross Product
    -- XXX is "bind/concatFor" better to have than crossWith?
    -- crossWith f xs1 xs2 = concatFor xs1 (\x -> fmap (f x) xs2)
    , crossWith
    -- , cross
    -- , joinInner
    -- , CrossStreamK (..)

    -- * Stream of streams
    , concatEffect
    -- , concatMap
    , concatMapWith
    , mergeMapWith

    -- * Buffered Operations
    , reverse
    , sortBy
    )
where

import Streamly.Internal.Data.StreamK
import Prelude hiding (reverse, zipWith, mapM, dropWhile, take)

#include "DocTestDataStreamK.hs"

-- $overview
--
-- Continuation passing style (CPS) stream implementation. The 'K' in 'StreamK'
-- stands for Kontinuation.
--
-- StreamK can be constructed like lists, except that they use 'nil' instead of
-- '[]' and 'cons' instead of ':'.
--
-- `cons` adds a pure value at the head of the stream:
--
-- >>> import Streamly.Data.StreamK (StreamK, cons, consM, nil)
-- >>> stream = 1 `cons` 2 `cons` nil :: StreamK IO Int
--
-- You can use operations from "Streamly.Data.Stream" for StreamK as well by
-- converting StreamK to Stream ('toStream'), and vice-versa ('fromStream').
--
-- >>> Stream.fold Fold.toList $ StreamK.toStream stream -- IO [Int]
-- [1,2]
--
-- `consM` adds an effect at the head of the stream:
--
-- >>> stream = effect 1 `consM` effect 2 `consM` nil
-- >>> Stream.fold Fold.toList $ StreamK.toStream stream
-- 1
-- 2
-- [1,2]
--
-- == Exception Handling
--
-- There are no native exception handling operations in the StreamK module,
-- please convert to 'Stream' type and use exception handling operations from
-- "Streamly.Data.Stream".
