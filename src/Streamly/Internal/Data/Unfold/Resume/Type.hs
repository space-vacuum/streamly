-- |
-- Module      : Streamly.Internal.Data.Unfold.Resume.Type
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Resumable unfolds.  See "Streamly.Internal.Data.Unfold.Resume" for
-- introduction.
--

module Streamly.Internal.Data.Unfold.Resume.Type
    (
    -- * Type
    Unfold (..)

    -- * Unfolds
    , nil
    , nilM
    , unfoldrM
    , fromList

    -- * Mapping
    , lmap

    -- * Nesting
    , concat
    )
where

#include "inline.hs"

import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Stream.StreamD.Step (Step(..))
import Prelude hiding (concat)

------------------------------------------------------------------------------
-- Type
------------------------------------------------------------------------------

-- | An @Unfold m a b@ is a generator of a stream of values of type @b@ from a
-- seed of type 'a' in 'Monad' @m@.
--
-- /Internal/

data Unfold m a b =
    -- | @Unfold step inject extract@
    forall s. Unfold (s -> m (Step s b)) (a -> m s) (s -> m a)

------------------------------------------------------------------------------
-- Unfolds
------------------------------------------------------------------------------

{-# INLINE nilM #-}
nilM :: Monad m => (a -> m c) -> Unfold m a b
nilM f = Unfold step return return

    where

    {-# INLINE_LATE step #-}
    step x = f x >> return Stop

{-# INLINE nil #-}
nil :: Monad m => Unfold m a b
nil = nilM (\_ -> return ())

{-# INLINE unfoldrM #-}
unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> Unfold m a b
unfoldrM next = Unfold step return return
  where
    {-# INLINE_LATE step #-}
    step st = do
        r <- next st
        return $ case r of
            Just (x, s) -> Yield x s
            Nothing -> Stop

-- | Convert a list of pure values to a 'Stream'
--
-- /Internal/
{-# INLINE_LATE fromList #-}
fromList :: Monad m => Unfold m [a] a
fromList = Unfold step inject return
  where
    inject = return
    {-# INLINE_LATE step #-}
    step (x:xs) = return $ Yield x xs
    step [] = return Stop

------------------------------------------------------------------------------
-- Mapping
------------------------------------------------------------------------------

-- | We can only lmap between two types that can be interconverted without
-- loss.
--
-- /Internal/
{-# INLINE_NORMAL lmap #-}
lmap :: Functor m => (a -> c) -> (c -> a) -> Unfold m c b -> Unfold m a b
lmap f g (Unfold ustep uinject uextract) =
    Unfold ustep (uinject . f) (fmap g . uextract)

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------

{-# ANN type ConcatState Fuse #-}
data ConcatState s1 s2 = ConcatOuter s1 | ConcatInner s1 s2

-- XXX Use Either a (a,b) for extracted state.
--
-- | Apply the second unfold to each output element of the first unfold and
-- flatten the output in a single stream.
--
-- /Internal/
--
{-# INLINE_NORMAL concat #-}
concat :: Monad m => Unfold m a b -> Unfold m b c -> Unfold m (a, Maybe b) c
concat (Unfold step1 inject1 extract1) (Unfold step2 inject2 extract2) =
    Unfold step inject extract

    where

    inject (x, Nothing) = do
        s <- inject1 x
        return $ ConcatOuter s
    inject (x, Just y) = do
        s1 <- inject1 x
        s2 <- inject2 y
        return $ ConcatInner s1 s2

    {-# INLINE_LATE step #-}
    step (ConcatOuter st) = do
        r <- step1 st
        case r of
            Yield x s -> do
                innerSt <- inject2 x
                return $ Skip (ConcatInner s innerSt)
            Skip s    -> return $ Skip (ConcatOuter s)
            Stop      -> return Stop

    step (ConcatInner ost ist) = do
        r <- step2 ist
        return $ case r of
            Yield x s -> Yield x (ConcatInner ost s)
            Skip s    -> Skip (ConcatInner ost s)
            Stop      -> Skip (ConcatOuter ost)

    extract (ConcatOuter s1) = (,Nothing) <$> extract1 s1
    extract (ConcatInner s1 s2) = do
        r1 <- extract1 s1
        r2 <- extract2 s2
        return (r1, Just r2)