{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.Serial
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To run examples in this module:
--
-- >>> import qualified Streamly.Prelude as Stream
--
module Streamly.Internal.Data.Stream.Serial
    (
    -- * Serial appending stream
      SerialT
    , Serial
    , serial

    -- * Construction
    , cons
    , consM
    , repeat
    , unfoldrM
    , fromList
    , list

    -- * Elimination
    , toList
    , foldWith
    , drain

    -- * Transformation
    , map
    , mapM
    , filter
    , foldFilter
    )
where

#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import GHC.Exts (IsList(..))
import Streamly.Internal.Data.Fold.Type (Fold)

import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as D
import qualified Streamly.Internal.Data.Stream.StreamD.Transform as D
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Prelude hiding (map, mapM, repeat, filter)

#include "Instances.hs"
#include "inline.hs"

-- $setup
-- >>> import qualified Streamly.Prelude as Stream

------------------------------------------------------------------------------
-- SerialT
------------------------------------------------------------------------------

-- | For 'SerialT' streams:
--
-- @
-- (<>) = 'Streamly.Prelude.serial'                       -- 'Semigroup'
-- (>>=) = flip . 'Streamly.Prelude.concatMapWith' 'Streamly.Prelude.serial' -- 'Monad'
-- @
--
-- A single 'Monad' bind behaves like a @for@ loop:
--
-- >>> :{
-- Stream.toList $ do
--      x <- Stream.fromList [1,2] -- foreach x in stream
--      return x
-- :}
-- [1,2]
--
-- Nested monad binds behave like nested @for@ loops:
--
-- >>> :{
-- Stream.toList $ do
--     x <- Stream.fromList [1,2] -- foreach x in stream
--     y <- Stream.fromList [3,4] -- foreach y in stream
--     return (x, y)
-- :}
-- [(1,3),(1,4),(2,3),(2,4)]
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
--newtype SerialT m a = SerialT {getSerialT :: Stream m a}
    -- XXX when deriving do we inherit an INLINE?
    --deriving (Semigroup, Monoid, MonadTrans)

type SerialT = Stream.Stream

-- | A serial IO stream of elements of type @a@. See 'SerialT' documentation
-- for more details.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
type Serial = SerialT IO

------------------------------------------------------------------------------
-- Generation
------------------------------------------------------------------------------

{-# INLINE cons #-}
cons :: a -> SerialT m a -> SerialT m a
cons x st = Stream.fromStreamK $ K.cons x (Stream.toStreamK st)

{-# INLINE consM #-}
{-# SPECIALIZE consM :: IO a -> SerialT IO a -> SerialT IO a #-}
consM :: Monad m => m a -> SerialT m a -> SerialT m a
consM m st = Stream.fromStreamK $ K.consM m (Stream.toStreamK st)

-- |
-- Generate an infinite stream by repeating a pure value.
--
{-# INLINE_NORMAL repeat #-}
repeat :: Monad m => a -> SerialT m a
repeat = Stream.fromStreamK . D.toStreamK . D.repeat

------------------------------------------------------------------------------
-- Combining
------------------------------------------------------------------------------

{-# INLINE serial #-}
serial :: SerialT m a -> SerialT m a -> SerialT m a
serial = (<>)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> SerialT m a -> SerialT m b
mapM f st = Stream.fromStreamK $ D.toStreamK $ D.mapM f $ D.fromStreamK (Stream.toStreamK st)

-- |
-- @
-- map = fmap
-- @
--
-- Same as 'fmap'.
--
-- @
-- > S.toList $ S.map (+1) $ S.fromList [1,2,3]
-- [2,3,4]
-- @
--
-- @since 0.4.0
{-# INLINE map #-}
map :: Monad m => (a -> b) -> SerialT m a -> SerialT m b
map f = mapM (return . f)

{-
{-# INLINE apSerial #-}
apSerial :: Monad m => SerialT m (a -> b) -> SerialT m a -> SerialT m b
apSerial st1 st2 =
    Stream.fromStreamK $ D.toStreamK $ D.fromStreamK (Stream.toStreamK st1) <*> D.fromStreamK (Stream.toStreamK st2)

{-# INLINE apSequence #-}
apSequence :: Monad m => SerialT m a -> SerialT m b -> SerialT m b
apSequence st1 st2 =
    Stream.fromStreamK $ D.toStreamK $ D.fromStreamK (Stream.toStreamK st1) *> D.fromStreamK (Stream.toStreamK st2)

{-# INLINE apDiscardSnd #-}
apDiscardSnd :: Monad m => SerialT m a -> SerialT m b -> SerialT m a
apDiscardSnd st1 st2 =
    Stream.fromStreamK $ D.toStreamK $ D.fromStreamK (Stream.toStreamK st1) <* D.fromStreamK (Stream.toStreamK st2)

-}

{-# INLINE toStreamD #-}
toStreamD :: Applicative m => SerialT m a -> D.Stream m a
toStreamD st = D.fromStreamK (Stream.toStreamK st)

{-# INLINE fromStreamD #-}
fromStreamD :: Monad m => D.Stream m a -> SerialT m a
fromStreamD m = Stream.fromStreamK $ D.toStreamK m

-- XXX We should only export generation and combinators from this module.
--
-- | Include only those elements that pass a predicate.
--
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> SerialT m a -> SerialT m a
filter p = fromStreamD . D.filter p . toStreamD

-- | Use a filtering fold on a stream.
--
-- > Stream.sum $ Stream.foldFilter (Fold.satisfy (> 5)) $ Stream.fromList [1..10]
-- 40
--
{-# INLINE foldFilter #-}
foldFilter :: Monad m => Fold m a (Maybe b) -> SerialT m a -> SerialT m b
foldFilter p = fromStreamD . D.foldFilter p . toStreamD

-- XXX Renamed to foldWith because SerialT has a Foldable instance having
-- method fold.
--
-- | Fold a stream using the supplied left 'Fold' and reducing the resulting
-- expression strictly at each step. The behavior is similar to 'foldl''. A
-- 'Fold' can terminate early without consuming the full stream. See the
-- documentation of individual 'Fold's for termination behavior.
--
-- >>> Stream.foldWith Fold.sum (Stream.enumerateFromTo 1 100)
-- 5050
--
-- Folds never fail, therefore, they produce a default value even when no input
-- is provided. It means we can always fold an empty stream and get a valid
-- result.  For example:
--
-- >>> Stream.foldWith Fold.sum Stream.nil
-- 0
--
-- However, 'foldMany' on an empty stream results in an empty stream.
-- Therefore, @Stream.foldWith f@ is not the same as @Stream.head . Stream.foldMany
-- f@.
--
-- @foldWith f = Stream.parse (Parser.fromFold f)@
--
-- /Pre-release/
{-# INLINE foldWith #-}
foldWith :: Monad m => Fold m a b -> SerialT m a -> m b
foldWith fld m = D.fold fld $ toStreamD m

-- XXX Renamed to "list" because fromList is present in IsList instance.
--
-- |
-- @
-- fromList = 'Prelude.foldr' 'K.cons' 'K.nil'
-- @
--
-- Construct a stream from a list of pure values. This is more efficient than
-- 'K.fromFoldable' for serial streams.
--
-- @since 0.4.0
{-# INLINE_EARLY list #-}
list :: Monad m => [a] -> SerialT m a
list = fromStreamD . D.fromList
{-# RULES "list fallback to StreamK" [1]
    forall a. D.toStreamK (D.fromList a) = K.fromFoldable a #-}

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

-- | Build a stream by unfolding a /monadic/ step function starting from a
-- seed.  The step function returns the next element in the stream and the next
-- seed value. When it is done it returns 'Nothing' and the stream ends. For
-- example,
--
-- @
-- let f b =
--         if b > 3
--         then return Nothing
--         else print b >> return (Just (b, b + 1))
-- in drain $ unfoldrM f 0
-- @
-- @
--  0
--  1
--  2
--  3
-- @
--
-- /Pre-release/
--
{-# INLINE unfoldrM #-}
unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> SerialT m a
unfoldrM step seed = Stream.fromStreamK $ D.toStreamK (D.unfoldrM step seed)

{-# INLINE_EARLY drain #-}
drain :: Monad m => SerialT m a -> m ()
drain st = D.drain $ D.fromStreamK (Stream.toStreamK st)
{-# RULES "drain fallback to CPS" [1]
    forall a. D.drain (D.fromStreamK a) = K.drain a #-}
