{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

#include "inline.hs"

-- |
-- Module      : Streamly.Streams.StreamK
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
-- Continuation passing style (CPS) stream implementation. The symbol 'K' below
-- denotes a function as well as a Kontinuation.
--
-- @
-- import qualified Streamly.Streams.StreamK as K
-- @
--
module Streamly.Streams.StreamK
    (
    -- * A class for streams
      IsStream (..)
    , adapt

    -- * The stream type
    , Stream

    -- * Construction Primitives
    , mkStream
    , nil
    , cons
    , (.:)

    -- * Elimination Primitives
    , foldStream
    , foldStreamShared
    , foldStreamSVar

    -- * Transformation Primitives
    , unShare

    -- * Deconstruction
    , uncons

    -- * Generation
    -- ** Unfolds
    , unfoldr
    , unfoldrM

    -- ** Specialized Generation
    , repeat
    , replicate
    , replicateM

    -- ** Conversions
    , yield
    , yieldM
    , fromFoldable
    , fromList
    , fromStreamK

    -- * Elimination
    -- ** General Folds
    , foldr
    , foldr1
    , foldrM
    , foldrS
    , foldrT

    , foldl'
    , foldlM'
    , foldlS
    , foldlT
    , foldlx'
    , foldlMx'

    -- ** Specialized Folds
    , runStream
    , null
    , head
    , tail
    , init
    , elem
    , notElem
    , all
    , any
    , last
    , minimum
    , minimumBy
    , maximum
    , maximumBy
    , findIndices
    , lookup
    , findM
    , find
    , (!!)

    -- ** Map and Fold
    , mapM_

    -- ** Conversions
    , toList
    , toStreamK

    -- * Transformation
    -- ** By folding (scans)
    , scanl'
    , scanlx'

    -- ** Filtering
    , filter
    , take
    , takeWhile
    , drop
    , dropWhile

    -- ** Mapping
    , map
    , mapM
    , sequence

    -- ** Inserting
    , intersperseM
    , insertBy

    -- ** Deleting
    , deleteBy

    -- ** Reordering
    , reverse

    -- ** Map and Filter
    , mapMaybe

    -- ** Zipping
    , zipWith
    , zipWithM

    -- ** Merging
    , mergeBy
    , mergeByM

    -- ** Transformation comprehensions
    , the

    -- * Semigroup Style Composition
    , serial

    -- * Utilities
    , consMStream
    , bindWith
    , withLocal

    -- * Deprecated
    , Streaming -- deprecated
    , once      -- deprecated
    )
where

import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad (void)
import Control.Monad.Reader.Class  (MonadReader(..))
import Prelude
       hiding (foldl, foldr, last, map, mapM, mapM_, repeat, sequence,
               take, filter, all, any, takeWhile, drop, dropWhile, minimum,
               maximum, elem, notElem, null, head, tail, init, zipWith, lookup,
               foldr1, (!!), replicate, reverse)
import qualified Prelude

import Streamly.SVar
import Streamly.Streams.StreamK.Type

-- | Detach a stream from an SVar
{-# INLINE unShare #-}
unShare :: IsStream t => t m a -> t m a
unShare x = mkStream $ \st yld sng stp ->
    foldStream st yld sng stp x

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

infixr 5 `cons`

-- faster than consM because there is no bind.
-- | Construct a stream by adding a pure value at the head of an existing
-- stream. For serial streams this is the same as @(return a) \`consM` r@ but
-- more efficient. For concurrent streams this is not concurrent whereas
-- 'consM' is concurrent. For example:
--
-- @
-- > toList $ 1 \`cons` 2 \`cons` 3 \`cons` nil
-- [1,2,3]
-- @
--
-- @since 0.1.0
{-# INLINE cons #-}
cons :: IsStream t => a -> t m a -> t m a
cons a r = mkStream $ \_ yld _ _ -> yld a r

infixr 5 .:

-- | Operator equivalent of 'cons'.
--
-- @
-- > toList $ 1 .: 2 .: 3 .: nil
-- [1,2,3]
-- @
--
-- @since 0.1.1
{-# INLINE (.:) #-}
(.:) :: IsStream t => a -> t m a -> t m a
(.:) = cons

-------------------------------------------------------------------------------
-- Deconstruction
-------------------------------------------------------------------------------

{-# INLINE uncons #-}
uncons :: (IsStream t, Monad m) => t m a -> m (Maybe (a, t m a))
uncons m =
    let stop = return Nothing
        single a = return (Just (a, nil))
        yieldk a r = return (Just (a, r))
    in foldStream defState yieldk single stop m

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

{-# INLINE_NORMAL build #-}
build :: IsStream t => forall a. (forall b. (a -> b -> b) -> b -> b) -> t m a
build g = g cons nil

{-# INLINE_NORMAL _augment #-}
_augment
    :: IsStream t
    => forall a. (forall b. (a -> b -> b) -> b -> b) -> t m a -> t m a
_augment g xs = g cons xs

{-# INLINE_NORMAL _buildM #-}
_buildM
    :: (IsStream t, MonadAsync m)
    => forall a. ((m a -> t m a -> t m a) -> t m a -> t m a) -> t m a
_buildM g = g consM nil

{-# INLINE unfoldr #-}
unfoldr :: IsStream t => (b -> Maybe (a, b)) -> b -> t m a
unfoldr step b0 = build $ \cns nl ->
    let go s =
            case step s of
                Just (a, b) -> a `cns` go b
                Nothing -> nl
    in go b0

{-# INLINE unfoldrM #-}
unfoldrM :: (IsStream t, MonadAsync m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM step = go
    where
    go s = mkStream $ \st yld sng stp -> do
        mayb <- step s
        case mayb of
            Nothing -> stp
            Just (a, b) ->
                foldStreamShared st yld sng stp $ return a |: go b

-------------------------------------------------------------------------------
-- Special generation
-------------------------------------------------------------------------------

{-# INLINE yield #-}
yield :: IsStream t => a -> t m a
yield a = mkStream $ \_ _ single _ -> single a

-- | Same as yieldM
--
-- @since 0.2.0
{-# DEPRECATED once "Please use yieldM instead." #-}
{-# INLINE once #-}
once :: (Monad m, IsStream t) => m a -> t m a
once = yieldM

-- |
-- @
-- repeatM = fix . cons
-- repeatM = cycle1 . yield
-- @
--
-- Generate an infinite stream by repeating a pure value.
--
-- @since 0.4.0
{-# INLINE repeat #-}
repeat :: IsStream t => a -> t m a
repeat a = let x = cons a x in x

{-# INLINE replicateM #-}
replicateM :: (IsStream t, MonadAsync m) => Int -> m a -> t m a
replicateM n m = go n
    where
    go cnt = if cnt <= 0 then nil else m |: go (cnt - 1)

{-# INLINE replicate #-}
replicate :: IsStream t => Int -> a -> t m a
replicate n a = go n
    where
    go cnt = if cnt <= 0 then nil else a `cons` go (cnt - 1)

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- |
-- @
-- fromFoldable = 'Prelude.foldr' 'cons' 'nil'
-- @
--
-- Construct a stream from a 'Foldable' containing pure values:
--
-- @since 0.2.0
{-# INLINE fromFoldable #-}
fromFoldable :: (IsStream t, Foldable f) => f a -> t m a
fromFoldable = Prelude.foldr cons nil

{-# INLINE fromList #-}
fromList :: IsStream t => [a] -> t m a
fromList = fromFoldable

{-# INLINE fromStreamK #-}
fromStreamK :: IsStream t => Stream m a -> t m a
fromStreamK = fromStream

-------------------------------------------------------------------------------
-- Elimination by Folding
-------------------------------------------------------------------------------

-- | Lazy right associative fold.
{-# INLINE foldr #-}
foldr :: (IsStream t, Monad m) => (a -> b -> b) -> b -> t m a -> m b
foldr step acc m = go m
    where
    go m1 =
        let stop = return acc
            single a = return (step a acc)
            yieldk a r = go r >>= \b -> return (step a b)
        in foldStream defState yieldk single stop m1

-- | Lazy right fold with a monadic step function.
{-# INLINE foldrM #-}
foldrM :: IsStream t => (a -> m b -> m b) -> m b -> t m a -> m b
foldrM step acc m = go m
    where
    go m1 =
        let stop = acc
            single a = step a acc
            yieldk a r = step a (go r)
        in foldStream defState yieldk single stop m1

-- | Lazy right associative fold to a stream.
{-# INLINE foldrS #-}
foldrS :: IsStream t => (a -> t m b -> t m b) -> t m b -> t m a -> t m b
foldrS step final m = go m
    where
    go m1 = mkStream $ \st yld sng stp ->
        let run x = foldStream st yld sng stp x
            stop = run final
            single a = run $ step a final
            yieldk a r = run $ step a (go r)
         in foldStream (adaptState st) yieldk single stop m1

-- | Right associative fold to an arbitrary transformer monad.
{-# INLINE foldrT #-}
foldrT :: (IsStream t, Monad m, Monad (s m), MonadTrans s)
    => (a -> s m b -> s m b) -> s m b -> t m a -> s m b
foldrT step final m = go m
  where
    go m1 = do
        res <- lift $ uncons m1
        case res of
            Just (h, t) -> step h (go t)
            Nothing -> final

{-# INLINE foldr1 #-}
foldr1 :: (IsStream t, Monad m) => (a -> a -> a) -> t m a -> m (Maybe a)
foldr1 step m = do
    r <- uncons m
    case r of
        Nothing -> return Nothing
        Just (h, t) -> fmap Just (go h t)
    where
    go p m1 =
        let stp = return p
            single a = return $ step a p
            yieldk a r = fmap (step p) (go a r)
         in foldStream defState yieldk single stp m1

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
--
-- Note that the accumulator is always evaluated including the initial value.
{-# INLINE foldlx' #-}
foldlx' :: forall t m a b x. (IsStream t, Monad m)
    => (x -> a -> x) -> x -> (x -> b) -> t m a -> m b
foldlx' step begin done m = get $ go m begin
    where
    {-# NOINLINE get #-}
    get :: t m x -> m b
    get m1 =
        -- XXX we are not strictly evaluating the accumulator here. Is this
        -- okay?
        let single = return . done
        -- XXX this is foldSingleton. why foldStreamShared?
         in foldStreamShared undefined undefined single undefined m1

    -- Note, this can be implemented by making a recursive call to "go",
    -- however that is more expensive because of unnecessary recursion
    -- that cannot be tail call optimized. Unfolding recursion explicitly via
    -- continuations is much more efficient.
    go :: t m a -> x -> t m x
    go m1 !acc = mkStream $ \_ yld sng _ ->
        let stop = sng acc
            single a = sng $ step acc a
            -- XXX this is foldNonEmptyStream
            yieldk a r = foldStream defState yld sng undefined $
                go r (step acc a)
        in foldStream defState yieldk single stop m1

-- | Strict left associative fold.
{-# INLINE foldl' #-}
foldl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> m b
foldl' step begin = foldlx' step begin id

-- XXX replace the recursive "go" with explicit continuations.
-- | Like 'foldx', but with a monadic step function.
{-# INLINABLE foldlMx' #-}
foldlMx' :: (IsStream t, Monad m)
    => (x -> a -> m x) -> m x -> (x -> m b) -> t m a -> m b
foldlMx' step begin done m = go begin m
    where
    go !acc m1 =
        let stop = acc >>= done
            single a = acc >>= \b -> step b a >>= done
            yieldk a r = acc >>= \b -> step b a >>= \x -> go (return x) r
         in foldStream defState yieldk single stop m1

-- | Like 'foldl'' but with a monadic step function.
{-# INLINE foldlM' #-}
foldlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> b -> t m a -> m b
foldlM' step begin = foldlMx' step (return begin) return

-- | Lazy left fold to a stream.
{-# INLINE foldlS #-}
foldlS :: IsStream t => (t m b -> a -> t m b) -> t m b -> t m a -> t m b
foldlS step begin m = go begin m
    where
    go acc rest = mkStream $ \st yld sng stp ->
        let run x = foldStream st yld sng stp x
            stop = run acc
            single a = run $ step acc a
            yieldk a r = run $ go (step acc a) r
         in foldStream (adaptState st) yieldk single stop rest

-- | Lazy left fold to an arbitrary transformer monad.
{-# INLINE foldlT #-}
foldlT :: (IsStream t, Monad m, Monad (s m), MonadTrans s)
    => (s m b -> a -> s m b) -> s m b -> t m a -> s m b
foldlT step begin m = go begin m
  where
    go acc m1 = do
        res <- lift $ uncons m1
        case res of
            Just (h, t) -> go (step acc h) t
            Nothing -> acc

------------------------------------------------------------------------------
-- Specialized folds
------------------------------------------------------------------------------

-- |
-- > runStream = foldl' (\_ _ -> ()) ()
-- > runStream = mapM_ (\_ -> return ())
{-# INLINE runStream #-}
runStream :: (Monad m, IsStream t) => t m a -> m ()
runStream = go
    where
    go m1 =
        let stop = return ()
            single _ = return ()
            yieldk _ r = go r
         in foldStream defState yieldk single stop m1

{-# INLINE null #-}
null :: (IsStream t, Monad m) => t m a -> m Bool
null m =
    let stop      = return True
        single _  = return False
        yieldk _ _ = return False
    in foldStream defState yieldk single stop m

{-# INLINE head #-}
head :: (IsStream t, Monad m) => t m a -> m (Maybe a)
head m =
    let stop      = return Nothing
        single a  = return (Just a)
        yieldk a _ = return (Just a)
    in foldStream defState yieldk single stop m

{-# INLINE tail #-}
tail :: (IsStream t, Monad m) => t m a -> m (Maybe (t m a))
tail m =
    let stop      = return Nothing
        single _  = return $ Just nil
        yieldk _ r = return $ Just r
    in foldStream defState yieldk single stop m

{-# INLINE init #-}
init :: (IsStream t, Monad m) => t m a -> m (Maybe (t m a))
init m = go1 m
    where
    go1 m1 = do
        r <- uncons m1
        case r of
            Nothing -> return Nothing
            Just (h, t) -> return . Just $ go h t
    go p m1 = mkStream $ \_ yld sng stp ->
        let single _ = sng p
            yieldk a x = yld p $ go a x
         in foldStream defState yieldk single stp m1

{-# INLINE elem #-}
elem :: (IsStream t, Monad m, Eq a) => a -> t m a -> m Bool
elem e m = go m
    where
    go m1 =
        let stop      = return False
            single a  = return (a == e)
            yieldk a r = if a == e then return True else go r
        in foldStream defState yieldk single stop m1

{-# INLINE notElem #-}
notElem :: (IsStream t, Monad m, Eq a) => a -> t m a -> m Bool
notElem e m = go m
    where
    go m1 =
        let stop      = return True
            single a  = return (a /= e)
            yieldk a r = if a == e then return False else go r
        in foldStream defState yieldk single stop m1

{-# INLINABLE all #-}
all :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> m Bool
all p m = go m
    where
    go m1 =
        let single a   | p a       = return True
                       | otherwise = return False
            yieldk a r | p a       = go r
                       | otherwise = return False
         in foldStream defState yieldk single (return True) m1

{-# INLINABLE any #-}
any :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> m Bool
any p m = go m
    where
    go m1 =
        let single a   | p a       = return True
                       | otherwise = return False
            yieldk a r | p a       = return True
                       | otherwise = go r
         in foldStream defState yieldk single (return False) m1

-- | Extract the last element of the stream, if any.
{-# INLINE last #-}
last :: (IsStream t, Monad m) => t m a -> m (Maybe a)
last = foldlx' (\_ y -> Just y) Nothing id

{-# INLINE minimum #-}
minimum :: (IsStream t, Monad m, Ord a) => t m a -> m (Maybe a)
minimum m = go Nothing m
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in foldStream defState yieldk single stop m1

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  =
                if res <= a
                then return (Just res)
                else return (Just a)
            yieldk a r =
                if res <= a
                then go (Just res) r
                else go (Just a) r
        in foldStream defState yieldk single stop m1

{-# INLINE minimumBy #-}
minimumBy
    :: (IsStream t, Monad m)
    => (a -> a -> Ordering) -> t m a -> m (Maybe a)
minimumBy cmp m = go Nothing m
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in foldStream defState yieldk single stop m1

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  = case cmp res a of
                GT -> return (Just a)
                _  -> return (Just res)
            yieldk a r = case cmp res a of
                GT -> go (Just a) r
                _  -> go (Just res) r
        in foldStream defState yieldk single stop m1

{-# INLINE maximum #-}
maximum :: (IsStream t, Monad m, Ord a) => t m a -> m (Maybe a)
maximum m = go Nothing m
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in foldStream defState yieldk single stop m1

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  =
                if res <= a
                then return (Just a)
                else return (Just res)
            yieldk a r =
                if res <= a
                then go (Just a) r
                else go (Just res) r
        in foldStream defState yieldk single stop m1

{-# INLINE maximumBy #-}
maximumBy :: (IsStream t, Monad m) => (a -> a -> Ordering) -> t m a -> m (Maybe a)
maximumBy cmp m = go Nothing m
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in foldStream defState yieldk single stop m1

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  = case cmp res a of
                GT -> return (Just res)
                _  -> return (Just a)
            yieldk a r = case cmp res a of
                GT -> go (Just res) r
                _  -> go (Just a) r
        in foldStream defState yieldk single stop m1

{-# INLINE (!!) #-}
(!!) :: (IsStream t, Monad m) => t m a -> Int -> m (Maybe a)
m !! i = go i m
    where
    go n m1 =
      let single a | n == 0 = return $ Just a
                   | otherwise = return Nothing
          yieldk a x | n < 0 = return Nothing
                     | n == 0 = return $ Just a
                     | otherwise = go (n - 1) x
      in foldStream defState yieldk single (return Nothing) m1

{-# INLINE lookup #-}
lookup :: (IsStream t, Monad m, Eq a) => a -> t m (a, b) -> m (Maybe b)
lookup e m = go m
    where
    go m1 =
        let single (a, b) | a == e = return $ Just b
                          | otherwise = return Nothing
            yieldk (a, b) x | a == e = return $ Just b
                            | otherwise = go x
        in foldStream defState yieldk single (return Nothing) m1

{-# INLINE findM #-}
findM :: (IsStream t, Monad m) => (a -> m Bool) -> t m a -> m (Maybe a)
findM p m = go m
    where
    go m1 =
        let single a = do
                b <- p a
                if b then return $ Just a else return Nothing
            yieldk a x = do
                b <- p a
                if b then return $ Just a else go x
        in foldStream defState yieldk single (return Nothing) m1

{-# INLINE find #-}
find :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> m (Maybe a)
find p = findM (return . p)

{-# INLINE findIndices #-}
findIndices :: IsStream t => (a -> Bool) -> t m a -> t m Int
findIndices p = go 0
    where
    go offset m1 = mkStream $ \st yld sng stp ->
        let single a | p a = sng offset
                     | otherwise = stp
            yieldk a x | p a = yld offset $ go (offset + 1) x
                       | otherwise = foldStream (adaptState st) yld sng stp $
                            go (offset + 1) x
        in foldStream (adaptState st) yieldk single stp m1

------------------------------------------------------------------------------
-- Map and Fold
------------------------------------------------------------------------------

-- | Apply a monadic action to each element of the stream and discard the
-- output of the action.
{-# INLINE mapM_ #-}
mapM_ :: (IsStream t, Monad m) => (a -> m b) -> t m a -> m ()
mapM_ f m = go m
    where
    go m1 =
        let stop = return ()
            single a = void (f a)
            yieldk a r = f a >> go r
         in foldStream defState yieldk single stop m1

------------------------------------------------------------------------------
-- Converting folds
------------------------------------------------------------------------------

{-# INLINABLE toList #-}
toList :: (IsStream t, Monad m) => t m a -> m [a]
toList = foldr (:) []

{-# INLINE toStreamK #-}
toStreamK :: Stream m a -> Stream m a
toStreamK = id

-------------------------------------------------------------------------------
-- Transformation by folding (Scans)
-------------------------------------------------------------------------------

{-# INLINE scanlx' #-}
scanlx' :: IsStream t => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scanlx' step begin done m =
    cons (done begin) $ go m begin
    where
    go m1 !acc = mkStream $ \st yld sng stp ->
        let single a = sng (done $ step acc a)
            yieldk a r =
                let s = step acc a
                in yld (done s) (go r s)
        in foldStream (adaptState st) yieldk single stp m1

{-# INLINE scanl' #-}
scanl' :: IsStream t => (b -> a -> b) -> b -> t m a -> t m b
scanl' step begin = scanlx' step begin id

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE filter #-}
filter :: IsStream t => (a -> Bool) -> t m a -> t m a
filter p m = go m
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single a   | p a       = sng a
                       | otherwise = stp
            yieldk a r | p a       = yld a (go r)
                       | otherwise = foldStream st yieldk single stp r
         in foldStream st yieldk single stp m1

{-# INLINE take #-}
take :: IsStream t => Int -> t m a -> t m a
take n m = go n m
    where
    go n1 m1 = mkStream $ \st yld sng stp ->
        let yieldk a r = yld a (go (n1 - 1) r)
        in if n1 <= 0
           then stp
           else foldStream st yieldk sng stp m1

{-# INLINE takeWhile #-}
takeWhile :: IsStream t => (a -> Bool) -> t m a -> t m a
takeWhile p m = go m
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single a   | p a       = sng a
                       | otherwise = stp
            yieldk a r | p a       = yld a (go r)
                       | otherwise = stp
         in foldStream st yieldk single stp m1

{-# INLINE drop #-}
drop :: IsStream t => Int -> t m a -> t m a
drop n m = fromStream $ unShare (go n (toStream m))
    where
    go n1 m1 = mkStream $ \st yld sng stp ->
        let single _ = stp
            yieldk _ r = foldStreamShared st yld sng stp $ go (n1 - 1) r
        -- Somehow "<=" check performs better than a ">"
        in if n1 <= 0
           then foldStreamShared st yld sng stp m1
           else foldStreamShared st yieldk single stp m1

{-# INLINE dropWhile #-}
dropWhile :: IsStream t => (a -> Bool) -> t m a -> t m a
dropWhile p m = go m
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single a   | p a       = stp
                       | otherwise = sng a
            yieldk a r | p a = foldStream st yieldk single stp r
                       | otherwise = yld a r
         in foldStream st yieldk single stp m1

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

-- Be careful when modifying this, this uses a consM (|:) deliberately to allow
-- other stream types to overload it.
{-# INLINE mapM #-}
mapM :: (IsStream t, MonadAsync m) => (a -> m b) -> t m a -> t m b
mapM f m = go m
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single a  = f a >>= sng
            yieldk a r = foldStreamShared st yld sng stp $ f a |: go r
         in foldStream (adaptState st) yieldk single stp m1

-- Be careful when modifying this, this uses a consM (|:) deliberately to allow
-- other stream types to overload it.
{-# INLINE sequence #-}
sequence :: (IsStream t, MonadAsync m) => t m (m a) -> t m a
sequence m = go m
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single ma = ma >>= sng
            yieldk ma r = foldStreamShared st yld sng stp $ ma |: go r
         in foldStream (adaptState st) yieldk single stp m1

-------------------------------------------------------------------------------
-- Inserting
-------------------------------------------------------------------------------

{-# INLINE intersperseM #-}
intersperseM :: (IsStream t, MonadAsync m) => m a -> t m a -> t m a
intersperseM a m = prependingStart m
    where
    prependingStart m1 = mkStream $ \st yld sng stp ->
        let yieldk i x = foldStreamShared st yld sng stp $ return i |: go x
         in foldStream st yieldk sng stp m1
    go m2 = mkStream $ \st yld sng stp ->
        let single i = foldStreamShared st yld sng stp $ a |: yield i
            yieldk i x = foldStreamShared st yld sng stp $ a |: return i |: go x
         in foldStream st yieldk single stp m2

{-# INLINE insertBy #-}
insertBy :: IsStream t => (a -> a -> Ordering) -> a -> t m a -> t m a
insertBy cmp x m = go m
  where
    go m1 = mkStream $ \st yld _ _ ->
        let single a = case cmp x a of
                GT -> yld a (yield x)
                _  -> yld x (yield a)
            stop = yld x nil
            yieldk a r = case cmp x a of
                GT -> yld a (go r)
                _  -> yld x (a `cons` r)
         in foldStream st yieldk single stop m1

------------------------------------------------------------------------------
-- Deleting
------------------------------------------------------------------------------

{-# INLINE deleteBy #-}
deleteBy :: IsStream t => (a -> a -> Bool) -> a -> t m a -> t m a
deleteBy eq x m = go m
  where
    go m1 = mkStream $ \st yld sng stp ->
        let single a = if eq x a then stp else sng a
            yieldk a r = if eq x a
              then foldStream st yld sng stp r
              else yld a (go r)
         in foldStream st yieldk single stp m1

------------------------------------------------------------------------------
-- Reordering
------------------------------------------------------------------------------

{-# INLINE reverse #-}
reverse :: IsStream t => t m a -> t m a
reverse = foldlS (flip cons) nil

-------------------------------------------------------------------------------
-- Map and Filter
-------------------------------------------------------------------------------

{-# INLINE mapMaybe #-}
mapMaybe :: IsStream t => (a -> Maybe b) -> t m a -> t m b
mapMaybe f m = go m
  where
    go m1 = mkStream $ \st yld sng stp ->
        let single a = case f a of
                Just b  -> sng b
                Nothing -> stp
            yieldk a r = case f a of
                Just b  -> yld b $ go r
                Nothing -> foldStream (adaptState st) yieldk single stp r
        in foldStream (adaptState st) yieldk single stp m1

------------------------------------------------------------------------------
-- Serial Zipping
------------------------------------------------------------------------------

-- | Zip two streams serially using a pure zipping function.
--
-- @since 0.1.0
{-# INLINABLE zipWith #-}
zipWith :: IsStream t => (a -> b -> c) -> t m a -> t m b -> t m c
zipWith f = go
    where
    go mx my = mkStream $ \st yld sng stp -> do
        let merge a ra =
                let single2 b = sng (f a b)
                    yield2 b rb = yld (f a b) (go ra rb)
                 in foldStream (adaptState st) yield2 single2 stp my
        let single1 a = merge a nil
            yield1 = merge
        foldStream (adaptState st) yield1 single1 stp mx

-- | Zip two streams serially using a monadic zipping function.
--
-- @since 0.1.0
{-# INLINABLE zipWithM #-}
zipWithM :: (IsStream t, Monad m) => (a -> b -> m c) -> t m a -> t m b -> t m c
zipWithM f m1 m2 = go m1 m2
    where
    go mx my = mkStream $ \st yld sng stp -> do
        let merge a ra =
                let runIt x = foldStream st yld sng stp x
                    single2 b   = f a b >>= sng
                    yield2 b rb = f a b >>= \x -> runIt (x `cons` go ra rb)
                 in foldStream (adaptState st) yield2 single2 stp my
        let single1 a = merge a nil
            yield1 = merge
        foldStream (adaptState st) yield1 single1 stp mx

------------------------------------------------------------------------------
-- Merging
------------------------------------------------------------------------------

{-# INLINE mergeByM #-}
mergeByM
    :: (IsStream t, Monad m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeByM cmp = go
    where
    go mx my = mkStream $ \st yld sng stp -> do
        let mergeWithY a ra =
                let stop2 = foldStream st yld sng stp mx
                    single2 b = do
                        r <- cmp a b
                        case r of
                            GT -> yld b (go (a `cons` ra) nil)
                            _  -> yld a (go ra (b `cons` nil))
                    yield2 b rb = do
                        r <- cmp a b
                        case r of
                            GT -> yld b (go (a `cons` ra) rb)
                            _  -> yld a (go ra (b `cons` rb))
                 in foldStream st yield2 single2 stop2 my
        let stopX = foldStream st yld sng stp my
            singleX a = mergeWithY a nil
            yieldX = mergeWithY
        foldStream st yieldX singleX stopX mx

{-# INLINABLE mergeBy #-}
mergeBy
    :: (IsStream t, Monad m)
    => (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeBy cmp = mergeByM (\a b -> return $ cmp a b)

------------------------------------------------------------------------------
-- Transformation comprehensions
------------------------------------------------------------------------------

{-# INLINE the #-}
the :: (Eq a, IsStream t, Monad m) => t m a -> m (Maybe a)
the m = do
    r <- uncons m
    case r of
        Nothing -> return Nothing
        Just (h, t) -> go h t
    where
    go h m1 =
        let single a   | h == a    = return $ Just h
                       | otherwise = return Nothing
            yieldk a r | h == a    = go h r
                       | otherwise = return Nothing
         in foldStream defState yieldk single (return $ Just h) m1

-------------------------------------------------------------------------------
-- Bind utility
-------------------------------------------------------------------------------

{-# INLINE bindWith #-}
bindWith
    :: IsStream t
    => (forall c. t m c -> t m c -> t m c)
    -> t m a
    -> (a -> t m b)
    -> t m b
bindWith par m1 f = go m1
    where
        go m =
            mkStream $ \st yld sng stp ->
                let foldShared = foldStreamShared st yld sng stp
                    single a   = foldShared $ unShare (f a)
                    yieldk a r = foldShared $ unShare (f a) `par` go r
                in foldStream (adaptState st) yieldk single stp m

------------------------------------------------------------------------------
-- Alternative & MonadPlus
------------------------------------------------------------------------------

_alt :: Stream m a -> Stream m a -> Stream m a
_alt m1 m2 = mkStream $ \st yld sng stp ->
    let stop  = foldStream st yld sng stp m2
    in foldStream st yld sng stop m1

------------------------------------------------------------------------------
-- MonadReader
------------------------------------------------------------------------------

{-# INLINABLE withLocal #-}
withLocal :: MonadReader r m => (r -> r) -> Stream m a -> Stream m a
withLocal f m =
    mkStream $ \st yld sng stp ->
        let single = local f . sng
            yieldk a r = local f $ yld a (withLocal f r)
        in foldStream st yieldk single (local f stp) m

------------------------------------------------------------------------------
-- MonadError
------------------------------------------------------------------------------

{-
-- XXX handle and test cross thread state transfer
withCatchError
    :: MonadError e m
    => Stream m a -> (e -> Stream m a) -> Stream m a
withCatchError m h =
    mkStream $ \_ stp sng yld ->
        let run x = unStream x Nothing stp sng yieldk
            handle r = r `catchError` \e -> run $ h e
            yieldk a r = yld a (withCatchError r h)
        in handle $ run m
-}
