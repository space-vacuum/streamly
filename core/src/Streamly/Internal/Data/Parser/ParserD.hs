#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Parser.ParserD
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Direct style parser implementation with stream fusion.

module Streamly.Internal.Data.Parser.ParserD
    (
      Parser (..)
    , ParseError (..)
    , Step (..)
    , Initial (..)
    , rmapM

    -- * Conversion to/from ParserK
    , fromParserK
    , toParserK

    -- * Downgrade to Fold
    , toFold

    -- First order parsers
    -- * Accumulators
    , fromFold
    , fromFoldMaybe
    , fromPure
    , fromEffect
    , die
    , dieM

    -- * Map on input
    , lmap
    , lmapM
    , postscan
    , filter

    -- * Element parsers
    , peek
    , eof
    , satisfy
    , next
    , maybe
    , either

    -- * Sequence parsers
    --
    -- Parsers chained in series, if one parser terminates the composition
    -- terminates. Currently we are using folds to collect the output of the
    -- parsers but we can use Parsers instead of folds to make the composition
    -- more powerful. For example, we can do:
    --
    -- takeEndByOrMax cond n p = takeEndBy cond (take n p)
    -- takeEndByBetween cond m n p = takeEndBy cond (takeBetween m n p)
    -- takeWhileBetween cond m n p = takeWhile cond (takeBetween m n p)

    -- Grab a sequence of input elements without inspecting them
    , takeBetween
    -- , take -- take   -- takeBetween 0 n
    , takeEQ -- takeBetween n n
    , takeGE -- takeBetween n maxBound
    -- , takeGE1 -- take1 -- takeBetween 1 n
    , takeP

    -- Grab a sequence of input elements by inspecting them
    , lookAhead
    , takeWhile
    , takeWhileP
    , takeWhile1

    -- Separators
    , takeEndBy
    , takeEndBy_
    , takeEndByEsc
    , takeStartBy
    , takeFramedBy_
    , takeFramedByEsc_
    , takeFramedByGeneric

    -- Words and grouping
    , wordBy
    , wordFramedBy
    , wordQuotedBy
    , groupBy
    , groupByRolling
    , groupByRollingEither

    -- Matching strings
    , eqBy
    , matchBy
    -- , prefixOf -- match any prefix of a given string
    -- , suffixOf -- match any suffix of a given string
    -- , infixOf -- match any substring of a given string

    -- ** Spanning
    , span
    , spanBy
    , spanByRolling

    -- Second order parsers (parsers using parsers)
    -- * Binary Combinators

    -- ** Sequential Applicative
    , serialWith
    , split_

    -- ** Parallel Applicatives
    , teeWith
    , teeWithFst
    , teeWithMin
    -- , teeTill -- like manyTill but parallel

    -- ** Sequential Interleaving
    -- Use two folds, run a primary parser, its rejected values go to the
    -- secondary parser.
    , deintercalate
    , sepBy

    -- ** Sequential Alternative
    , alt

    -- ** Parallel Alternatives
    , shortest
    , longest
    -- , fastest

    -- * N-ary Combinators
    -- ** Sequential Collection
    , sequence
    , concatMap

    -- ** Sequential Repetition
    , count
    , countBetween
    -- , countBetweenTill

    , many
    , manyP
    , some
    , manyTill

    -- -- ** Special cases
    -- XXX traditional implmentations of these may be of limited use. For
    -- example, consider parsing lines separated by "\r\n". The main parser
    -- will have to detect and exclude the sequence "\r\n" anyway so that we
    -- can apply the "sep" parser.
    --
    -- We can instead implement these as special cases of deintercalate.
    --
    -- , endBy
    -- , sepBy
    -- , sepEndBy
    -- , beginBy
    -- , sepBeginBy
    -- , sepAroundBy

    -- -- * Distribution
    --
    -- A simple and stupid impl would be to just convert the stream to an array
    -- and give the array reference to all consumers. The array can be grown on
    -- demand by any consumer and truncated when nonbody needs it.
    --
    -- -- ** Distribute to collection
    -- -- ** Distribute to repetition

    -- -- ** Interleaved collection
    -- Round robin
    -- Priority based
    -- -- ** Interleaved repetition
    -- repeat one parser and when it fails run an error recovery parser
    -- e.g. to find a key frame in the stream after an error

    -- ** Collection of Alternatives
    -- , shortestN
    -- , longestN
    -- , fastestN -- first N successful in time
    -- , choiceN  -- first N successful in position
    , choice   -- first successful in position

    -- -- ** Repeated Alternatives
    -- , retryMax    -- try N times
    -- , retryUntil  -- try until successful
    -- , retryUntilN -- try until successful n times

    -- ** Zipping Input
    , zipWithM
    , zip
    , indexed
    , makeIndexFilter
    , sampleFromthen
    )
where

import Control.Exception (assert, Exception)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch, MonadThrow(..))
import Data.Bifunctor (first)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.SVar.Type (defState)
import Streamly.Internal.Data.Either.Strict (Either'(..))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as D

import Prelude hiding
       (any, all, take, takeWhile, sequence, concatMap, maybe, either, span
       , zip, filter)
import Streamly.Internal.Data.Parser.ParserD.NonFailing
import Streamly.Internal.Data.Parser.ParserD.Tee
import Streamly.Internal.Data.Parser.ParserD.Type

--
-- $setup
-- >>> :m
-- >>> :set -package streamly
-- >>> import Prelude hiding ()
-- >>> import qualified Data.Maybe as Maybe
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Parser as Parser

-------------------------------------------------------------------------------
-- Downgrade a parser to a Fold
-------------------------------------------------------------------------------

data ParserToFoldError =
      InitialError String
    | PartialError Int
    | ContinueError Int
    | DoneError Int
    | ErrorError String
    deriving Show

instance Exception ParserToFoldError

-- | See 'Streamly.Internal.Data.Parser.toFold'.
--
-- /Internal/
--
{-# INLINE toFold #-}
toFold :: MonadThrow m => Parser m a b -> Fold m a b
toFold (Parser pstep pinitial pextract) = Fold step initial pextract

    where

    initial = do
        r <- pinitial
        case r of
            IPartial s -> return $ FL.Partial s
            IDone b -> return $ FL.Done b
            IError err -> throwM $ InitialError err

    step st a = do
        r <- pstep st a
        case r of
            Partial 0 s -> return $ FL.Partial s
            Continue 0 s -> return $ FL.Partial s
            Done 0 b -> return $ FL.Done b
            Partial n _ -> throwM $ PartialError n
            Continue n _ -> throwM $ ContinueError n
            Done n _ -> throwM $ DoneError n
            Error err -> throwM $ ErrorError err

-------------------------------------------------------------------------------
-- Upgrade folds to parses
-------------------------------------------------------------------------------
-- XXX fromMaybeFold

-- | Convert Maybe returning folds to error returning parsers.
--
-- /Pre-release/
--
{-# INLINE fromFoldMaybe #-}
fromFoldMaybe :: MonadThrow m => String -> Fold m a (Maybe b) -> Parser m a b
fromFoldMaybe errMsg (Fold fstep finitial fextract) = Parser step initial extract

    where

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s1 -> IPartial s1
                  FL.Done b ->
                        case b of
                            Just x -> IDone x
                            Nothing -> IError errMsg

    step s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Partial s1 -> Partial 0 s1
                  FL.Done b ->
                        case b of
                            Just x -> Done 0 x
                            Nothing -> Error errMsg

    extract s = do
        res <- fextract s
        case res of
            Just x -> return x
            Nothing -> throwM $ ParseError errMsg

-------------------------------------------------------------------------------
-- Failing Parsers
-------------------------------------------------------------------------------

-- | See 'Streamly.Internal.Data.Parser.peek'.
--
-- /Pre-release/
--
{-# INLINE peek #-}
peek :: MonadThrow m => Parser m a a
peek = Parser step initial extract

    where

    initial = return $ IPartial ()

    step () a = return $ Done 1 a

    extract () = throwM $ ParseError "peek: end of input"

-- | See 'Streamly.Internal.Data.Parser.eof'.
--
-- /Pre-release/
--
{-# INLINE eof #-}
eof :: Monad m => Parser m a ()
eof = Parser step initial return

    where

    initial = return $ IPartial ()

    step () _ = return $ Error "eof: not at end of input"

-- | See 'Streamly.Internal.Data.Parser.either'.
--
-- /Pre-release/
--
{-# INLINE either #-}
either :: MonadThrow m => (a -> Either String b) -> Parser m a b
either f = Parser step initial extract

    where

    initial = return $ IPartial ()

    step () a = return $
        case f a of
            Right b -> Done 0 b
            Left err -> Error err

    extract _ = throwM $ ParseError "end of input"

-- | See 'Streamly.Internal.Data.Parser.maybe'.
--
-- >>> toEither = Maybe.maybe (Left "maybe: predicate failed") Right
-- >>> maybe f = Parser.either (toEither . f)
--
-- /Pre-release/
--
{-# INLINE maybe #-}
maybe :: MonadThrow m => (a -> Maybe b) -> Parser m a b
-- maybe f = either (Maybe.maybe (Left "maybe: predicate failed") Right . f)
maybe parserF = Parser step initial extract

    where

    initial = return $ IPartial ()

    step () a = return $
        case parserF a of
            Just b -> Done 0 b
            Nothing -> Error "maybe: predicate failed"

    extract _ = throwM $ ParseError "maybe: end of input"

-- | See 'Streamly.Internal.Data.Parser.satisfy'.
--
-- >>> toMaybe f x = if f x then Just x else Nothing
-- >>> satisfy f = Parser.maybe (toMaybe f)
--
-- /Pre-release/
--
{-# INLINE satisfy #-}
satisfy :: MonadThrow m => (a -> Bool) -> Parser m a a
-- satisfy predicate = maybe (\a -> if predicate a then Just a else Nothing)
satisfy predicate = Parser step initial extract

    where

    initial = return $ IPartial ()

    step () a = return $
        if predicate a
        then Done 0 a
        else Error "satisfy: predicate failed"

    extract _ = throwM $ ParseError "satisfy: end of input"

-------------------------------------------------------------------------------
-- Taking elements
-------------------------------------------------------------------------------

-- Required to fuse "take" with "many" in "chunksOf", for ghc-9.x
{-# ANN type Tuple'Fused Fuse #-}
data Tuple'Fused a b = Tuple'Fused !a !b deriving Show

-- | See 'Streamly.Internal.Data.Parser.takeBetween'.
--
-- /Pre-release/
--
{-# INLINE takeBetween #-}
takeBetween :: MonadCatch m => Int -> Int -> Fold m a b -> Parser m a b
takeBetween low high (Fold fstep finitial fextract) =

    Parser step initial (extract streamErr)

    where

    streamErr i =
           "takeBetween: Expecting alteast " ++ show low
        ++ " elements, got " ++ show i

    invalidRange =
        "takeBetween: lower bound - " ++ show low
            ++ " is greater than higher bound - " ++ show high

    foldErr :: Int -> String
    foldErr i =
        "takeBetween: the collecting fold terminated after"
            ++ " consuming" ++ show i ++ " elements"
            ++ " minimum" ++ show low ++ " elements needed"

    -- Exactly the same as snext except different constructors, we can possibly
    -- deduplicate the two.
    {-# INLINE inext #-}
    inext i res =
        let i1 = i + 1
        in case res of
            FL.Partial s -> do
                let s1 = Tuple'Fused i1 s
                if i1 < high
                -- XXX ideally this should be a Continue instead
                then return $ IPartial s1
                else IDone <$> extract foldErr s1
            FL.Done b ->
                return
                    $ if i1 >= low
                      then IDone b
                      else IError (foldErr i1)

    initial = do
        when (low >= 0 && high >= 0 && low > high)
            $ throwM $ ParseError invalidRange

        finitial >>= inext (-1)

    -- Keep the impl same as inext
    {-# INLINE snext #-}
    snext i res =
        let i1 = i + 1
        in case res of
            FL.Partial s -> do
                let s1 = Tuple'Fused i1 s
                if i1 < high
                then return $ Continue 0 s1
                else Done 0 <$> extract foldErr s1
            FL.Done b ->
                return
                    $ if i1 >= low
                      then Done 0 b
                      else Error (foldErr i1)

    step (Tuple'Fused i s) a = fstep s a >>= snext i

    extract f (Tuple'Fused i s)
        | i >= low && i <= high = fextract s
        | otherwise = throwM $ ParseError (f i)

-- | See 'Streamly.Internal.Data.Parser.takeEQ'.
--
-- /Pre-release/
--
{-# INLINE takeEQ #-}
takeEQ :: MonadThrow m => Int -> Fold m a b -> Parser m a b
takeEQ n (Fold fstep finitial fextract) = Parser step initial extract

    where

    cnt = max n 0

    initial = do
        res <- finitial
        return $ case res of
            FL.Partial s -> IPartial $ Tuple' 0 s
            FL.Done b ->
                if cnt == 0
                then IDone b
                else IError
                         $ "takeEQ: Expecting exactly " ++ show cnt
                             ++ " elements, fold terminated without"
                             ++ " consuming any elements"

    step (Tuple' i r) a
        | i1 < cnt = do
            res <- fstep r a
            return
                $ case res of
                    FL.Partial s -> Continue 0 $ Tuple' i1 s
                    FL.Done _ ->
                        Error
                            $ "takeEQ: Expecting exactly " ++ show cnt
                                ++ " elements, fold terminated on " ++ show i1
        | i1 == cnt = do
            res <- fstep r a
            Done 0
                <$> case res of
                        FL.Partial s -> fextract s
                        FL.Done b -> return b
        -- XXX we should not reach here when initial returns Step type
        -- reachable only when n == 0
        | otherwise = Done 1 <$> fextract r

        where

        i1 = i + 1

    extract (Tuple' i r)
        | i == 0 && cnt == 0 = fextract r
        | otherwise =
            throwM
                $ ParseError
                $ "takeEQ: Expecting exactly " ++ show cnt
                    ++ " elements, input terminated on " ++ show i

-- | See 'Streamly.Internal.Data.Parser.takeGE'.
--
-- /Pre-release/
--
{-# INLINE takeGE #-}
takeGE :: MonadThrow m => Int -> Fold m a b -> Parser m a b
takeGE n (Fold fstep finitial fextract) = Parser step initial extract

    where

    cnt = max n 0
    initial = do
        res <- finitial
        return $ case res of
            FL.Partial s -> IPartial $ Tuple' 0 s
            FL.Done b ->
                if cnt == 0
                then IDone b
                else IError
                         $ "takeGE: Expecting at least " ++ show cnt
                             ++ " elements, fold terminated without"
                             ++ " consuming any elements"

    step (Tuple' i r) a
        | i1 < cnt = do
            res <- fstep r a
            return
                $ case res of
                      FL.Partial s -> Continue 0 $ Tuple' i1 s
                      FL.Done _ ->
                        Error
                            $ "takeGE: Expecting at least " ++ show cnt
                                ++ " elements, fold terminated on " ++ show i1
        | otherwise = do
            res <- fstep r a
            return
                $ case res of
                      FL.Partial s -> Partial 0 $ Tuple' i1 s
                      FL.Done b -> Done 0 b

        where

        i1 = i + 1

    extract (Tuple' i r)
        | i >= cnt = fextract r
        | otherwise =
            throwM
                $ ParseError
                $ "takeGE: Expecting at least " ++ show cnt
                    ++ " elements, input terminated on " ++ show i

-------------------------------------------------------------------------------
-- Conditional splitting
-------------------------------------------------------------------------------

-- | See 'Streamly.Internal.Data.Parser.takeWhileP'.
--
-- /Pre-release/
--
{-# INLINE takeWhileP #-}
takeWhileP :: Monad m => (a -> Bool) -> Parser m a b -> Parser m a b
takeWhileP predicate (Parser pstep pinitial pextract) =
    Parser step pinitial pextract

    where

    step s a =
        if predicate a
        then pstep s a
        else Done 1 <$> pextract s

-- | See 'Streamly.Internal.Data.Parser.takeWhile1'.
--
-- /Pre-release/
--
{-# INLINE takeWhile1 #-}
takeWhile1 :: MonadThrow m => (a -> Bool) -> Fold m a b -> Parser m a b
takeWhile1 predicate (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial = do
        res <- finitial
        return $ case res of
            FL.Partial s -> IPartial (Left' s)
            FL.Done _ ->
                IError
                    $ "takeWhile1: fold terminated without consuming:"
                          ++ " any element"

    {-# INLINE process #-}
    process s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Partial s1 -> Partial 0 (Right' s1)
                  FL.Done b -> Done 0 b

    step (Left' s) a =
        if predicate a
        then process s a
        else return $ Error "takeWhile1: predicate failed on first element"
    step (Right' s) a =
        if predicate a
        then process s a
        else do
            b <- fextract s
            return $ Done 1 b

    extract (Left' _) = throwM $ ParseError "takeWhile1: end of input"
    extract (Right' s) = fextract s

-------------------------------------------------------------------------------
-- Separators
-------------------------------------------------------------------------------

{-# INLINE takeFramedByGeneric #-}
takeFramedByGeneric :: MonadThrow m =>
       Maybe (a -> Bool)
    -> Maybe (a -> Bool)
    -> Maybe (a -> Bool)
    -> Fold m a b
    -> Parser m a b
takeFramedByGeneric esc begin end (Fold fstep finitial fextract) =

    Parser step initial extract

    where

    initial =  do
        res <- finitial
        return $
            case res of
                FL.Partial s -> IPartial (FrameEscInit s)
                FL.Done _ ->
                    error "takeFramedByGeneric: fold done without input"

    {-# INLINE process #-}
    process s a n = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Continue 0 (FrameEscGo s1 n)
                FL.Done b -> Done 0 b

    {-# INLINE processNoEsc #-}
    processNoEsc s a n =
        case end of
            Just isEnd ->
                case begin of
                    Just isBegin ->
                        -- takeFramedBy case
                        if isEnd a
                        then
                            if n == 0
                            then Done 0 <$> fextract s
                            else process s a (n - 1)
                        else
                            let n1 = if isBegin a then n + 1 else n
                             in process s a n1
                    Nothing -> -- takeEndBy case
                        if isEnd a
                        then Done 0 <$> fextract s
                        else process s a n
            Nothing -> -- takeStartBy case
                case begin of
                    Just isBegin ->
                        if isBegin a
                        then Done 0 <$> fextract s
                        else process s a n
                    Nothing ->
                        error $ "takeFramedByGeneric: "
                            ++ "Both begin and end frame predicate missing"

    {-# INLINE processCheckEsc #-}
    processCheckEsc s a n =
        case esc of
            Just isEsc ->
                if isEsc a
                then return $ Partial 0 $ FrameEscEsc s n
                else processNoEsc s a n
            Nothing -> processNoEsc s a n

    step (FrameEscInit s) a =
        case begin of
            Just isBegin ->
                if isBegin a
                then return $ Partial 0 (FrameEscGo s 0)
                else return $ Error "takeFramedByGeneric: missing frame start"
            Nothing ->
                case end of
                    Just isEnd ->
                        if isEnd a
                        then Done 0 <$> fextract s
                        else processCheckEsc s a 0
                    Nothing ->
                        error "Both begin and end frame predicate missing"
    step (FrameEscGo s n) a = processCheckEsc s a n
    step (FrameEscEsc s n) a = process s a n

    err = throwM . ParseError

    extract (FrameEscInit _) =
        err "takeFramedByGeneric: empty token"
    extract (FrameEscGo s _) =
        case begin of
            Just _ ->
                case end of
                    Nothing -> fextract s
                    Just _ -> err "takeFramedByGeneric: missing frame end"
            Nothing -> err "takeFramedByGeneric: missing closing frame"
    extract (FrameEscEsc _ _) = err "takeFramedByGeneric: trailing escape"

-- | See 'Streamly.Internal.Data.Parser.takeEndBy'.
--
-- /Pre-release/
--
{-# INLINE takeEndBy #-}
takeEndBy :: MonadCatch m => (a -> Bool) -> Parser m a b -> Parser m a b
takeEndBy cond (Parser pstep pinitial pextract) =

    Parser step initial pextract

    where

    initial = pinitial

    step s a = do
        res <- pstep s a
        if not (cond a)
        then return res
        else extractStep pextract res

-- | See 'Streamly.Internal.Data.Parser.takeEndByEsc'.
--
-- /Pre-release/
--
{-# INLINE takeEndByEsc #-}
takeEndByEsc :: MonadCatch m =>
    (a -> Bool) -> (a -> Bool) -> Parser m a b -> Parser m a b
takeEndByEsc isEsc isSep (Parser pstep pinitial pextract) =

    Parser step initial extract

    where

    initial = first Left' <$> pinitial

    step (Left' s) a = do
        if isEsc a
        then return $ Partial 0 $ Right' s
        else do
            res <- pstep s a
            if not (isSep a)
            then return $ mapStateStep Left' res
            else extractStep pextract res

    step (Right' s) a = do
        res <- pstep s a
        return $ mapStateStep Left' res

    extract (Left' s) = pextract s
    extract (Right' _) =
        throwM $ ParseError "takeEndByEsc: trailing escape"

-- | See 'Streamly.Internal.Data.Parser.takeEndBy_'.
--
-- /Pre-release/
--
{-# INLINE takeEndBy_ #-}
takeEndBy_ :: MonadCatch m =>
    (a -> Bool) -> Parser m a b -> Parser m a b
takeEndBy_ cond (Parser pstep pinitial pextract) =

    Parser step initial pextract

    where

    initial = pinitial

    step s a =
        if cond a
        then do
            res <- pextract s
            return $ Done 0 res
        else pstep s a

-- | See 'Streamly.Internal.Data.Parser.takeStartBy'.
--
-- /Pre-release/
--

{-# INLINE takeStartBy #-}
takeStartBy :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
takeStartBy cond (Fold fstep finitial fextract) =

    Parser step initial extract

    where

    initial =  do
        res <- finitial
        return $
            case res of
                FL.Partial s -> IPartial (Left' s)
                FL.Done _ -> IError "takeStartBy: fold done without input"

    {-# INLINE process #-}
    process s a = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Partial 0 (Right' s1)
                FL.Done b -> Done 0 b

    step (Left' s) a =
        if cond a
        then process s a
        else return $ Error "takeStartBy: missing frame start"
    step (Right' s) a =
        if not (cond a)
        then process s a
        else Done 1 <$> fextract s

    extract (Left' s) = fextract s
    extract (Right' s) = fextract s

data FramedEscState s =
    FrameEscInit !s | FrameEscGo !s !Int | FrameEscEsc !s !Int

{-# INLINE takeFramedByEsc_ #-}
takeFramedByEsc_ :: MonadThrow m =>
    (a -> Bool) -> (a -> Bool) -> (a -> Bool) -> Fold m a b -> Parser m a b
takeFramedByEsc_ isEsc isBegin isEnd (Fold fstep finitial fextract) =

    Parser step initial extract

    where

    initial =  do
        res <- finitial
        return $
            case res of
                FL.Partial s -> IPartial (FrameEscInit s)
                FL.Done _ ->
                    error "takeFramedByEsc_: fold done without input"

    {-# INLINE process #-}
    process s a n = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Continue 0 (FrameEscGo s1 n)
                FL.Done b -> Done 0 b

    step (FrameEscInit s) a =
        if isBegin a
        then return $ Partial 0 (FrameEscGo s 0)
        else return $ Error "takeFramedByEsc_: missing frame start"
    step (FrameEscGo s n) a =
        if isEsc a
        then return $ Partial 0 $ FrameEscEsc s n
        else do
            if not (isEnd a)
            then
                let n1 = if isBegin a then n + 1 else n
                 in process s a n1
            else
                if n == 0
                then Done 0 <$> fextract s
                else process s a (n - 1)
    step (FrameEscEsc s n) a = process s a n

    err = throwM . ParseError

    extract (FrameEscInit _) = err "takeFramedByEsc_: empty token"
    extract (FrameEscGo _ _) = err "takeFramedByEsc_: missing frame end"
    extract (FrameEscEsc _ _) = err "takeFramedByEsc_: trailing escape"

data FramedState s = FrameInit !s | FrameGo !s Int

{-# INLINE takeFramedBy_ #-}
takeFramedBy_ :: MonadThrow m =>
    (a -> Bool) -> (a -> Bool) -> Fold m a b -> Parser m a b
takeFramedBy_ isBegin isEnd (Fold fstep finitial fextract) =

    Parser step initial extract

    where

    initial =  do
        res <- finitial
        return $
            case res of
                FL.Partial s -> IPartial (FrameInit s)
                FL.Done _ ->
                    error "takeFramedBy_: fold done without input"

    {-# INLINE process #-}
    process s a n = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Continue 0 (FrameGo s1 n)
                FL.Done b -> Done 0 b

    step (FrameInit s) a =
        if isBegin a
        then return $ Continue 0 (FrameGo s 0)
        else return $ Error "takeFramedBy_: missing frame start"
    step (FrameGo s n) a
        | not (isEnd a) =
            let n1 = if isBegin a then n + 1 else n
             in process s a n1
        | n == 0 = Done 0 <$> fextract s
        | otherwise = process s a (n - 1)

    err = throwM . ParseError

    extract (FrameInit _) = err "takeFramedBy_: empty token"
    extract (FrameGo _ _) = err "takeFramedBy_: missing frame end"

-------------------------------------------------------------------------------
-- Grouping and words
-------------------------------------------------------------------------------

data WordFramedState s b =
      WordFramedSkipPre !s
    | WordFramedWord !s !Int
    | WordFramedEsc !s !Int
    | WordFramedSkipPost !b

-- | See 'Streamly.Internal.Data.Parser.wordFramedBy'
--
{-# INLINE wordFramedBy #-}
wordFramedBy :: MonadCatch m =>
       (a -> Bool)  -- ^ Escape
    -> (a -> Bool)  -- ^ left quote
    -> (a -> Bool)  -- ^ right quote
    -> (a -> Bool)  -- ^ word seperator
    -> Fold m a b
    -> Parser m a b
wordFramedBy isEsc isBegin isEnd isSep
    (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial =  do
        res <- finitial
        return $
            case res of
                FL.Partial s -> IPartial (WordFramedSkipPre s)
                FL.Done _ ->
                    error "wordFramedBy: fold done without input"

    {-# INLINE process #-}
    process s a n = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Continue 0 (WordFramedWord s1 n)
                FL.Done b -> Done 0 b

    step (WordFramedSkipPre s) a
        | isEsc a = return $ Continue 0 $ WordFramedEsc s 0
        | isSep a = return $ Partial 0 $ WordFramedSkipPre s
        | isBegin a = return $ Continue 0 $ WordFramedWord s 1
        | isEnd a =
            return $ Error "wordFramedBy: missing frame start"
        | otherwise = process s a 0
    step (WordFramedWord s n) a
        | isEsc a = return $ Continue 0 $ WordFramedEsc s n
        | n == 0 && isSep a = do
            b <- fextract s
            return $ Partial 0 $ WordFramedSkipPost b
        | otherwise = do
            -- We need to use different order for checking begin and end for
            -- the n == 0 and n == 1 case so that when the begin and end
            -- character is the same we treat the one after begin as end.
            if n == 0
            then
               -- Need to check isBegin first
               if isBegin a
               then return $ Continue 0 $ WordFramedWord s 1
               else if isEnd a
                    then return $ Error "wordFramedBy: missing frame start"
                    else process s a n
            else
               -- Need to check isEnd first
                if isEnd a
                then
                   if n == 1
                   then return $ Continue 0 $ WordFramedWord s 0
                   else process s a (n - 1)
                else if isBegin a
                     then process s a (n + 1)
                     else process s a n
    step (WordFramedEsc s n) a = process s a n
    step (WordFramedSkipPost b) a =
        return
            $ if not (isSep a)
              then Done 1 b
              else Partial 0 $ WordFramedSkipPost b

    err = throwM . ParseError

    extract (WordFramedSkipPre s) = fextract s
    extract (WordFramedWord s n) =
        if n == 0
        then fextract s
        else err "wordFramedBy: missing frame end"
    extract (WordFramedEsc _ _) =
        err "wordFramedBy: trailing escape"
    extract (WordFramedSkipPost b) = return b

data WordQuotedState s b a =
      WordQuotedSkipPre !s
    | WordUnquotedWord !s
    | WordQuotedWord !s !Int a
    | WordUnquotedEsc !s
    | WordQuotedEsc !s !Int a
    | WordQuotedSkipPost !b

{-# INLINE wordQuotedBy #-}
wordQuotedBy :: (MonadCatch m, Eq a) =>
       Bool         -- ^ keep the quotes in the output
    -> (a -> Bool)  -- ^ Escape
    -> (a -> Bool)  -- ^ left quote
    -> (a -> Bool)  -- ^ right quote
    -> (a -> a)     -- ^ get right quote from the left quote
    -> (a -> Bool)  -- ^ word seperator
    -> Fold m a b
    -> Parser m a b
wordQuotedBy keepQuotes isEsc isBegin isEnd toRight isSep
    (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial =  do
        res <- finitial
        return $
            case res of
                FL.Partial s -> IPartial (WordQuotedSkipPre s)
                FL.Done _ ->
                    error "wordQuotedBy: fold done without input"

    {-# INLINE process #-}
    process s a n q = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Continue 0 (WordQuotedWord s1 n q)
                FL.Done b -> Done 0 b

    {-# INLINE processUnquoted #-}
    processUnquoted s a = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Continue 0 (WordUnquotedWord s1)
                FL.Done b -> Done 0 b

    step (WordQuotedSkipPre s) a
        | isEsc a = return $ Continue 0 $ WordUnquotedEsc s
        | isSep a = return $ Partial 0 $ WordQuotedSkipPre s
        | isBegin a =
              if keepQuotes
              then process s a 1 a
              else return $ Continue 0 $ WordQuotedWord s 1 a
        | isEnd a =
            return $ Error "wordQuotedBy: missing frame start"
        | otherwise = processUnquoted s a
    step (WordUnquotedWord s) a
        | isEsc a = return $ Continue 0 $ WordUnquotedEsc s
        | isSep a = do
            b <- fextract s
            return $ Partial 0 $ WordQuotedSkipPost b
        | otherwise = do
               if isBegin a
               then if keepQuotes
                    then process s a 1 a
                    else return $ Continue 0 $ WordQuotedWord s 1 a
               else if isEnd a
                    then return $ Error "wordQuotedBy: missing frame start"
                    else processUnquoted s a
    step (WordQuotedWord s n q) a
        | isEsc a = return $ Continue 0 $ WordQuotedEsc s n q
        -- XXX Will this ever occur? Will n ever be 0?
        | n == 0 && isSep a = do
            b <- fextract s
            return $ Partial 0 $ WordQuotedSkipPost b
        | otherwise = do
                if a == toRight q
                then
                   if n == 1
                   then if keepQuotes
                        then processUnquoted s a
                        else return $ Continue 0 $ WordUnquotedWord s
                   else process s a (n - 1) q
                else if a == q
                     then process s a (n + 1) q
                     else process s a n q
    step (WordUnquotedEsc s) a = processUnquoted s a
    step (WordQuotedEsc s n q) a = process s a n q
    step (WordQuotedSkipPost b) a =
        return
            $ if not (isSep a)
              then Done 1 b
              else Partial 0 $ WordQuotedSkipPost b

    err = throwM . ParseError

    extract (WordQuotedSkipPre s) = fextract s
    extract (WordUnquotedWord s) = fextract s
    extract (WordQuotedWord s n _) =
        if n == 0
        then fextract s
        else err "wordQuotedBy: missing frame end"
    extract WordQuotedEsc {} =
        err "wordQuotedBy: trailing escape"
    extract (WordUnquotedEsc _) =
        err "wordQuotedBy: trailing escape"
    extract (WordQuotedSkipPost b) = return b

-- XXX use an Unfold instead of a list?
-- XXX custom combinators for matching list, array and stream?
-- XXX rename to listBy?
--
-- | See 'Streamly.Internal.Data.Parser.eqBy'.
--
-- /Pre-release/
--
{-# INLINE eqBy #-}
eqBy :: MonadThrow m => (a -> a -> Bool) -> [a] -> Parser m a ()
eqBy cmp str = Parser step initial extract

    where

    -- XXX Should return IDone in initial for [] case
    initial = return $ IPartial str

    step [] _ = return $ Done 0 ()
    step [x] a =
        return
            $ if x `cmp` a
              then Done 0 ()
              else Error "eqBy: failed, yet to match the last element"
    step (x:xs) a =
        return
            $ if x `cmp` a
              then Continue 0 xs
              else Error
                       $ "eqBy: failed, yet to match "
                       ++ show (length xs + 1) ++ " elements"

    extract xs =
        throwM
            $ ParseError
            $ "eqBy: end of input, yet to match "
            ++ show (length xs) ++ " elements"

-- XXX rename to streamBy?
-- | Like eqBy but uses a stream instead of a list
{-# INLINE matchBy #-}
matchBy :: MonadThrow m => (a -> a -> Bool) -> D.Stream m a -> Parser m a ()
matchBy cmp (D.Stream sstep state) = Parser step initial extract

    where

    initial = do
        r <- sstep defState state
        case r of
            D.Yield x s -> return $ IPartial (Just' x, s)
            D.Stop -> return $ IDone ()
            -- Need Skip/Continue in initial to loop right here
            D.Skip s -> return $ IPartial (Nothing', s)

    step (Just' x, st) a =
        if x `cmp` a
          then do
            r <- sstep defState st
            return
                $ case r of
                    D.Yield x1 s -> Continue 0 (Just' x1, s)
                    D.Stop -> Done 0 ()
                    D.Skip s -> Continue 1 (Nothing', s)
          else return $ Error "match: mismtach occurred"
    step (Nothing', st) a = do
        r <- sstep defState st
        return
            $ case r of
                D.Yield x s -> do
                    if x `cmp` a
                    then Continue 0 (Nothing', s)
                    else Error "match: mismatch occurred"
                D.Stop -> Done 1 ()
                D.Skip s -> Continue 1 (Nothing', s)

    extract _ = throwM $ ParseError "match: end of input"

-------------------------------------------------------------------------------
-- Transformations on input
-------------------------------------------------------------------------------

-- Initial needs a "Continue" constructor to implement scans on parsers. As a
-- parser can always return a Continue in initial when we feed the fold's
-- initial result to it. We can work this around for postscan by introducing an
-- initial state and calling "initial" only on the first input.
--
-- | Stateful scan on the input of a parser using a Fold.
--
-- /Unimplemented/
--
{-# INLINE postscan #-}
postscan :: -- Monad m =>
    Fold m a b -> Parser m b c -> Parser m a c
postscan = undefined

-- XXX More variants of this are possible based on how do we end the fold, when
-- the stream ends, when the fold ends, or when any ends.
{-# INLINE zipWithM #-}
zipWithM :: MonadThrow m =>
    (a -> b -> m c) -> D.Stream m a -> Fold m c x -> Parser m b x
zipWithM zf (D.Stream sstep state) (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial = do
        fres <- finitial
        case fres of
            FL.Partial fs -> do
                r <- sstep defState state
                case r of
                    D.Yield x s -> return $ IPartial (Just' x, s, fs)
                    D.Stop -> do
                        x <- fextract fs
                        return $ IDone x
                    -- Need Skip/Continue in initial to loop right here
                    D.Skip s -> return $ IPartial (Nothing', s, fs)
            FL.Done x -> return $ IDone x

    step (Just' a, st, fs) b = do
        c <- zf a b
        fres <- fstep fs c
        case fres of
            FL.Partial fs1 -> do
                r <- sstep defState st
                case r of
                    D.Yield x1 s -> return $ Continue 0 (Just' x1, s, fs1)
                    D.Stop -> do
                        x <- fextract fs1
                        return $ Done 0 x
                    D.Skip s -> return $ Continue 1 (Nothing', s, fs1)
            FL.Done x -> return $ Done 0 x
    step (Nothing', st, fs) b = do
        r <- sstep defState st
        case r of
                D.Yield a s -> do
                    c <- zf a b
                    fres <- fstep fs c
                    case fres of
                        FL.Partial fs1 ->
                            return $ Continue 0 (Nothing', s, fs1)
                        FL.Done x -> return $ Done 0 x
                D.Stop -> do
                    x <- fextract fs
                    return $ Done 1 x
                D.Skip s -> return $ Continue 1 (Nothing', s, fs)

    extract _ = throwM $ ParseError "zipWithM: end of input"

-- | Zip the input of a fold with a stream.
--
-- /Pre-release/
--
{-# INLINE zip #-}
zip :: MonadThrow m => D.Stream m a -> Fold m (a, b) x -> Parser m b x
zip = zipWithM (curry return)

-- | Pair each element of a fold input with its index, starting from index 0.
--
-- /Pre-release/
{-# INLINE indexed #-}
indexed :: forall m a b. MonadThrow m => Fold m (Int, a) b -> Parser m a b
indexed = zip (D.enumerateFromIntegral 0 :: D.Stream m Int)

-- | @makeIndexFilter indexer filter predicate@ generates a fold filtering
-- function using a fold indexing function that attaches an index to each input
-- element and a filtering function that filters using @(index, element) ->
-- Bool) as predicate.
--
-- For example:
--
-- @
-- filterWithIndex = makeIndexFilter indexed filter
-- filterWithAbsTime = makeIndexFilter timestamped filter
-- filterWithRelTime = makeIndexFilter timeIndexed filter
-- @
--
-- /Pre-release/
{-# INLINE makeIndexFilter #-}
makeIndexFilter ::
       (Fold m (s, a) b -> Parser m a b)
    -> (((s, a) -> Bool) -> Fold m (s, a) b -> Fold m (s, a) b)
    -> (((s, a) -> Bool) -> Fold m a b -> Parser m a b)
makeIndexFilter f comb g = f . comb g . FL.lmap snd

-- | @sampleFromthen offset stride@ samples the element at @offset@ index and
-- then every element at strides of @stride@.
--
-- /Pre-release/
{-# INLINE sampleFromthen #-}
sampleFromthen :: MonadThrow m => Int -> Int -> Fold m a b -> Parser m a b
sampleFromthen offset size =
    makeIndexFilter indexed FL.filter (\(i, _) -> (i + offset) `mod` size == 0)

-------------------------------------------------------------------------------
-- nested parsers
-------------------------------------------------------------------------------

-- | See 'Streamly.Internal.Data.Parser.takeP'.
--
-- /Internal/
{-# INLINE takeP #-}
takeP :: Monad m => Int -> Parser m a b -> Parser m a b
takeP lim (Parser pstep pinitial pextract) = Parser step initial extract

    where

    initial = do
        res <- pinitial
        case res of
            IPartial s ->
                if lim > 0
                then return $ IPartial $ Tuple' 0 s
                else IDone <$> pextract s
            IDone b -> return $ IDone b
            IError e -> return $ IError e

    step (Tuple' cnt r) a = do
        assert (cnt < lim) (return ())
        res <- pstep r a
        let cnt1 = cnt + 1
        case res of
            Partial 0 s -> do
                assert (cnt1 >= 0) (return ())
                if cnt1 < lim
                then return $ Partial 0 $ Tuple' cnt1 s
                else Done 0 <$> pextract s
            Continue 0 s -> do
                assert (cnt1 >= 0) (return ())
                if cnt1 < lim
                then return $ Continue 0 $ Tuple' cnt1 s
                -- XXX This should error out?
                -- If designed properly, this will probably error out.
                -- "pextract" should error out
                --
                -- By Harendra,
                --
                -- This is a tricky case, we have the following options:
                --   1. Done 0 with extract as you have written
                --   2. Done n, will require buffering elements
                --   3. Use a backtracking fold and not a parser, once we have
                --      backtracking in folds
                else Done 0 <$> pextract s
            Partial n s -> do
                let taken = cnt1 - n
                assert (taken >= 0) (return ())
                return $ Partial n $ Tuple' taken s
            Continue n s -> do
                let taken = cnt1 - n
                assert (taken >= 0) (return ())
                return $ Continue n $ Tuple' taken s
            Done n b -> return $ Done n b
            Error str -> return $ Error str

    extract (Tuple' _ r) = pextract r

-- | See 'Streamly.Internal.Data.Parser.lookahead'.
--
-- /Pre-release/
--
{-# INLINE lookAhead #-}
lookAhead :: MonadThrow m => Parser m a b -> Parser m a b
lookAhead (Parser step1 initial1 _) = Parser step initial extract

    where

    initial = do
        res <- initial1
        return $ case res of
            IPartial s -> IPartial (Tuple' 0 s)
            IDone b -> IDone b
            IError e -> IError e

    step (Tuple' cnt st) a = do
        r <- step1 st a
        let cnt1 = cnt + 1
        return
            $ case r of
                  Partial n s -> Continue n (Tuple' (cnt1 - n) s)
                  Continue n s -> Continue n (Tuple' (cnt1 - n) s)
                  Done _ b -> Done cnt1 b
                  Error err -> Error err

    -- XXX returning an error let's us backtrack.  To implement it in a way so
    -- that it terminates on eof without an error then we need a way to
    -- backtrack on eof, that will require extract to return 'Step' type.
    extract (Tuple' n _) =
        throwM
            $ ParseError
            $ "lookAhead: end of input after consuming "
            ++ show n ++ " elements"

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------

data DeintercalateState fs sp ss =
      DeintercalateL !fs !sp
    | DeintercalateR !fs !ss !Bool

-- | See 'Streamly.Internal.Data.Parser.deintercalate'.
--
-- /Internal/
--
{-# INLINE deintercalate #-}
deintercalate :: Monad m =>
       Fold m (Either x y) z
    -> Parser m a x
    -> Parser m a y
    -> Parser m a z
deintercalate
    (Fold fstep finitial fextract)
    (Parser stepL initialL extractL)
    (Parser stepR initialR extractR) = Parser step initial extract

    where

    errMsg p status =
        error $ "deintercalate: " ++ p ++ " parser cannot "
                ++ status ++ " without input"

    initial = do
        res <- finitial
        case res of
            FL.Partial fs -> do
                resL <- initialL
                case resL of
                    IPartial sL -> return $ IPartial $ DeintercalateL fs sL
                    IDone _ -> errMsg "left" "succeed"
                    IError _ -> errMsg "left" "fail"
            FL.Done c -> return $ IDone c

    step (DeintercalateL fs sL) a = do
        r <- stepL sL a
        case r of
            Partial n s -> return $ Partial n (DeintercalateL fs s)
            Continue n s -> return $ Continue n (DeintercalateL fs s)
            Done n b -> do
                fres <- fstep fs (Left b)
                case fres of
                    FL.Partial fs1 -> do
                        resR <- initialR
                        case resR of
                            IPartial sR ->
                                return
                                    $ Partial n (DeintercalateR fs1 sR False)
                            IDone _ -> errMsg "right" "succeed"
                            IError _ -> errMsg "right" "fail"
                    FL.Done c -> return $ Done n c
            Error err -> return $ Error err
    step (DeintercalateR fs sR consumed) a = do
        r <- stepR sR a
        case r of
            Partial n s -> return $ Partial n (DeintercalateR fs s True)
            Continue n s -> return $ Continue n (DeintercalateR fs s True)
            Done n b ->
                if consumed
                then do
                    fres <- fstep fs (Right b)
                    case fres of
                        FL.Partial fs1 -> do
                            resL <- initialL
                            case resL of
                                IPartial sL ->
                                    return $ Partial n $ DeintercalateL fs1 sL
                                IDone _ -> errMsg "left" "succeed"
                                IError _ -> errMsg "left" "fail"
                        FL.Done c -> return $ Done n c
                else error "deintercalate: infinite loop"
            Error err -> return $ Error err

    extract (DeintercalateL fs sL) = do
        r <- extractL sL
        res <- fstep fs (Left r)
        case res of
            FL.Partial fs1 -> fextract fs1
            FL.Done c -> return c
    extract (DeintercalateR fs sR _) = do
        r <- extractR sR
        res <- fstep fs (Right r)
        case res of
            FL.Partial fs1 -> fextract fs1
            FL.Done c -> return c

data SepByState fs sp ss =
      SepByInit !fs !sp
    | SepBySeparator !fs !ss !Bool

-- This is a special case of deintercalate and can be easily implemented in
-- terms of deintercalate.
{-# INLINE sepBy #-}
sepBy :: MonadCatch m =>
    Fold m b c -> Parser m a b -> Parser m a x -> Parser m a c
sepBy
    (Fold fstep finitial fextract)
    (Parser pstep pinitial pextract)
    (Parser sstep sinitial _) = Parser step initial extract

    where

    errMsg p status =
        error $ "sepBy: " ++ p ++ " parser cannot "
                ++ status ++ " without input"

    initial = do
        res <- finitial
        case res of
            FL.Partial fs -> do
                resP <- pinitial
                case resP of
                    IPartial sp -> return $ IPartial $ SepByInit fs sp
                    IDone _ -> errMsg "content" "succeed"
                    IError _ -> errMsg "content" "fail"
            FL.Done b -> return $ IDone b

    step (SepByInit fs sp) a = do
        r <- pstep sp a
        case r of
            Partial n s -> return $ Partial n (SepByInit fs s)
            Continue n s -> return $ Continue n (SepByInit fs s)
            Done n b -> do
                fres <- fstep fs b
                case fres of
                    FL.Partial fs1 -> do
                        resS <- sinitial
                        case resS of
                            IPartial ss ->
                                return $ Partial n (SepBySeparator fs1 ss False)
                            IDone _ -> errMsg "separator" "succeed"
                            IError _ -> errMsg "separator" "fail"
                    FL.Done c -> return $ Done n c
            Error err -> return $ Error err
    step (SepBySeparator fs ss consumed) a = do
        r <- sstep ss a
        case r of
            Partial n s -> return $ Partial n (SepBySeparator fs s True)
            Continue n s -> return $ Continue n (SepBySeparator fs s True)
            Done n _ ->
                if consumed
                then do
                    resP <- pinitial
                    case resP of
                        IPartial sp -> return $ Partial n $ SepByInit fs sp
                        IDone _ -> errMsg "content" "succeed"
                        IError _ -> errMsg "content" "fail"
                else error "sepBy: infinite loop"
            Error err -> return $ Error err

    extract (SepByInit fs sp) = do
        r <- pextract sp
        res <- fstep fs r
        case res of
            FL.Partial fs1 -> fextract fs1
            FL.Done c -> return c
    extract (SepBySeparator fs _ _) = fextract fs

-------------------------------------------------------------------------------
-- Sequential Collection
-------------------------------------------------------------------------------
--
-- | See 'Streamly.Internal.Data.Parser.sequence'.
{-# INLINE sequence #-}
sequence :: MonadThrow m =>
    Fold m b c -> D.Stream m (Parser m a b) -> Parser m a c
sequence (Fold fstep finitial fextract) (D.Stream sstep sstate) =
    Parser step initial extract

    where

    initial = do
        fres <- finitial
        case fres of
            FL.Partial fs -> return $ IPartial (Nothing', sstate, fs)
            FL.Done c -> return $ IDone c

    -- state does not contain any parser
    -- yield a new parser from the stream
    step (Nothing', ss, fs) _ = do
        sres <- sstep defState ss
        case sres of
            D.Yield p ss1 -> return $ Continue 1 (Just' p, ss1, fs)
            D.Stop -> do
                c <- fextract fs
                return $ Done 1 c
            D.Skip ss1 -> return $ Continue 1 (Nothing', ss1, fs)

    -- state holds a parser that may or may not have been
    -- initialized. pinit holds the initial parser state
    -- or modified parser state respectively
    step (Just' (Parser pstep pinit pextr), ss, fs) a = do
        ps <- pinit
        case ps of
            IPartial ps1 -> do
                pres <- pstep ps1 a
                case pres of
                    Partial n ps2 ->
                        let newP =
                              Just' $ Parser pstep (return $ IPartial ps2) pextr
                        in return $ Partial n (newP, ss, fs)
                    Continue n ps2 ->
                        let newP =
                              Just' $ Parser pstep (return $ IPartial ps2) pextr
                        in return $ Continue n (newP, ss, fs)
                    Done n b -> do
                        fres <- fstep fs b
                        case fres of
                            FL.Partial fs1 ->
                                return $ Partial n (Nothing', ss, fs1)
                            FL.Done c -> return $ Done n c
                    Error msg -> return $ Error msg
            IDone b -> do
                fres <- fstep fs b
                case fres of
                    FL.Partial fs1 ->
                        return $ Partial 1 (Nothing', ss, fs1)
                    FL.Done c -> return $ Done 1 c
            IError err -> return $ Error err

    extract (Nothing', _, fs) = fextract fs
    extract (Just' (Parser _ pinit pextr), _, fs) = do
        ps <- pinit
        case ps of
            IPartial ps1 ->  do
                b <- pextr ps1
                fres <- fstep fs b
                case fres of
                    FL.Partial fs1 -> fextract fs1
                    FL.Done c -> return c
            IDone b -> do
                fres <- fstep fs b
                case fres of
                    FL.Partial fs1 -> fextract fs1
                    FL.Done c -> return c
            IError err -> throwM $ ParseError err

-------------------------------------------------------------------------------
-- Alternative Collection
-------------------------------------------------------------------------------

-- | See 'Streamly.Internal.Data.Parser.choice'.
--
-- /Broken/
--
{-# INLINE choice #-}
choice :: (MonadCatch m, Foldable t) => t (Parser m a b) -> Parser m a b
choice = foldl1 shortest

-------------------------------------------------------------------------------
-- Sequential Repetition
-------------------------------------------------------------------------------

{-# INLINE manyP #-}
manyP :: -- MonadCatch m =>
    Parser m a b -> Parser m b c -> Parser m a c
manyP _p _f = undefined

-- | See 'Streamly.Internal.Data.Parser.many'.
--
-- /Pre-release/
--
{-# INLINE many #-}
many :: MonadCatch m => Parser m a b -> Fold m b c -> Parser m a c
many = splitMany
-- many = countBetween 0 maxBound

-- | See 'Streamly.Internal.Data.Parser.some'.
--
-- /Pre-release/
--
{-# INLINE some #-}
some :: MonadCatch m => Parser m a b -> Fold m b c -> Parser m a c
some = splitSome
-- some p f = manyP p (takeGE 1 f)
-- some = countBetween 1 maxBound

-- | See 'Streamly.Internal.Data.Parser.countBetween'.
--
-- /Unimplemented/
--
{-# INLINE countBetween #-}
countBetween :: -- MonadCatch m =>
    Int -> Int -> Parser m a b -> Fold m b c -> Parser m a c
countBetween _m _n _p = undefined
-- countBetween m n p f = manyP p (takeBetween m n f)

-- | See 'Streamly.Internal.Data.Parser.count'.
--
-- /Unimplemented/
--
{-# INLINE count #-}
count :: -- MonadCatch m =>
    Int -> Parser m a b -> Fold m b c -> Parser m a c
count n = countBetween n n
-- count n p f = manyP p (takeEQ n f)

data ManyTillState fs sr sl
    = ManyTillR Int fs sr
    | ManyTillL Int fs sl

-- | See 'Streamly.Internal.Data.Parser.manyTill'.
--
-- /Pre-release/
--
{-# INLINE manyTill #-}
manyTill :: MonadCatch m
    => Fold m b c -> Parser m a b -> Parser m a x -> Parser m a c
manyTill (Fold fstep finitial fextract)
         (Parser stepL initialL extractL)
         (Parser stepR initialR _) =
    Parser step initial extract

    where

    -- Caution: Mutual recursion

    scrutL fs p c d e = do
        resL <- initialL
        case resL of
            IPartial sl -> return $ c (ManyTillL 0 fs sl)
            IDone bl -> do
                fr <- fstep fs bl
                case fr of
                    FL.Partial fs1 -> scrutR fs1 p c d e
                    FL.Done fb -> return $ d fb
            IError err -> return $ e err

    scrutR fs p c d e = do
        resR <- initialR
        case resR of
            IPartial sr -> return $ p (ManyTillR 0 fs sr)
            IDone _ -> d <$> fextract fs
            IError _ -> scrutL fs p c d e

    initial = do
        res <- finitial
        case res of
            FL.Partial fs -> scrutR fs IPartial IPartial IDone IError
            FL.Done b -> return $ IDone b

    step (ManyTillR cnt fs st) a = do
        r <- stepR st a
        case r of
            Partial n s -> return $ Partial n (ManyTillR 0 fs s)
            Continue n s -> do
                assert (cnt + 1 - n >= 0) (return ())
                return $ Continue n (ManyTillR (cnt + 1 - n) fs s)
            Done n _ -> do
                b <- fextract fs
                return $ Done n b
            Error _ -> do
                resL <- initialL
                case resL of
                    IPartial sl ->
                        return $ Continue (cnt + 1) (ManyTillL 0 fs sl)
                    IDone bl -> do
                        fr <- fstep fs bl
                        let cnt1 = cnt + 1
                            p = Partial cnt
                            c = Continue cnt
                            d = Done cnt
                        case fr of
                            FL.Partial fs1 -> scrutR fs1 p c d Error
                            FL.Done fb -> return $ Done cnt1 fb
                    IError err -> return $ Error err
    -- XXX the cnt is being used only by the assert
    step (ManyTillL cnt fs st) a = do
        r <- stepL st a
        case r of
            Partial n s -> return $ Partial n (ManyTillL 0 fs s)
            Continue n s -> do
                assert (cnt + 1 - n >= 0) (return ())
                return $ Continue n (ManyTillL (cnt + 1 - n) fs s)
            Done n b -> do
                fs1 <- fstep fs b
                case fs1 of
                    FL.Partial s ->
                        scrutR s (Partial n) (Continue n) (Done n) Error
                    FL.Done b1 -> return $ Done n b1
            Error err -> return $ Error err

    extract (ManyTillL _ fs sR) = do
        res <- extractL sR >>= fstep fs
        case res of
            FL.Partial s -> fextract s
            FL.Done b -> return b
    extract (ManyTillR _ fs _) = fextract fs
