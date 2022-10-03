#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

-- |
-- Module      : Streamly.Internal.Data.Stream.SVar.Generate
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.Stream.SVar.Generate
    (
    -- * Write to SVar
      toSVar

    -- * Read from SVar
    -- $concurrentEval
    , fromSVar
    , fromSVarD
    -- , fromStreamVar
    )
where

#include "inline.hs"

import Control.Exception (fromException)
import Control.Monad (when)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (newIORef, readIORef, mkWeakIORef, writeIORef)
import Data.Maybe (isNothing)
import Streamly.Internal.Control.Concurrent (MonadAsync, askRunInIO)
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import System.Mem (performMajorGC)

import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
    (Stream(..), Step(..))
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
    (Stream(..), foldStreamShared, mkStream, foldStream)
import qualified Streamly.Internal.Data.Stream.Type as Stream (fromStreamK)

import Streamly.Internal.Data.SVar

#if MIN_VERSION_base(4,13,0)
#ifdef INSPECTION
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Typeable (Typeable)
import Test.Inspection (inspect, hasNoTypeClassesExcept)
#endif
#endif

------------------------------------------------------------------------------
-- Generating streams from SVar
------------------------------------------------------------------------------

-- $concurrentEval
--
-- Usually the SVar is used to concurrently evaluate multiple actions in a
-- stream using many worker threads that push the results to the SVar and a
-- single puller that pulls them from SVar generating the evaluated stream.
--
-- @
--                  input stream
--                       |
--     <-----------------|<--------worker
--     |  exceptions     |
-- output stream <------SVar<------worker
--                       |
--                       |<--------worker
--
-- @
--
-- The puller itself schedules the worker threads based on demand.
-- Exceptions are propagated from the worker threads to the puller.

-------------------------------------------------------------------------------
-- Write a stream to an SVar
-------------------------------------------------------------------------------

-- XXX this errors out for Parallel/Ahead SVars
-- | Write a stream to an 'SVar' in a non-blocking manner. The stream can then
-- be read back from the SVar using 'fromSVar'.
toSVar :: MonadAsync m => SVar Stream m a -> Stream m a -> m ()
toSVar sv m = do
    runIn <- askRunInIO
    liftIO $ enqueue sv (runIn, m)
    done <- allThreadsDone sv
    -- XXX This is safe only when called from the consumer thread or when no
    -- consumer is present.  There may be a race if we are not running in the
    -- consumer thread.
    -- XXX do this only if the work queue is not empty. The work may have been
    -- carried out by existing workers.
    when done $
        case yieldRateInfo sv of
            Nothing -> pushWorker 0 sv
            Just _  -> pushWorker 1 sv

-------------------------------------------------------------------------------
-- Read a stream from an SVar
-------------------------------------------------------------------------------

-- | Pull a stream from an SVar.
{-# NOINLINE fromStreamVar #-}
fromStreamVar :: MonadAsync m => SVar K.Stream m a -> K.Stream m a
fromStreamVar sv = K.MkStream $ \st yld sng stp -> do
    list <- readOutputQ sv
    -- Reversing the output is important to guarantee that we process the
    -- outputs in the same order as they were generated by the constituent
    -- streams.
    K.foldStream st yld sng stp $ processEvents $ reverse list

    where

    allDone stp = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar sv "SVar Done"
        stp

    {-# INLINE processEvents #-}
    processEvents [] = K.MkStream $ \st yld sng stp -> do
        done <- postProcess sv
        if done
        then allDone stp
        else K.foldStream st yld sng stp $ fromStreamVar sv

    processEvents (ev : es) = K.MkStream $ \st yld sng stp -> do
        let rest = processEvents es
        case ev of
            ChildYield a -> yld a rest
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> do
                        stop <- shouldStop tid
                        if stop
                        then liftIO (cleanupSVar sv) >> allDone stp
                        else K.foldStream st yld sng stp rest
                    Just ex ->
                        case fromException ex of
                            Just ThreadAbort ->
                                K.foldStream st yld sng stp rest
                            Nothing -> liftIO (cleanupSVar sv) >> throwM ex
    shouldStop tid =
        case svarStopStyle sv of
            StopNone -> return False
            StopAny -> return True
            StopBy -> do
                sid <- liftIO $ readIORef (svarStopBy sv)
                return $ tid == sid

#if MIN_VERSION_base(4,13,0)
#ifdef INSPECTION
-- Use of GHC constraint tuple (GHC.Classes.(%,,%)) in fromStreamVar leads to
-- space leak because the tuple gets allocated in every recursive call and each
-- allocation holds on to the previous allocation. This test is to make sure
-- that we do not use the constraint tuple type class.
--
inspect $ hasNoTypeClassesExcept 'fromStreamVar
    [ ''Monad
    , ''Applicative
    , ''MonadThrow
    , ''Exception
    , ''MonadIO
    , ''MonadBaseControl
    , ''Typeable
    , ''Functor
    ]
#endif
#endif

-- | Generate a stream from an SVar.  An unevaluated stream can be pushed to an
-- SVar using 'toSVar'.  As we pull a stream from the SVar the input stream
-- gets evaluated concurrently. The evaluation depends on the SVar style and
-- the configuration parameters e.g. using the maxBuffer/maxThreads
-- combinators.
--
{-# INLINE fromSVar #-}
fromSVar :: MonadAsync m => SVar K.Stream m a -> Stream m a
fromSVar sv =
    Stream.fromStreamK $ K.mkStream $ \st yld sng stp -> do
        ref <- liftIO $ newIORef ()
        _ <- liftIO $ mkWeakIORef ref hook
        -- We pass a copy of sv to fromStreamVar, so that we know that it has
        -- no other references, when that copy gets garbage collected "ref"
        -- will get garbage collected and our hook will be called.
        K.foldStreamShared st yld sng stp $
            fromStreamVar sv{svarRef = Just ref}
    where

    hook = do
        when (svarInspectMode sv) $ do
            r <- liftIO $ readIORef (svarStopTime (svarStats sv))
            when (isNothing r) $
                printSVar sv "SVar Garbage Collected"
        cleanupSVar sv
        -- If there are any SVars referenced by this SVar a GC will prompt
        -- them to be cleaned up quickly.
        when (svarInspectMode sv) performMajorGC

data FromSVarState t m a =
      FromSVarInit
    | FromSVarRead (SVar t m a)
    | FromSVarLoop (SVar t m a) [ChildEvent a]
    | FromSVarDone (SVar t m a)

-- | Like 'fromSVar' but generates a StreamD style stream instead of CPS.
--
{-# INLINE_NORMAL fromSVarD #-}
fromSVarD :: (MonadAsync m) => SVar t m a -> D.Stream m a
fromSVarD svar = D.Stream step FromSVarInit
    where

    {-# INLINE_LATE step #-}
    step _ FromSVarInit = do
        ref <- liftIO $ newIORef ()
        _ <- liftIO $ mkWeakIORef ref hook
        -- when this copy of svar gets garbage collected "ref" will get
        -- garbage collected and our GC hook will be called.
        let sv = svar{svarRef = Just ref}
        return $ D.Skip (FromSVarRead sv)

        where

        {-# NOINLINE hook #-}
        hook = do
            when (svarInspectMode svar) $ do
                r <- liftIO $ readIORef (svarStopTime (svarStats svar))
                when (isNothing r) $
                    printSVar svar "SVar Garbage Collected"
            cleanupSVar svar
            -- If there are any SVars referenced by this SVar a GC will prompt
            -- them to be cleaned up quickly.
            when (svarInspectMode svar) performMajorGC

    step _ (FromSVarRead sv) = do
        list <- readOutputQ sv
        -- Reversing the output is important to guarantee that we process the
        -- outputs in the same order as they were generated by the constituent
        -- streams.
        return $ D.Skip $ FromSVarLoop sv (Prelude.reverse list)

    step _ (FromSVarLoop sv []) = do
        done <- postProcess sv
        return $ D.Skip $ if done
                      then FromSVarDone sv
                      else FromSVarRead sv

    step _ (FromSVarLoop sv (ev : es)) = do
        case ev of
            ChildYield a -> return $ D.Yield a (FromSVarLoop sv es)
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> do
                        stop <- shouldStop tid
                        if stop
                        then do
                            liftIO (cleanupSVar sv)
                            return $ D.Skip (FromSVarDone sv)
                        else return $ D.Skip (FromSVarLoop sv es)
                    Just ex ->
                        case fromException ex of
                            Just ThreadAbort ->
                                return $ D.Skip (FromSVarLoop sv es)
                            Nothing -> liftIO (cleanupSVar sv) >> throwM ex
        where

        shouldStop tid =
            case svarStopStyle sv of
                StopNone -> return False
                StopAny -> return True
                StopBy -> do
                    sid <- liftIO $ readIORef (svarStopBy sv)
                    return $ tid == sid

    step _ (FromSVarDone sv) = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar sv "SVar Done"
        return D.Stop
