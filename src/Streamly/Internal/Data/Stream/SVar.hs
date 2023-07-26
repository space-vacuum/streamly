{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts          #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

-- |
-- Module      : Streamly.Internal.Data.Stream.SVar
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.Stream.SVar
    ( fromSVar
    , fromStreamVar
    , fromProducer
    , fromConsumer
    , toSVar
    , pushToFold
    )
where

import Control.Exception (fromException)
import Control.Monad (when, void)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (newIORef, readIORef, mkWeakIORef, writeIORef)
import Data.Maybe (isNothing)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import System.Mem (performMajorGC)

import Streamly.Internal.Data.SVar
import Streamly.Internal.Data.Stream.StreamK hiding (reverse)

#if __GLASGOW_HASKELL__ < 810
#ifdef INSPECTION
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Typeable (Typeable)
import Test.Inspection (inspect, hasNoTypeClassesExcept)
#endif
#endif

-- | Pull a stream from an SVar.
{-# NOINLINE fromStreamVar #-}
fromStreamVar :: MonadAsync m => SVar Stream m a -> Stream m a
fromStreamVar sv = MkStream $ \st yld sng stp -> do
    list <- readOutputQ sv
    -- Reversing the output is important to guarantee that we process the
    -- outputs in the same order as they were generated by the constituent
    -- streams.
    foldStream st yld sng stp $ processEvents $ reverse list

    where

    allDone stp = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar sv "SVar Done"
        stp

    {-# INLINE processEvents #-}
    processEvents [] = MkStream $ \st yld sng stp -> do
        done <- postProcess sv
        if done
        then allDone stp
        else foldStream st yld sng stp $ fromStreamVar sv

    processEvents (ev : es) = MkStream $ \st yld sng stp -> do
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
                        else foldStream st yld sng stp rest
                    Just ex ->
                        case fromException ex of
                            Just ThreadAbort ->
                                foldStream st yld sng stp rest
                            Nothing -> liftIO (cleanupSVar sv) >> throwM ex
    shouldStop tid =
        case svarStopStyle sv of
            StopNone -> return False
            StopAny -> return True
            StopBy -> do
                sid <- liftIO $ readIORef (svarStopBy sv)
                return $ if tid == sid then True else False

#if __GLASGOW_HASKELL__ < 810
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

{-# INLINE fromSVar #-}
fromSVar :: (MonadAsync m, IsStream t) => SVar Stream m a -> t m a
fromSVar sv =
    mkStream $ \st yld sng stp -> do
        ref <- liftIO $ newIORef ()
        _ <- liftIO $ mkWeakIORef ref hook
        -- We pass a copy of sv to fromStreamVar, so that we know that it has
        -- no other references, when that copy gets garbage collected "ref"
        -- will get garbage collected and our hook will be called.
        foldStreamShared st yld sng stp $
            fromStream $ fromStreamVar sv{svarRef = Just ref}
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

-- | Write a stream to an 'SVar' in a non-blocking manner. The stream can then
-- be read back from the SVar using 'fromSVar'.
toSVar :: (IsStream t, MonadAsync m) => SVar Stream m a -> t m a -> m ()
toSVar sv m = toStreamVar sv (toStream m)

-------------------------------------------------------------------------------
-- Process events received by a fold consumer from a stream producer
-------------------------------------------------------------------------------

-- | Pull a stream from an SVar.
{-# NOINLINE fromProducer #-}
fromProducer :: MonadAsync m => SVar Stream m a -> Stream m a
fromProducer sv = mkStream $ \st yld sng stp -> do
    list <- readOutputQ sv
    -- Reversing the output is important to guarantee that we process the
    -- outputs in the same order as they were generated by the constituent
    -- streams.
    foldStream st yld sng stp $ processEvents $ reverse list

    where

    allDone stp = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar sv "SVar Done"
        sendStopToProducer sv
        stp

    {-# INLINE processEvents #-}
    processEvents [] = mkStream $ \st yld sng stp -> do
        foldStream st yld sng stp $ fromProducer sv

    processEvents (ev : es) = mkStream $ \_ yld _ stp -> do
        let rest = processEvents es
        case ev of
            ChildYield a -> yld a rest
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> allDone stp
                    Just _ -> error "Bug: fromProducer: received exception"

-------------------------------------------------------------------------------
-- Process events received by the producer thread from the consumer side
-------------------------------------------------------------------------------

-- XXX currently only one event is sent by a fold consumer to the stream
-- producer. But we can potentially have multiple events e.g. the fold step can
-- generate exception more than once and the producer can ignore those
-- exceptions or handle them and still keep driving the fold.
--
{-# NOINLINE fromConsumer #-}
fromConsumer :: MonadAsync m => SVar Stream m a -> m Bool
fromConsumer sv = do
    (list, _) <- liftIO $ readOutputQBasic (outputQueueFromConsumer sv)
    -- Reversing the output is important to guarantee that we process the
    -- outputs in the same order as they were generated by the constituent
    -- streams.
    processEvents $ reverse list

    where

    {-# INLINE processEvents #-}
    processEvents [] = return False
    processEvents (ev : _) = do
        case ev of
            ChildStop _ e -> do
                case e of
                    Nothing -> return True
                    Just ex -> throwM ex
            ChildYield _ -> error "Bug: fromConsumer: invalid ChildYield event"

-- push values to a fold worker via an SVar. Returns whether the fold is done.
{-# INLINE pushToFold #-}
pushToFold :: MonadAsync m => SVar Stream m a -> a -> m Bool
pushToFold sv a = do
    -- Check for exceptions before decrement so that we do not
    -- block forever if the child already exited with an exception.
    --
    -- We avoid a race between the consumer fold sending an event and we
    -- blocking on decrementBufferLimit by waking up the producer thread in
    -- sendToProducer before any event is sent by the fold to the producer
    -- stream.
    let qref = outputQueueFromConsumer sv
    done <- do
        (_, n) <- liftIO $ readIORef qref
        if (n > 0)
        then fromConsumer sv
        else return False
    if done
    then return True
    else liftIO $ do
        decrementBufferLimit sv
        void $ send sv (ChildYield a)
        return False