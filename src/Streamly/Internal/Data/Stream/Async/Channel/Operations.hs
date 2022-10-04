#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

-- |
-- Module      : Streamly.Internal.Data.Stream.Async.Channel.Operations
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Async.Channel.Operations
    (
      toChannel
    , toChannelK
    , fromChannel
    , fromChannelK
    )
where

#include "inline.hs"

import Control.Exception (fromException)
import Control.Monad (when)
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef (newIORef, readIORef, mkWeakIORef, writeIORef)
import Data.Maybe (isNothing)
import Streamly.Internal.Control.Concurrent (MonadAsync, askRunInIO)
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import System.Mem (performMajorGC)

import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Streamly.Internal.Data.Stream.Async.Channel.Dispatcher
import Streamly.Internal.Data.Stream.Async.Channel.Type
import Streamly.Internal.Data.Stream.Channel.Types

import Prelude hiding (map, concat, concatMap)

#ifdef INSPECTION
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Typeable (Typeable)
import Test.Inspection (inspect, hasNoTypeClassesExcept)
#endif

------------------------------------------------------------------------------
-- Generating streams from a channel
------------------------------------------------------------------------------

-- $concurrentEval
--
-- Usually a channel is used to concurrently evaluate multiple actions in a
-- stream using many worker threads that push the results to the channel and a
-- single puller that pulls them from channel generating the evaluated stream.
--
-- @
--                  input stream
--                       |
--     <-----------------|<--------worker
--     |  exceptions     |
-- output stream <---Channel<------worker
--                       |
--                       |<--------worker
--
-- @
--
-- The puller itself schedules the worker threads based on demand.
-- Exceptions are propagated from the worker threads to the puller.

-------------------------------------------------------------------------------
-- Write a stream to a channel
-------------------------------------------------------------------------------

-- XXX Should be a Fold, singleton API could be called joinChannel, or the fold
-- can be called joinChannel.

-- | Write a stream to an 'SVar' in a non-blocking manner. The stream can then
-- be read back from the SVar using 'fromSVar'.
{-# INLINE toChannelK #-}
toChannelK :: (MonadIO m, MonadBaseControl IO m) =>
    Channel m a -> K.Stream m a -> m ()
toChannelK sv m = do
    runIn <- askRunInIO
    liftIO $ enqueue sv (runIn, m)

-- INLINE for fromStreamK/toStreamK fusion

-- | Send a stream to a given channel for concurrent evaluation.
{-# INLINE toChannel #-}
toChannel :: (MonadIO m, MonadBaseControl IO m) =>
    Channel m a -> Stream m a -> m ()
toChannel chan = toChannelK chan . Stream.toStreamK

{-
-- | Send a stream of streams to a concurrent channel for evaluation.
{-# INLINE joinChannel #-}
joinChannel :: Channel m a -> Fold m (Stream m a) ()
joinChannel = undefined
-}

-------------------------------------------------------------------------------
-- Read a stream from a channel
-------------------------------------------------------------------------------

-- | Pull a stream from an SVar.
{-# NOINLINE fromChannelRaw #-}
fromChannelRaw :: (MonadIO m, MonadThrow m) => Channel m a -> K.Stream m a
fromChannelRaw sv = K.MkStream $ \st yld sng stp -> do
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
            liftIO $ printSVar (dumpSVar sv) "SVar Done"
        stp

    {-# INLINE processEvents #-}
    processEvents [] = K.MkStream $ \st yld sng stp -> do
        done <- postProcess sv
        if done
        then allDone stp
        else K.foldStream st yld sng stp $ fromChannelRaw sv

    processEvents (ev : es) = K.MkStream $ \st yld sng stp -> do
        let rest = processEvents es
        case ev of
            ChildYield a -> yld a rest
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> K.foldStream st yld sng stp rest
                    Just ex ->
                        case fromException ex of
                            Just ThreadAbort ->
                                K.foldStream st yld sng stp rest
                            Nothing -> do
                                liftIO (cleanupSVar (workerThreads sv))
                                throwM ex

#ifdef INSPECTION
-- Use of GHC constraint tuple (GHC.Classes.(%,,%)) in fromStreamVar leads to
-- space leak because the tuple gets allocated in every recursive call and each
-- allocation holds on to the previous allocation. This test is to make sure
-- that we do not use the constraint tuple type class.
--
inspect $ hasNoTypeClassesExcept 'fromChannelRaw
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

-- XXX fromChannel Should not be called multiple times, we can add a
-- safeguard for that. Or we can replicate the stream so that we can distribute
-- it to multiple consumers. or should we use an explicit dupChannel for that?

{-# INLINE fromChannelK #-}
fromChannelK :: MonadAsync m => Channel m a -> K.Stream m a
fromChannelK sv =
    K.mkStream $ \st yld sng stp -> do
        ref <- liftIO $ newIORef ()
        _ <- liftIO $ mkWeakIORef ref hook

        startChannel sv
        -- We pass a copy of sv to fromStreamVar, so that we know that it has
        -- no other references, when that copy gets garbage collected "ref"
        -- will get garbage collected and our hook will be called.
        K.foldStreamShared st yld sng stp $
            fromChannelRaw sv{svarRef = Just ref}
    where

    hook = do
        when (svarInspectMode sv) $ do
            r <- liftIO $ readIORef (svarStopTime (svarStats sv))
            when (isNothing r) $
                printSVar (dumpSVar sv) "SVar Garbage Collected"
        cleanupSVar (workerThreads sv)
        -- If there are any SVars referenced by this SVar a GC will prompt
        -- them to be cleaned up quickly.
        when (svarInspectMode sv) performMajorGC

-- | Generate a stream of results from concurrent evaluations from a channel.
-- Evaluation of the channel does not start until this API is called. This API
-- must not be called more than once on a channel. It kicks off evaluation of
-- the channel by dispatching concurrent workers and ensures that as long there
-- is work queued on the channel workers are dispatched proportional to the
-- demand by the consumer.
--
{-# INLINE fromChannel #-}
fromChannel :: MonadAsync m => Channel m a -> Stream m a
fromChannel = Stream.fromStreamK . fromChannelK

data FromSVarState t m a =
      FromSVarInit
    | FromSVarRead (Channel m a)
    | FromSVarLoop (Channel m a) [ChildEvent a]
    | FromSVarDone (Channel m a)

-- | Like 'fromSVar' but generates a StreamD style stream instead of CPS.
--
{-# INLINE_NORMAL _fromChannelD #-}
_fromChannelD :: (MonadIO m, MonadThrow m) => Channel m a -> D.Stream m a
_fromChannelD svar = D.Stream step FromSVarInit
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
                    printSVar (dumpSVar svar) "SVar Garbage Collected"
            cleanupSVar (workerThreads svar)
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
                    Nothing -> return $ D.Skip (FromSVarLoop sv es)
                    Just ex ->
                        case fromException ex of
                            Just ThreadAbort ->
                                return $ D.Skip (FromSVarLoop sv es)
                            Nothing -> do
                                liftIO (cleanupSVar (workerThreads sv))
                                throwM ex

    step _ (FromSVarDone sv) = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar (dumpSVar sv) "SVar Done"
        return D.Stop
