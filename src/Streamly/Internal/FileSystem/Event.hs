-- |
-- Module      : Streamly.FileSystem.Event
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- File system event notification API portable across Linux, macOS and Windows
-- platforms.
--
-- For platform specific API please see the following modules:
--
-- * "Streamly.Internal.FileSystem.Event.Darwin"
-- * "Streamly.Internal.FileSystem.Event.Linux"
-- * "Streamly.Internal.FileSystem.Event.Windows"

module Streamly.Internal.FileSystem.Event
    (
    -- * Creating a Watch

      watch
    , watchRecursive

    -- * Handling Events
    , Event
    , getAbsPath

    -- ** Item CRUD events
    , isCreated
    , isDeleted
    , isMoved
    , isModified

    -- ** Exception Conditions
    , isEventsLost

    -- ** Debugging
    , showEvent
    )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)
import Streamly.Internal.Data.Array.Foreign (Array)
import Streamly.Prelude (SerialT)

#if defined(CABAL_OS_DARWIN)
import Streamly.Internal.FileSystem.Event.Darwin (Event)
import qualified Streamly.Internal.FileSystem.Event.Darwin as Event
#elif defined(CABAL_OS_LINUX)
import Streamly.Internal.FileSystem.Event.Linux (Event)
import qualified Streamly.Internal.FileSystem.Event.Linux as Event
#elif defined(CABAL_OS_WINDOWS)
import Streamly.Internal.FileSystem.Event.Windows (Event)
import qualified Streamly.Internal.FileSystem.Event.Windows as Event
#else
#error "FS Events not supported on this platform"
#endif

-- XXX Ensure that defaultConfig is the same for all platforms.
--
-- XXX Ensure that equivalent events are generated on all platforms when same
-- operations are done. Document any differences in behavior on different
-- platforms. Any events that are not common to all platforms should be
-- filtered out.
--
-- XXX Only need to set common event types for macOS/Linux/Windows
-- XXX write a test, that generates all types of events that can possibly
-- occur on each platform. Then create a common test that only checks
-- for the common subset of those.

-------------------------------------------------------------------------------
-- Creating a watch
-------------------------------------------------------------------------------

-- macOS and Windows seem to do a path level watch i.e. they watch the path
-- rather than the underlying inode, possibly they can map the inode back to a
-- path?.  On Linux we cannot keep watching a watch root if the inode it is
-- pointing to has moved. However, the inode continues to be watched in this
-- case instead of the path, which is not the case for macOS.
--
-- To make the behavior same for all platforms, for the Linux case we will have
-- to stop watching the inode if it moves from under the path
-- (setUnwatchMoved).  And on macOS we can stop watching  a path if the inode
-- under it changes i.e. just filter out events for that path.
--
-- Also, to make the behavior of Linux same as macOS we need to set
-- setFollowSymLinks on Linux.

-- XXX Verify all the cases mentioned below using test cases.

-- | Start monitoring a list of file system paths for file system events.  The
-- paths could be files or directories.  Monitoring starts from the current
-- time onwards. The paths are specified as UTF-8 encoded 'Array' of 'Word8'.
--
-- If a watch root is a symbolic link then the target of the link is watched.
-- Fails if the watched path does not exist. If the user does not have
-- permissions (specific permissions?) on the watch root then no events are
-- generated.  No events are generated if the watch root itself is renamed or
-- deleted.
--
-- Note: not yet implemented on macOS, use watchRecursive instead.
--
-- /Pre-release/
--
watch :: NonEmpty (Array Word8) -> SerialT IO Event
watch = Event.watch

-- | Like 'watch' except that if a watched path is a directory the whole
-- directory tree under it is watched recursively.
--
-- On Linux 'watchRecursive' may be more expensive than 'watch'.
--
-- /Pre-release/
--
watchRecursive :: NonEmpty (Array Word8) -> SerialT IO Event
watchRecursive = Event.watchRecursive

-------------------------------------------------------------------------------
-- Handling Events
-------------------------------------------------------------------------------

-- | Get the absolute path of the file system object for which the event is
-- generated. The path is a UTF-8 encoded array of bytes.
--
-- /Pre-release/
--
getAbsPath :: Event -> Array Word8
getAbsPath = Event.getAbsPath

-- | Determine whether the event indicates creation of an object within the
-- monitored path. This event is generated when any file system object is
-- created.
--
-- For hard links the behavior is different on different operating systems. On
-- macOS hard linking does not generate a create event, it generates an
-- 'isInodeAttrsChanged' event on the directory instead. On Linux and Windows
-- hard linking generates a create event.
--
-- /Pre-release/
--
isCreated :: Event -> Bool
isCreated = Event.isCreated

-- XXX To make the behavior consistent, can we block the event on watch root on
-- macOS?
--
-- | Determine whether the event indicates deletion of an object within the
-- monitored path. On Linux and Windows hard link deletion generates a delete
-- event.
--
-- On Linux and Windows, this event does not occur when the watch root itself
-- is deleted. On macOS it occurs on deleting the watch root when it is not a
-- symbolic link.
--
-- See also 'isRootDeleted' event for Linux.
--
-- /Pre-release/
--
isDeleted :: Event -> Bool
isDeleted = Event.isDeleted

-- | Determine whether the event indicates rename of an object within the
-- monitored path. This event is generated when an object is renamed within the
-- watched directory or if it is moved out of or in the watched directory.
-- Moving hard links is no different than other types of objects.
--
-- /Pre-release/
--
isMoved :: Event -> Bool
isMoved = Event.isMoved

-- XXX Make the Windows behavior consistent with other platforms by removing
-- the event from directories.
--
-- | Determine whether the event indicates modification of an object within the
-- monitored path. This event is generated on file modification on all
-- platforms.
--
-- On Linux and macOS this event is never generated for directories.  On
-- Windows (in recursive watch mode) this event is generated for directories as
-- well when an object is created in or deleted from the directory.
--
-- /Pre-release/
--
isModified :: Event -> Bool
isModified = Event.isModified

-- | An event that indicates that some events before this may have been lost,
-- therefore, we need to take some recovery action.
--
-- /Pre-release/
--
isEventsLost :: Event -> Bool
isEventsLost = Event.isEventsLost

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- | Convert an 'Event' record to a String representation. Note that the output
-- of this function may be different on different platforms because it may
-- contain platform specific details.
--
-- /Internal/
--
showEvent :: Event -> String
showEvent = Event.showEvent