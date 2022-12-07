-- |
-- Module      : Streamly.Data.Array.Generic.Mut
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Data.Array.Generic.Mut
(
    -- * Type
    -- $arrayNotes
      Array (..)

    -- * Constructing and Writing
    -- ** Construction
    -- *** Uninitialized Arrays
    , newArray

    -- *** From streams
    , writeNUnsafe
    , writeN

    -- * Random writes
    , putIndex
    , putIndexUnsafe

    , modifyIndexUnsafe
    , modifyIndex

    -- * Growing and Shrinking
    -- Arrays grow only at the end, though it is possible to grow on both sides
    -- and therefore have a cons as well as snoc. But that will require two
    -- bounds in the array representation.

    -- ** Reallocation
    , realloc

    -- ** Appending elements
    , snocWith
    , snoc
    , snocUnsafe

    -- * Eliminating and Reading
    -- ** Unfolds
    , reader
    , toList

    -- ** Random reads
    , getIndex
    , getIndexUnsafe

    -- ** Construct from arrays
    -- get chunks without copying
    , getSliceUnsafe
    , getSlice

    , putSliceUnsafe
    , clone
    )
where

import Streamly.Internal.Data.Array.Generic.Mut.Type
