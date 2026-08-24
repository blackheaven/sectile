-- |
-- Module        : Data.Sectile.Types
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
--
-- Core types for the composable status line builder.
module Data.Sectile.Types
  ( -- * Main types
    Segment (..),
    Formatted (..),
    Detail (..),

    -- * Segment builder types
    Name (..),

    -- * Runner type
    SegmentsRunner,
  )
where

import qualified Data.ByteString.Builder as B
import qualified Data.Sectile.Tmux as Colour
import Data.String (IsString)

-- | A composable segment of a status line.
--
-- A segment wraps an effectful computation that, given a 'Colour.ChunkStyle',
-- produces a 'Formatted' output. Segments can be combined using 'Data.Sectile.Row.row'
-- and styled using functions from "Data.Sectile.Style".
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > hello :: Segment IO
-- > hello = string "Hello, world!"
newtype Segment m = Segment
  { runSegment :: m (Colour.ChunkStyle -> Formatted)
  }

-- | The result of rendering a 'Segment'.
--
-- Contains the rendered chunks, the style that should carry over to the
-- next segment, and an explanation tree for debugging.
--
-- Example:
--
-- > import qualified Data.Sectile.Tmux as Colour
-- >
-- > -- A Formatted value carries rendered output and debug info
-- > inspectRendered :: Formatted -> [Colour.Chunk]
-- > inspectRendered fmt = fmt.rendered
data Formatted = Formatted
  { rendered :: [Colour.Chunk],
    finalStyle :: Colour.ChunkStyle,
    explain :: ([Colour.Chunk] -> B.Builder) -> Detail B.Builder
  }

-- | A tree structure for segment explanations, used by 'Data.Sectile.Runners.explainSegment'.
--
-- * 'DetailPlain' holds a single line of explanation.
-- * 'DetailNested' indents its child one level deeper.
-- * 'DetailList' groups multiple explanation entries.
--
-- Example:
--
-- > explanationTree :: Detail String
-- > explanationTree =
-- >   DetailList
-- >     [ DetailPlain "Type: string",
-- >       DetailNested (DetailPlain "nested detail")
-- >     ]
data Detail a
  = DetailPlain a
  | DetailNested (Detail a)
  | DetailList [Detail a]

-- | A name for a segment, used for identification in explanations.
--
-- Can be created using @OverloadedStrings@:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > myName :: Name
-- > myName = "my-segment"
newtype Name
  = Name {unName :: B.Builder}
  deriving newtype (IsString)

-- | A strategy for running multiple segments.
--
-- This type alias represents a function that runs a collection of segments,
-- allowing different execution strategies (sequential via 'mapM' or
-- concurrent via @Control.Concurrent.Async.mapConcurrently@).
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > -- Sequential runner
-- > sequentialRunner :: SegmentsRunner IO
-- > sequentialRunner = mapM
type SegmentsRunner m =
  (Segment m -> m (Colour.ChunkStyle -> Formatted)) ->
  [Segment m] ->
  m [Colour.ChunkStyle -> Formatted]
