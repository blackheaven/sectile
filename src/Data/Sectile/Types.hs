-- |
-- Module        : Data.Sectile.Types
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
module Data.Sectile.Types
  ( -- * Main types
    Segment (..),
    Formatted (..),
    Env (..),
    Detail (..),
    bindingsDetail,

    -- * Segment builder types
    Name (..),
    Unit (..),

    -- * Runner type
    SegmentsRunner,

    -- * Environment helpers
    currentStyle,
    updateStyle,
    currentBindings,
    appendBindings,
    updateBindings,
    scopeBindings,

    -- * Binding helpers
    unitBindings,
    percentBindings,
  )
where

import Control.Monad.State (State, gets, modify)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Sectile.Tmux as Colour
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text.Encoding
import Numeric (showFFloat)

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
  { runSegment :: m (State Env Formatted)
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
    explain :: (Colour.ChunkStyle -> Maybe B.Builder) -> ([Colour.Chunk] -> B.Builder) -> Detail B.Builder
  }

-- | Segment environment: carries the incoming 'Env.style' and the
-- accumulated 'Env.bindings' across segments.
data Env = Env
  { style :: Colour.ChunkStyle,
    bindings :: HashMap Text Aeson.Value
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
  deriving newtype (IsString, Semigroup, Monoid)

-- | A unit for a segment, used for binding helpers.
newtype Unit
  = Unit {unUnit :: Text}
  deriving newtype (IsString, Eq, Show)

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
  (Segment m -> m (State Env Formatted)) ->
  [Segment m] ->
  m [State Env Formatted]

-- | The 'Colour.ChunkStyle' currently carried by the environment.
currentStyle :: State Env Colour.ChunkStyle
currentStyle = gets style

-- | Apply a transformation to the environment style and return the result.
updateStyle :: (Colour.ChunkStyle -> Colour.ChunkStyle) -> State Env Colour.ChunkStyle
updateStyle f = do
  modify (\env -> env {style = f (style env)})
  gets style

-- | The bindings currently carried by the environment.
currentBindings :: State Env (HashMap Text Aeson.Value)
currentBindings = gets bindings

-- | Apply a transformation to the environment bindings and return the result.
updateBindings :: (HashMap Text Aeson.Value -> HashMap Text Aeson.Value) -> State Env (HashMap Text Aeson.Value)
updateBindings f = do
  modify (\env -> env {bindings = f (bindings env)})
  gets bindings

-- | Add bindings; keys in the new map take precedence over existing ones.
appendBindings :: HashMap Text Aeson.Value -> State Env (HashMap Text Aeson.Value)
appendBindings newBindings = updateBindings (HashMap.union newBindings)

-- | Run an action with bindings scoped under the given 'Name':
-- child bindings get prefixed with the name.
scopeBindings :: Name -> State Env a -> State Env a
scopeBindings (Name nameBuilder) action = do
  let prefix = Text.Encoding.decodeUtf8 (LBS.toStrict (B.toLazyByteString nameBuilder)) <> "."
      mapKeys f hm = HashMap.fromList $ first f <$> HashMap.toList hm
  oldBindings <- gets bindings
  modify (\env -> env {bindings = HashMap.empty})
  result <- action
  childBindings <- gets bindings
  let prefixedChildBindings = mapKeys (prefix <>) childBindings
  modify (\env -> env {bindings = HashMap.union prefixedChildBindings oldBindings})
  pure result

-- | Build bindings for a value with a base unit (e.g. @\"B\"@), scaled
-- to the largest fitting binary magnitude (KiB, MiB, ...).
unitBindings :: Unit -> Name -> Double -> HashMap Text Aeson.Value
unitBindings (Unit base) (Name nameBuilder) val =
  HashMap.fromList
    [ (nameT, Aeson.String (full <> prefix <> base)),
      (nameT <> ".raw", Aeson.Number (realToFrac val)),
      (nameT <> ".value.full", Aeson.String full),
      (nameT <> ".value.round", Aeson.String (T.pack $ showFFloat (Just 0) scaled "")),
      (nameT <> ".unit.full", Aeson.String (prefix <> base)),
      (nameT <> ".unit.base", Aeson.String base),
      (nameT <> ".unit.prefix", Aeson.String prefix)
    ]
  where
    nameT = Text.Encoding.decodeUtf8 $ LBS.toStrict $ B.toLazyByteString nameBuilder
    full = T.pack $ showFFloat (Just 1) scaled ""
    (prefix, scaled)
      | abs val >= 1024 ** 6 = ("Ei", val / (1024 ** 6))
      | abs val >= 1024 ** 5 = ("Pi", val / (1024 ** 5))
      | abs val >= 1024 ** 4 = ("Ti", val / (1024 ** 4))
      | abs val >= 1024 ** 3 = ("Gi", val / (1024 ** 3))
      | abs val >= 1024 ** 2 = ("Mi", val / (1024 ** 2))
      | abs val >= 1024 = ("Ki", val / 1024)
      | otherwise = ("", val)

-- | Build bindings for a value expressed as a percentage.
percentBindings :: Name -> Double -> HashMap Text Aeson.Value
percentBindings (Name nameBuilder) val =
  HashMap.fromList
    [ (nameT, Aeson.String (full <> "%")),
      (nameT <> ".raw", Aeson.Number (realToFrac val)),
      (nameT <> ".absolute", Aeson.String (T.pack $ showFFloat (Just 2) val "")),
      (nameT <> ".percent.full", Aeson.String full),
      (nameT <> ".percent.round", Aeson.String (T.pack $ showFFloat (Just 0) (val * 100) ""))
    ]
  where
    nameT = Text.Encoding.decodeUtf8 $ LBS.toStrict $ B.toLazyByteString nameBuilder
    full = T.pack $ showFFloat (Just 1) (val * 100) ""

-- | Render bindings as a @"Bindings:"@ detail block: one
-- @path = value@ line per leaf, flattening nested JSON objects into
-- dot-separated paths (e.g. @_inner.style.raw@).
bindingsDetail :: HashMap Text Aeson.Value -> [Detail B.Builder]
bindingsDetail bnds
  | HashMap.null bnds = []
  | otherwise =
      [ DetailPlain "Bindings:",
        DetailNested $
          DetailList
            [DetailPlain (Text.Encoding.encodeUtf8Builder path <> " = " <> valueBuilder v) | (path, v) <- leaves]
      ]
  where
    leaves = List.sortOn fst (concatMap (flattenValue "") (HashMap.toList bnds))
    flattenValue prefix (k, v) = descend (joinKey prefix k) v
    joinKey "" k = k
    joinKey prefix k = prefix <> "." <> k
    descend path (Aeson.Object obj)
      | KeyMap.null obj = [(path, Aeson.Object obj)]
      | otherwise = concatMap (\(k, v) -> descend (joinKey path (Key.toText k)) v) (KeyMap.toList obj)
    descend path (Aeson.Array arr)
      | Foldable.null arr = [(path, Aeson.Array arr)]
      | otherwise =
          concat [descend (joinKey path (T.pack (show i))) v | (i, v) <- zip [0 :: Int ..] (Foldable.toList arr)]
    descend path v = [(path, v)]
    valueBuilder (Aeson.String t) = Text.Encoding.encodeUtf8Builder t
    valueBuilder v = B.lazyByteString (Aeson.encode v)
