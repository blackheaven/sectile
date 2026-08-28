module Data.Sectile.Runners
  ( -- * Runners
    renderSegment,
    explainSegment,
  )
where

import qualified Data.ByteString.Builder as B
import qualified Data.Sectile.Tmux as Colour
import Data.Sectile.Types
import Control.Monad.State (evalState)
import qualified Data.HashMap.Strict as HashMap

-- | Render a segment to a 'B.Builder' using the given terminal capabilities.
--
-- This is the main function for producing terminal output from a segment.
--
-- Example:
--
-- > import Data.Sectile
-- > import qualified Data.ByteString.Builder as B
-- > import qualified Data.Sectile.Tmux as Colour
-- >
-- > main :: IO ()
-- > main = do
-- >   output <- renderSegment Colour.With8Colours (string "Hello")
-- >   B.hPutBuilder stdout output
renderSegment :: (Functor m) => Colour.TerminalCapabilities -> Segment m -> m B.Builder
renderSegment t s = Colour.renderChunksUtf8BSBuilder t . (.rendered) . (`evalState` Env Colour.noStyle HashMap.empty) <$> s.runSegment

-- | Render an explanation tree for a segment, useful for debugging.
--
-- Produces a tree-formatted explanation of how a segment was built,
-- including types, values, and nested structure.
--
-- Example:
--
-- > import Data.Sectile
-- > import qualified Data.ByteString.Builder as B
-- > import qualified Data.Sectile.Tmux as Colour
-- >
-- > debugSegment :: IO ()
-- > debugSegment = do
-- >   output <- explainSegment Colour.With8Colours (string "test")
-- >   B.hPutBuilder stdout output
explainSegment :: (Functor m) => Colour.TerminalCapabilities -> Segment m -> m B.Builder
explainSegment t s = withFormat . (`evalState` Env Colour.noStyle HashMap.empty) <$> s.runSegment
  where
    withFormat fmt =
      go 0 $ fmt.explain $ Colour.renderChunksUtf8BSBuilder t
    go level =
      \case
        DetailPlain x -> mconcat (replicate (2 * level) " ") <> "└──" <> x
        DetailNested x -> go (level + 1) x
        DetailList xs -> foldMap (\x -> go level x <> "\n") xs
