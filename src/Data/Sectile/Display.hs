module Data.Sectile.Display
  ( -- * Truncation
    takeStart,
    takeEnd,

    -- * Padding
    padStart,
    padEnd,

    -- * Fixed-size
    fixedSizeStart,
    fixedSizeEnd,

    -- * Regex rewriting
    regex,

    -- * Combinators
    progressBar,
    hideIf,
    marquee,
  )
where

import qualified Control.Lens as Lens
import qualified Control.Lens.Regex.Text as Regex
import qualified Data.Char as Char
import Data.Maybe (listToMaybe)
import qualified Data.Sectile.Tmux as Colour
import Data.Sectile.Types
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Time.Clock.POSIX as Time
import qualified Text.Regex.PCRE.Light as PCRE

-- | Keep only the first @n@ characters of a segment's rendered text.
--
-- Truncates chunks from the end to fit within the character limit.
--
-- Example:
--
-- > import Data.Sectile
-- > import Data.Sectile.Display
-- >
-- > short :: Segment IO -> Segment IO
-- > short = takeStart 10
-- > -- "Hello, world!" becomes "Hello, wor"
takeStart :: (Functor m) => Int -> Segment m -> Segment m
takeStart = transformChunks . chunksStart

-- | Keep only the last @n@ characters of a segment's rendered text.
--
-- Truncates chunks from the start to fit within the character limit.
--
-- Example:
--
-- > import Data.Sectile
-- > import Data.Sectile.Display
-- >
-- > tail5 :: Segment IO -> Segment IO
-- > tail5 = takeEnd 5
-- > -- "Hello, world!" becomes "orld!"
takeEnd :: (Functor m) => Int -> Segment m -> Segment m
takeEnd = transformChunks . chunksEnd

-- | Pad the start of a segment with spaces to reach at least @n@ characters.
--
-- If the segment is already @n@ or more characters, it is unchanged.
--
-- Example:
--
-- > import Data.Sectile
-- > import Data.Sectile.Display
-- >
-- > rightAligned :: Segment IO -> Segment IO
-- > rightAligned = padStart 20
-- > -- "hi" becomes "                  hi"
padStart :: (Functor m) => Int -> Segment m -> Segment m
padStart n = transformChunks (padChunksStart n)

-- | Pad the end of a segment with spaces to reach at least @n@ characters.
--
-- If the segment is already @n@ or more characters, it is unchanged.
--
-- Example:
--
-- > import Data.Sectile
-- > import Data.Sectile.Display
-- >
-- > leftAligned :: Segment IO -> Segment IO
-- > leftAligned = padEnd 20
-- > -- "hi" becomes "hi                  "
padEnd :: (Functor m) => Int -> Segment m -> Segment m
padEnd n = transformChunks (padChunksEnd n)

-- | Constrain a segment to exactly @n@ characters, padding at the start
-- or truncating from the end as needed.
--
-- Example:
--
-- > import Data.Sectile
-- > import Data.Sectile.Display
-- >
-- > fixed :: Segment IO -> Segment IO
-- > fixed = fixedSizeStart 10
-- > -- "Hi" becomes "        Hi"
-- > -- "Hello, world!" becomes "Hello, wor"
fixedSizeStart :: (Functor m) => Int -> Segment m -> Segment m
fixedSizeStart n = transformChunks (padChunksStart n . chunksStart n)

-- | Constrain a segment to exactly @n@ characters, padding at the end
-- or truncating from the start as needed.
--
-- Example:
--
-- > import Data.Sectile
-- > import Data.Sectile.Display
-- >
-- > fixed :: Segment IO -> Segment IO
-- > fixed = fixedSizeEnd 10
-- > -- "Hi" becomes "Hi        "
-- > -- "Hello, world!" becomes "orld!"
fixedSizeEnd :: (Functor m) => Int -> Segment m -> Segment m
fixedSizeEnd n = transformChunks (padChunksEnd n . chunksEnd n)

-- | Apply a PCRE regex replacement to a segment's rendered text.
--
-- Takes a compiled regex and a replacement function.
-- The replacement function receives the matched text and returns
-- the replacement.
--
-- Example:
--
-- > import Data.Sectile
-- > import Data.Sectile.Display
-- > import qualified Text.Regex.PCRE.Light as PCRE
-- >
-- > -- Remove all digits
-- > noDigits :: Segment IO -> Segment IO
-- > noDigits seg =
-- >   let pat = PCRE.compile "[0-9]+" []
-- >    in regex pat (const "") seg
regex :: (Functor m) => PCRE.Regex -> (T.Text -> T.Text) -> Segment m -> Segment m
regex pat replacement = transformChunks (regexReplace pat replacement)

-- | Convert a numerical segment output into an ASCII progress bar.
--
-- Parses the first sequence of digits from the rendered text and bounds it between 0-100.
-- Replaces the text with a bar of the specified width.
--
-- Example:
--
-- > import Data.Sectile
-- > import Data.Sectile.Display
-- >
-- > batBar :: Segment IO
-- > batBar = progressBar 10 (string "50%")
-- > -- Renders as "[====    ]"
progressBar :: (Functor m) => Int -> Segment m -> Segment m
progressBar width = transformChunks $ \cs ->
  let txt = mconcat $ map Colour.chunkText cs
      digits = T.filter Char.isDigit txt
      parsed = case T.decimal digits of
        Right (n, _) -> n
        Left _ -> 0 :: Int
      pct = max 0 $ min 100 parsed
      filled = (pct * width) `div` 100
      empty = width - filled
      bar = "[" <> T.replicate filled "=" <> T.replicate empty " " <> "]"
   in [Colour.Chunk bar Colour.noStyle]

-- | Conditionally hide a segment if its rendered text matches a predicate.
--
-- Example:
--
-- > import Data.Sectile
-- > import Data.Sectile.Display
-- > import qualified Data.Text as T
-- >
-- > hideEmpty :: Segment IO -> Segment IO
-- > hideEmpty = hideIf T.null
hideIf :: (Functor m) => (T.Text -> Bool) -> Segment m -> Segment m
hideIf p = transformChunks $ \cs ->
  if p (mconcat $ map Colour.chunkText cs) then [] else cs

-- | Scroll a long segment text horizontally over time.
--
-- Takes a fixed width and a number of seconds per tick.
--
-- Example:
--
-- > import Data.Sectile
-- > import Data.Sectile.Display
-- >
-- > scrolling :: Segment IO -> Segment IO
-- > scrolling = marquee 10 1
marquee :: Int -> Int -> Segment IO -> Segment IO
marquee width tickLenSeg (Segment s) = Segment $ do
  now <- Time.getPOSIXTime
  let ticks = floor now `div` tickLenSeg
  runSeg <- s
  pure $ do
    formatted <- runSeg
    let txt = mconcat $ map Colour.chunkText formatted.rendered
        len = T.length txt
        shifted =
          if len <= width
            then txt
            else
              let offset = ticks `mod` len
                  padded = txt <> " " <> txt
               in T.take width (T.drop offset padded)
    pure
      formatted
        { rendered = [Colour.Chunk shifted Colour.noStyle],
          explain = \renderer -> formatted.explain $ renderer . const [Colour.Chunk shifted Colour.noStyle]
        }

-- Internal helpers

-- | Apply a chunk transformation to a segment.
transformChunks :: (Functor m) => ([Colour.Chunk] -> [Colour.Chunk]) -> Segment m -> Segment m
transformChunks f (Segment s) = Segment $ fmap transform s
  where
    transform g = do
      formatted <- g
      pure
        formatted
          { rendered = f formatted.rendered,
            explain = \renderer -> formatted.explain $ renderer . f
          }

-- | Total width of a list of chunks.
chunksWidth :: [Colour.Chunk] -> Int
chunksWidth = sum . map Colour.chunkWidth

-- | Take the first n characters across chunks.
chunksStart :: Int -> [Colour.Chunk] -> [Colour.Chunk]
chunksStart _ [] = []
chunksStart n _ | n <= 0 = []
chunksStart n (c : cs) =
  let w = Colour.chunkWidth c
   in if w <= n
        then c : chunksStart (n - w) cs
        else [c {Colour.chunkText = T.take n c.chunkText}]

-- | Take the last n characters across chunks.
chunksEnd :: Int -> [Colour.Chunk] -> [Colour.Chunk]
chunksEnd n cs = map reverseChunk $ reverse $ chunksStart n $ reverse $ map reverseChunk cs
  where
    reverseChunk c = c {Colour.chunkText = T.reverse c.chunkText}

-- | Pad chunks at the start with spaces to reach width n.
padChunksStart :: Int -> [Colour.Chunk] -> [Colour.Chunk]
padChunksStart n cs =
  let w = chunksWidth cs
      padding = n - w
   in if padding > 0
        then mkPadChunk padding (firstStyle cs) : cs
        else cs

-- | Pad chunks at the end with spaces to reach width n.
padChunksEnd :: Int -> [Colour.Chunk] -> [Colour.Chunk]
padChunksEnd n cs =
  let w = chunksWidth cs
      padding = n - w
   in if padding > 0
        then cs <> [mkPadChunk padding (firstStyle cs)]
        else cs

-- | Create a padding chunk of n spaces.
mkPadChunk :: Int -> Colour.ChunkStyle -> Colour.Chunk
mkPadChunk n style =
  Colour.Chunk
    { Colour.chunkText = T.replicate n " ",
      Colour.chunkStyle = style
    }

firstStyle :: [Colour.Chunk] -> Colour.ChunkStyle
firstStyle = maybe Colour.noStyle Colour.chunkStyle . listToMaybe

-- | Apply regex replacement to chunk text.
regexReplace :: PCRE.Regex -> (T.Text -> T.Text) -> [Colour.Chunk] -> [Colour.Chunk]
regexReplace pat replacement = map replaceInChunk
  where
    replaceInChunk c =
      c {Colour.chunkText = Lens.over (Regex.regexing pat . Regex.match) replacement c.chunkText}
