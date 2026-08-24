-- |
-- Module        : Data.Sectile.Segments
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
--
-- Visible segment constructors for building status line components.
module Data.Sectile.Segments
  ( -- * Basic segments
    string,

    -- * Composite segments
    row,

    -- * IO-based segments
    sh,
    time,
    volume,
    mpris,
    git,
    httpPoll,
  )
where

import qualified Control.Exception
import qualified Data.ByteString.Builder as B
import Data.Sectile.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Time as Time
import qualified System.Process as Process
import qualified Text.Colour.Chunk as Colour
import qualified Text.Colour.Chunk.Parsing as Colour

-- | Create a pure text segment.
--
-- The text may contain ANSI escape sequences which will be parsed
-- and rendered as styled chunks.
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > greeting :: Segment IO
-- > greeting = string "Hello, world!"
-- >
-- > styled :: Segment IO
-- > styled = string "\ESC[31mred text\ESC[0m"
string :: (Applicative m) => T.Text -> Segment m
string txt =
  Segment $
    pure $
      \style ->
        let (finalStyle, rendered) = Colour.parseAnsiChunks style txt
            explain f =
              DetailList
                [ DetailPlain "Type: string",
                  DetailPlain $ "Value: " <> T.encodeUtf8Builder txt,
                  DetailPlain $ "Rendered: " <> f rendered
                ]
         in Formatted {..}

-- | Combine multiple segments into a named row.
--
-- The segments are run using the provided 'SegmentsRunner', which
-- allows sequential or concurrent execution. Styles flow from one
-- segment to the next.
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > statusLine :: Segment IO
-- > statusLine =
-- >   row mapM "status" $
-- >     between (string " [") (string "] ") $
-- >       [ time "clock" "%H:%M",
-- >         string " | ",
-- >         sh "host" "hostname" Nothing
-- >       ]
row :: (Monad m) => SegmentsRunner m -> Name -> [Segment m] -> Segment m
row runSegments (Name name) ss =
  Segment $ do
    formats <- runSegments (.runSegment) ss
    pure $
      \style ->
        let (finalStyle, formatteds) =
              let safeLast = foldl' (const Just) Nothing
                  go (lastStyle, fs) f =
                    let fmt = f lastStyle
                     in (maybe lastStyle Colour.chunkStyle $ safeLast fmt.rendered, fmt : fs)
               in reverse <$> foldl' go (style, []) formats
            rendered = concatMap (.rendered) formatteds
            explain :: ([Colour.Chunk] -> B.Builder) -> Detail B.Builder
            explain f =
              DetailList $
                [ DetailPlain $ "Name: " <> name,
                  DetailPlain "Type: row",
                  DetailPlain $ "Rendered: " <> f rendered,
                  DetailPlain "Details:"
                ]
                  <> map (DetailNested . flip (.explain) f) formatteds
         in Formatted {..}

-- | Run a shell command and capture its stdout as a segment.
--
-- The command output may contain ANSI escape sequences. On failure,
-- displays @"Error on <name>"@.
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > hostname :: Segment IO
-- > hostname = sh "host" "hostname" Nothing
-- >
-- > withEnv :: Segment IO
-- > withEnv = sh "greeting" "echo $MSG" (Just [("MSG", "hello")])
sh :: Name -> String -> Maybe [(String, String)] -> Segment IO
sh (Name name) cmd env =
  Segment $ do
    let proc =
          (Process.shell cmd)
            { Process.env = env,
              Process.std_in = Process.CreatePipe,
              Process.std_out = Process.CreatePipe,
              Process.std_err = Process.CreatePipe
            }
    result <- tryReadProcess proc
    let stdout = case result of
          Right out -> T.pack out
          Left _ -> "Error on " <> TL.toStrict (TLE.decodeUtf8 (B.toLazyByteString name))
    pure $
      \style ->
        let (finalStyle, rendered) = Colour.parseAnsiChunks style stdout
            explain f =
              DetailList
                [ DetailPlain $ "Name: " <> name,
                  DetailPlain "Type: sh",
                  DetailPlain $ "Command: " <> T.encodeUtf8Builder (T.pack cmd),
                  DetailPlain $ "STDOUT: " <> T.encodeUtf8Builder stdout,
                  DetailPlain $ "Rendered: " <> f rendered
                ]
         in Formatted {..}

-- | Display the current time formatted with the given format string.
--
-- Uses 'Data.Time.formatTime' with 'Data.Time.defaultTimeLocale'.
-- On failure, displays @"Error on <name>"@.
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > clock :: Segment IO
-- > clock = time "clock" "%H:%M:%S"
-- >
-- > date :: Segment IO
-- > date = time "date" "%Y-%m-%d"
time :: Name -> String -> Segment IO
time (Name name) format =
  Segment $ do
    result <- tryIO $ Time.formatTime Time.defaultTimeLocale format <$> Time.getZonedTime
    let txt = case result of
          Right t -> T.pack t
          Left _ -> "Error on " <> TL.toStrict (TLE.decodeUtf8 (B.toLazyByteString name))
    pure $
      \style ->
        let (finalStyle, rendered) = Colour.parseAnsiChunks style txt
            explain f =
              DetailList
                [ DetailPlain $ "Name: " <> name,
                  DetailPlain "Type: time",
                  DetailPlain $ "Format: " <> T.encodeUtf8Builder (T.pack format),
                  DetailPlain $ "Formatted: " <> T.encodeUtf8Builder txt,
                  DetailPlain $ "Rendered: " <> f rendered
                ]
         in Formatted {..}

-- | Display the current volume using wpctl (Pipewire).
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > vol :: Segment IO
-- > vol = volume "vol"
volume :: Name -> Segment IO
volume name = sh name "wpctl get-volume @DEFAULT_AUDIO_SINK@" Nothing

-- | Display the currently playing song via playerctl (MPRIS).
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > song :: Segment IO
-- > song = mpris "song"
mpris :: Name -> Segment IO
mpris name = sh name "playerctl metadata --format '{{artist}} - {{title}}'" Nothing

-- | Display git branch and status for a specific repository.
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > gitStatus :: Segment IO
-- > gitStatus = git "git" "/path/to/repo"
git :: Name -> FilePath -> Segment IO
git name path = sh name ("git -C " <> path <> " status --porcelain -b | head -n 1") Nothing

-- | Display the result of polling an HTTP endpoint using curl.
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > weather :: Segment IO
-- > weather = httpPoll "weather" "wttr.in/?format=3"
httpPoll :: Name -> String -> Segment IO
httpPoll name url = sh name ("curl -s " <> url) Nothing

-- | Try to read a process, catching any IOException.
tryReadProcess :: Process.CreateProcess -> IO (Either IOError String)
tryReadProcess proc = tryIO (Process.readCreateProcess proc "")

-- | Try an IO action, catching IOExceptions.
tryIO :: IO a -> IO (Either IOError a)
tryIO act = (Right <$> act) `Control.Exception.catch` (pure . Left)
