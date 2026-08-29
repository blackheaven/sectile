{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Sectile.Segments
  ( -- * Core builders
    string,
    row,
    sh,
    time,
    volume,
    mpris,
    git,
    httpPoll,
    reformat,
  )
where

import qualified Control.Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sectile.Tmux as Colour
import Data.Sectile.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as POSIX
import qualified System.Process as Process
import qualified Text.EDE as EDE

-- | Create a pure text segment.
string :: (Applicative m) => T.Text -> Segment m
string txt =
  Segment $
    pure $ do
      currentSt <- currentStyle
      bnds <- currentBindings
      let (finalStyle, rendered) = Colour.parseAnsiChunks currentSt txt
          explain f =
            DetailList $
              [ DetailPlain "Type: string",
                DetailPlain $ "Value: " <> T.encodeUtf8Builder txt,
                DetailPlain $ "Rendered: " <> f rendered
              ]
                <> (if HashMap.null bnds then [] else [DetailPlain "Bindings:", DetailNested $ DetailList [DetailPlain (T.encodeUtf8Builder k <> " = " <> B.lazyByteString (Aeson.encode v)) | (k, v) <- List.sortOn fst (HashMap.toList bnds)]])
      _ <- updateStyle (const finalStyle)
      pure Formatted {..}

-- | Combine multiple segments into a named row.
row :: (Monad m) => SegmentsRunner m -> Name -> [Segment m] -> Segment m
row runSegments name@(Name nameBuilder) ss =
  Segment $ do
    states <- runSegments (.runSegment) ss
    pure $ scopeBindings name $ do
      formatteds <- sequence states
      let rendered = concatMap (.rendered) formatteds
          explain :: ([Colour.Chunk] -> B.Builder) -> Detail B.Builder
          explain f =
            DetailList $
              [ DetailPlain $ "Name: " <> nameBuilder,
                DetailPlain "Type: row",
                DetailPlain $ "Rendered: " <> f rendered,
                DetailPlain "Details:"
              ]
                <> map (DetailNested . flip (.explain) f) formatteds
      pure Formatted {..}

-- | Run a shell command and capture its stdout as a segment.
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
    pure $ do
      currentSt <- currentStyle
      bnds <- currentBindings
      let (finalStyle, rendered) = Colour.parseAnsiChunks currentSt stdout
          explain f =
            DetailList $
              [ DetailPlain $ "Name: " <> name,
                DetailPlain "Type: sh",
                DetailPlain $ "Command: " <> T.encodeUtf8Builder (T.pack cmd),
                DetailPlain $ "STDOUT: " <> T.encodeUtf8Builder stdout,
                DetailPlain $ "Rendered: " <> f rendered
              ]
                <> (if HashMap.null bnds then [] else [DetailPlain "Bindings:", DetailNested $ DetailList [DetailPlain (T.encodeUtf8Builder k <> " = " <> B.lazyByteString (Aeson.encode v)) | (k, v) <- List.sortOn fst (HashMap.toList bnds)]])
      _ <- updateStyle (const finalStyle)
      pure Formatted {..}

-- | Display the current time formatted with the given format string.
time :: Name -> String -> Segment IO
time (Name name) format =
  Segment $ do
    result <- tryIO $ Time.formatTime Time.defaultTimeLocale format <$> Time.getZonedTime
    posix <- POSIX.getPOSIXTime
    let txt = case result of
          Right t -> T.pack t
          Left _ -> "Error on " <> TL.toStrict (TLE.decodeUtf8 (B.toLazyByteString name))
    let nameT = T.decodeUtf8 (LBS.toStrict (B.toLazyByteString name))
    let generatedBnds = HashMap.singleton (nameT <> ".raw") (Aeson.Number (realToFrac posix))
    pure $ do
      currentSt <- currentStyle
      _ <- appendBindings generatedBnds
      bnds <- currentBindings
      let (finalStyle, rendered) = Colour.parseAnsiChunks currentSt txt
          explain f =
            DetailList $
              [ DetailPlain $ "Name: " <> name,
                DetailPlain "Type: time",
                DetailPlain $ "Format: " <> T.encodeUtf8Builder (T.pack format),
                DetailPlain $ "Formatted: " <> T.encodeUtf8Builder txt,
                DetailPlain $ "Rendered: " <> f rendered
              ]
                <> (if HashMap.null bnds then [] else [DetailPlain "Bindings:", DetailNested $ DetailList [DetailPlain (T.encodeUtf8Builder k <> " = " <> B.lazyByteString (Aeson.encode v)) | (k, v) <- List.sortOn fst (HashMap.toList bnds)]])
      _ <- updateStyle (const finalStyle)
      pure Formatted {..}

-- | Display the current volume using wpctl (Pipewire).
volume :: Name -> Segment IO
volume name = sh name "wpctl get-volume @DEFAULT_AUDIO_SINK@" Nothing

-- | Display the currently playing song via playerctl (MPRIS).
mpris :: Name -> Segment IO
mpris name = sh name "playerctl metadata --format '{{artist}} - {{title}}'" Nothing

-- | Display git branch and status for a specific repository.
git :: Name -> FilePath -> Segment IO
git name path = sh name ("git -C " <> path <> " status --porcelain -b | head -n 1") Nothing

-- | Display the result of polling an HTTP endpoint using curl.
httpPoll :: Name -> String -> Segment IO
httpPoll name url = sh name ("curl -s " <> url) Nothing

-- | Try to read a process, catching any IOException.
tryReadProcess :: Process.CreateProcess -> IO (Either IOError String)
tryReadProcess proc = tryIO (Process.readCreateProcess proc "")

-- | Try an IO action, catching IOExceptions.
tryIO :: IO a -> IO (Either IOError a)
tryIO act = (Right <$> act) `Control.Exception.catch` (pure . Left)

-- | Reformat a segment's output using an EDE template.
reformat :: (Functor m) => T.Text -> Segment m -> Segment m
reformat format (Segment s) = Segment $ fmap transform s
  where
    transform action = do
      oldSt <- currentStyle
      formatted <- action
      bnds <- currentBindings
      let rawText = mconcat $ map Colour.chunkText formatted.rendered
          styleText = T.decodeUtf8 $ LBS.toStrict $ B.toLazyByteString $ Colour.renderChunksUtf8BSBuilder Colour.With24BitColours formatted.rendered
          incomingStyleText = T.replace "#[default]" "" $ T.decodeUtf8 $ LBS.toStrict $ B.toLazyByteString $ Colour.renderChunksUtf8BSBuilder Colour.With24BitColours [Colour.Chunk "" oldSt]

          envObj =
            HashMap.fromList
              [ ("_inner", Aeson.toJSON (HashMap.fromList [("raw" :: T.Text, Aeson.String rawText), ("style" :: T.Text, Aeson.String styleText)])),
                ("_incoming", Aeson.toJSON (HashMap.fromList [("_style" :: T.Text, Aeson.String incomingStyleText)]))
              ]

          mergedEnv = HashMap.union envObj bnds

      case EDE.parse (T.encodeUtf8 format) of
        EDE.Failure err -> do
          let (_errStyle, errRendered) = Colour.parseAnsiChunks Colour.noStyle (T.pack $ show err)
          pure (formatted {rendered = errRendered})
        EDE.Success tmpl -> case EDE.render tmpl mergedEnv of
          EDE.Failure err -> do
            let (_errStyle, errRendered) = Colour.parseAnsiChunks Colour.noStyle (T.pack $ show err)
            pure (formatted {rendered = errRendered})
          EDE.Success renderedText -> do
            let (newStyle, newRendered) = Colour.parseAnsiChunks oldSt (TL.toStrict renderedText)
            _ <- updateStyle (const newStyle)
            pure (formatted {rendered = newRendered})
