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
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Maybe (catMaybes)
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
                DetailPlain $ "Style: " <> T.encodeUtf8Builder (T.pack $ show currentSt) <> " -> " <> T.encodeUtf8Builder (T.pack $ show finalStyle),
                DetailPlain $ "Rendered: " <> f rendered
              ]
                <> (if HashMap.null bnds then [] else [DetailPlain "Bindings:", DetailNested $ DetailList [DetailPlain (T.encodeUtf8Builder k <> " = " <> B.lazyByteString (Aeson.encode v)) | (k, v) <- List.sortOn fst (HashMap.toList bnds)]])
      _ <- updateStyle (const finalStyle)
      pure Formatted {..}

-- | Combine multiple segments into a named row.
row :: (Monad m) => SegmentsRunner m -> Scoping -> Name -> [Segment m] -> Segment m
row runSegments scoping name@(Name nameBuilder) ss =
  Segment $ do
    states <- runSegments (.runSegment) ss
    let wrap = case scoping of
                 Isolating -> scopeBindings name
                 Propagating -> id
    pure $ wrap $ do
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
                DetailPlain $ "Style: " <> T.encodeUtf8Builder (T.pack $ show currentSt) <> " -> " <> T.encodeUtf8Builder (T.pack $ show finalStyle),
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
                DetailPlain $ "Style: " <> T.encodeUtf8Builder (T.pack $ show currentSt) <> " -> " <> T.encodeUtf8Builder (T.pack $ show finalStyle),
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
          styleText =
            T.decodeUtf8 $
              LBS.toStrict $
                B.toLazyByteString $
                  Colour.renderChunksUtf8BSBuilder Colour.With24BitColours formatted.rendered
          incomingStyleText =
            T.replace "#[default]" "" $
              T.decodeUtf8 $
                LBS.toStrict $
                  B.toLazyByteString $
                    Colour.renderChunksUtf8BSBuilder Colour.With24BitColours [Colour.Chunk "" oldSt]

          innerSt = case formatted.rendered of
            (c : _) -> Colour.chunkStyle c
            [] -> Colour.noStyle

          styleToObj :: T.Text -> Colour.ChunkStyle -> Aeson.Value
          styleToObj sText st =
            Aeson.toJSON $
              HashMap.fromList $
                [ ("raw" :: T.Text, Aeson.String sText)
                ]
                  <> catMaybes
                    [ (,) "foreground" . Aeson.String . Colour.renderColour <$> Colour.chunkStyleForeground st,
                      (,) "background" . Aeson.String . Colour.renderColour <$> Colour.chunkStyleBackground st,
                      (,) "italic" . Aeson.Bool <$> Colour.chunkStyleItalic st,
                      (,) "strikethrough" . Aeson.Bool <$> Colour.chunkStyleStrikethrough st,
                      (,) "swapForegroundBackground" . Aeson.Bool <$> Colour.chunkStyleSwapForegroundBackground st,
                      (,) "concealed" . Aeson.Bool <$> Colour.chunkStyleConcealed st,
                      (,) "overlined" . Aeson.Bool <$> Colour.chunkStyleOverlined st,
                      (,) "bold" . Aeson.Bool . (== Colour.BoldIntensity) <$> Colour.chunkStyleConsoleIntensity st,
                      (,) "dim" . Aeson.Bool . (== Colour.FaintIntensity) <$> Colour.chunkStyleConsoleIntensity st,
                      (,) "underlined" . Aeson.Bool . (`elem` [Colour.SingleUnderline, Colour.DoubleUnderline]) <$> Colour.chunkStyleUnderlining st,
                      (,) "blink" . Aeson.Bool . (`elem` [Colour.SlowBlinking, Colour.RapidBlinking]) <$> Colour.chunkStyleBlinking st,
                      (,) "hyperlink" . Aeson.String <$> Colour.chunkStyleHyperlink st
                    ]

          envObj =
            HashMap.fromList
              [ ( "_inner",
                  Aeson.toJSON $
                    HashMap.fromList
                      [ ("raw" :: T.Text, Aeson.String rawText),
                        ("style" :: T.Text, styleToObj styleText innerSt)
                      ]
                ),
                ( "_incoming",
                  Aeson.toJSON $
                    HashMap.fromList
                      [ ("style" :: T.Text, styleToObj incomingStyleText oldSt)
                      ]
                )
              ]

          mergedEnv = HashMap.union envObj bnds

      case EDE.parse (T.encodeUtf8 format) of
        EDE.Failure err -> do
          let (_errStyle, errRendered) = Colour.parseAnsiChunks Colour.noStyle (T.pack $ show err)
          pure (formatted {rendered = errRendered})
        EDE.Success tmpl -> case EDE.render tmpl (nestify mergedEnv) of
          EDE.Failure err -> do
            let (_errStyle, errRendered) = Colour.parseAnsiChunks Colour.noStyle (T.pack $ show err)
            pure (formatted {rendered = errRendered})
          EDE.Success renderedText -> do
            let (newStyle, newRendered) = Colour.parseAnsiChunks oldSt (TL.toStrict renderedText)
            _ <- updateStyle (const newStyle)
            pure (formatted {rendered = newRendered})

    nestify :: HashMap.HashMap T.Text Aeson.Value -> HashMap.HashMap T.Text Aeson.Value
    nestify flatMap = HashMap.fromList $ map (\(k, v) -> (Key.toText k, v)) $ KeyMap.toList $ List.foldl' insertPath KeyMap.empty (HashMap.toList flatMap)
      where
        insertPath :: KeyMap.KeyMap Aeson.Value -> (T.Text, Aeson.Value) -> KeyMap.KeyMap Aeson.Value
        insertPath obj (key, val) = go obj (T.splitOn "." key) val

        go :: KeyMap.KeyMap Aeson.Value -> [T.Text] -> Aeson.Value -> KeyMap.KeyMap Aeson.Value
        go obj [] _ = obj
        go obj [k] val =
          let k' = Key.fromText k
           in case KeyMap.lookup k' obj of
                Just (Aeson.Object _) ->
                  obj
                _ ->
                  KeyMap.insert k' val obj
        go obj (k : ks) val =
          let k' = Key.fromText k
           in case KeyMap.lookup k' obj of
                Just (Aeson.Object existingObj) ->
                  KeyMap.insert k' (Aeson.Object (go existingObj ks val)) obj
                _ ->
                  KeyMap.insert k' (Aeson.Object (go KeyMap.empty ks val)) obj
