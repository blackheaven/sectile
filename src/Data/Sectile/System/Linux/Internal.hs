module Data.Sectile.System.Linux.Internal (errMsg, mkFormatted, tryReadFile, tryWriteFile, formatKiB, showFFloat1, readInt, readDouble) where

import qualified Control.Exception as Exception
import Control.Monad.State (State)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Sectile.Tmux as Colour
import Data.Sectile.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Read as T
import qualified Data.Time.Clock.POSIX as POSIX
import Numeric (showFFloat)
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.Process as Process

-- Internal helpers

-- | Build an error message from a segment name.
errMsg :: B.Builder -> T.Text
errMsg name = "Error on " <> TL.toStrict (TLE.decodeUtf8 (B.toLazyByteString name))

-- | Build a 'Formatted' value with standard explain structure.
mkFormatted :: B.Builder -> T.Text -> T.Text -> [(T.Text, T.Text)] -> State Env Formatted
mkFormatted name typeName txt extraFields = do
  currentSt <- currentStyle
  bnds <- currentBindings
  let (finalStyle, rendered) = Colour.parseAnsiChunks currentSt txt
      explain f =
        DetailList $
          [ DetailPlain $ "Name: " <> name,
            DetailPlain $ "Type: " <> T.encodeUtf8Builder typeName,
            DetailPlain $ "Value: " <> T.encodeUtf8Builder txt,
            DetailPlain $ "Style: " <> T.encodeUtf8Builder (T.pack $ show currentSt) <> " -> " <> T.encodeUtf8Builder (T.pack $ show finalStyle),
            DetailPlain $ "Rendered: " <> f rendered
          ]
            <> map (\(k, v) -> DetailPlain $ T.encodeUtf8Builder k <> ": " <> T.encodeUtf8Builder v) extraFields
            <> (if HashMap.null bnds then [] else [DetailPlain "Bindings:", DetailNested $ DetailList [DetailPlain (T.encodeUtf8Builder k <> " = " <> B.lazyByteString (Aeson.encode v)) | (k, v) <- List.sortOn fst (HashMap.toList bnds)]])
  _ <- updateStyle (const finalStyle)
  pure Formatted {..}

-- | Try to read a file, catching any IOException.
tryReadFile :: FilePath -> IO (Either IOError T.Text)
tryReadFile path =
  (Right . T.pack <$> readFile path)
    `Exception.catch` (\(e :: IOError) -> pure $ Left e)

-- | Try to write a file, catching any IOException.
tryWriteFile :: FilePath -> T.Text -> IO (Either IOError ())
tryWriteFile path content =
  (Right <$> writeFile path (T.unpack content))
    `Exception.catch` (\(e :: IOError) -> pure $ Left e)

-- | Format kibibytes to human-readable.
formatKiB :: Int -> T.Text
formatKiB kb
  | kb >= 1024 ^ (4 :: Int) = T.pack (showFFloat1 (fromIntegral kb / (1024.0 ** 4) :: Double)) <> "EiB"
  | kb >= 1024 ^ (3 :: Int) = T.pack (showFFloat1 (fromIntegral kb / (1024.0 ** 3) :: Double)) <> "TiB"
  | kb >= 1024 ^ (2 :: Int) = T.pack (showFFloat1 (fromIntegral kb / (1024.0 ** 2) :: Double)) <> "GiB"
  | kb >= 1024 = T.pack (showFFloat1 (fromIntegral kb / 1024.0 :: Double)) <> "MiB"
  | otherwise = T.pack (show kb) <> "KiB"

-- | Show a Double with 1 decimal place.
showFFloat1 :: Double -> String
showFFloat1 x = showFFloat (Just n) x ""
  where
    n
      | abs x >= 100 = 0
      | abs x >= 10 = 1
      | otherwise = 2

-- | Read an Int from Text, returning Nothing on failure.
readInt :: T.Text -> Maybe Int
readInt t = case T.decimal t of
  Right (n, _) -> Just n
  Left _ -> Nothing

-- | Read a Double from Text, returning Nothing on failure.
readDouble :: T.Text -> Maybe Double
readDouble t = case T.double t of
  Right (n, _) -> Just n
  Left _ -> Nothing
