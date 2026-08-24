module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Convert (convertBar)
import qualified Data.ByteString.Builder as B
import qualified Data.Either.Validation as V
import Data.Sectile (explainSegment, renderSegment, row)
import qualified Data.Sectile.Tmux as Colour
import qualified Data.Text.IO as Text.IO
import qualified Dhall
import qualified Dhall.Core
import DhallTypes (BarConfig)
import Options.Applicative as Options
import System.IO (stdout)

main :: IO ()
main = do
  args <- parseArgs
  case args of
    DumpFormat ->
      case Dhall.expected (Dhall.auto @BarConfig) of
        V.Success result -> Text.IO.putStrLn (Dhall.Core.pretty result)
        V.Failure errors -> print errors
    Render (RenderArgs {..}) -> do
      bar <- Dhall.inputFile (Dhall.auto @BarConfig) configFile
      let segments = convertBar bar
          status = row mapConcurrently "bar" segments
      output <- renderSegment capabilities status
      B.hPutBuilder stdout output
      putStrLn ""
    Explain (RenderArgs {..}) -> do
      bar <- Dhall.inputFile (Dhall.auto @BarConfig) configFile
      let segments = convertBar bar
          status = row mapConcurrently "bar" segments
      output <- explainSegment capabilities status
      B.hPutBuilder stdout output
      putStrLn ""

parseArgs :: IO Args
parseArgs =
  customExecParser (prefs showHelpOnEmpty) $
    info (argsParser <**> helper) (fullDesc <> header "sectile: composable status line from Dhall config")
  where
    argsParser :: Parser Args
    argsParser =
      hsubparser
        ( command "dump-dhall-format" (info (pure DumpFormat) (progDesc "Dump Dhall type definition"))
            <> command "render" (info (Render <$> renderP) (progDesc "Render the status bar"))
            <> command "explain" (info (Explain <$> renderP) (progDesc "Explain the status bar configuration"))
        )
    renderP :: Parser RenderArgs
    renderP =
      RenderArgs
        <$> strOption (long "config" <> short 'c' <> metavar "FILE_PATH" <> help "Dhall config file (.dhall)")
        <*> colourFlag
    colourFlag :: Parser Colour.TerminalCapabilities
    colourFlag =
      flag' Colour.With24BitColours (long "24bit-colours" <> help "Use 24-bit true colour")
        <|> flag' Colour.With8BitColours (long "8bit-colours" <> help "Use 8-bit true colour")
        <|> flag' Colour.WithoutColours (long "no-colours" <> help "Disable colours")
        <|> pure Colour.With8Colours

data Args = DumpFormat | Render RenderArgs | Explain RenderArgs
  deriving stock (Eq, Ord, Show)

data RenderArgs = RenderArgs
  { configFile :: FilePath,
    capabilities :: Colour.TerminalCapabilities
  }
  deriving stock (Eq, Ord, Show)
