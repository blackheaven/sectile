module Data.Sectile.TmuxSpec (spec) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Sectile.Tmux
import Test.Hspec

spec :: Spec
spec = do
  describe "Tmux rendering" $ do
    it "renders unstyled chunk as plain text" $ do
      let chunk = Chunk "hello" noStyle
          res = renderChunksUtf8BSBuilder With24BitColours [chunk]
      BL.toStrict (B.toLazyByteString res) `shouldBe` "hello"

    it "renders fg colour" $ do
      let style = noStyle {chunkStyleForeground = Just (Colour8 Bright Red)}
          chunk = Chunk "hello" style
          res = renderChunksUtf8BSBuilder With24BitColours [chunk]
      BL.toStrict (B.toLazyByteString res) `shouldBe` "#[fg=red]hello#[default]"

    it "renders bg colour and bold" $ do
      let style = noStyle {chunkStyleBackground = Just (Colour24Bit 255 0 255), chunkStyleConsoleIntensity = Just BoldIntensity}
          chunk = Chunk "hello" style
          res = renderChunksUtf8BSBuilder With24BitColours [chunk]
      BL.toStrict (B.toLazyByteString res) `shouldBe` "#[bg=#ff00ff,bold]hello#[default]"

    it "renders multiple chunks" $ do
      let chunk1 = Chunk "hello " (noStyle {chunkStyleForeground = Just (Colour8 Dull Blue)})
          chunk2 = Chunk "world" noStyle
          res = renderChunksUtf8BSBuilder With24BitColours [chunk1, chunk2]
      BL.toStrict (B.toLazyByteString res) `shouldBe` "#[fg=blue]hello #[default]world"

    it "handles WithoutColours capability" $ do
      let style = noStyle {chunkStyleForeground = Just (Colour8 Bright Red)}
          chunk = Chunk "hello" style
          res = renderChunksUtf8BSBuilder WithoutColours [chunk]
      BL.toStrict (B.toLazyByteString res) `shouldBe` "hello"
