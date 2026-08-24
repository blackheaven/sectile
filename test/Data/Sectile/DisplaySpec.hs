module Data.Sectile.DisplaySpec (spec) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import Data.Sectile
import qualified Data.Sectile.Tmux as Colour
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Test.Hspec

spec :: Spec
spec = do
  describe "takeStart" $ do
    it "truncates to n characters" $ do
      output <-
        renderSegment Colour.WithoutColours $
          takeStart 5 (string "Hello, world!")
      builderToText output `shouldBe` "Hello"

    it "returns full text when shorter than n" $ do
      output <-
        renderSegment Colour.WithoutColours $
          takeStart 20 (string "Hello")
      builderToText output `shouldBe` "Hello"

    it "returns empty for n=0" $ do
      output <-
        renderSegment Colour.WithoutColours $
          takeStart 0 (string "Hello")
      builderToText output `shouldBe` ""

  describe "takeEnd" $ do
    it "keeps last n characters" $ do
      output <-
        renderSegment Colour.WithoutColours $
          takeEnd 6 (string "Hello, world!")
      builderToText output `shouldBe` "world!"

    it "returns full text when shorter than n" $ do
      output <-
        renderSegment Colour.WithoutColours $
          takeEnd 20 (string "Hello")
      builderToText output `shouldBe` "Hello"

  describe "padStart" $ do
    it "pads with spaces at the start" $ do
      output <-
        renderSegment Colour.WithoutColours $
          padStart 8 (string "Hi")
      builderToText output `shouldBe` "      Hi"

    it "does not pad when already long enough" $ do
      output <-
        renderSegment Colour.WithoutColours $
          padStart 2 (string "Hello")
      builderToText output `shouldBe` "Hello"

  describe "padEnd" $ do
    it "pads with spaces at the end" $ do
      output <-
        renderSegment Colour.WithoutColours $
          padEnd 8 (string "Hi")
      builderToText output `shouldBe` "Hi      "

    it "does not pad when already long enough" $ do
      output <-
        renderSegment Colour.WithoutColours $
          padEnd 2 (string "Hello")
      builderToText output `shouldBe` "Hello"

  describe "fixedSizeStart" $ do
    it "pads short text at start" $ do
      output <-
        renderSegment Colour.WithoutColours $
          fixedSizeStart 6 (string "Hi")
      builderToText output `shouldBe` "    Hi"

    it "truncates long text from end" $ do
      output <-
        renderSegment Colour.WithoutColours $
          fixedSizeStart 5 (string "Hello, world!")
      builderToText output `shouldBe` "Hello"

  describe "fixedSizeEnd" $ do
    it "pads short text at end" $ do
      output <-
        renderSegment Colour.WithoutColours $
          fixedSizeEnd 6 (string "Hi")
      builderToText output `shouldBe` "Hi    "

    it "truncates long text from start" $ do
      output <-
        renderSegment Colour.WithoutColours $
          fixedSizeEnd 6 (string "Hello, world!")
      builderToText output `shouldBe` "world!"

builderToText :: B.Builder -> T.Text
builderToText = T.decodeUtf8 . BSL.toStrict . B.toLazyByteString
