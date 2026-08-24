module Data.Sectile.SegmentsSpec (spec) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import Data.Sectile
import qualified Data.Sectile.Tmux as Colour
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Test.Hspec

spec :: Spec
spec = do
  describe "string" $ do
    it "renders plain text" $ do
      output <- renderSegment Colour.WithoutColours (string "hello")
      builderToText output `shouldBe` "hello"

    it "renders empty text" $ do
      output <- renderSegment Colour.WithoutColours (string "")
      builderToText output `shouldBe` ""

    it "preserves text content" $ do
      output <- renderSegment Colour.WithoutColours (string "foo bar baz")
      builderToText output `shouldBe` "foo bar baz"

  describe "row" $ do
    it "concatenates segments" $ do
      output <-
        renderSegment Colour.WithoutColours $
          row mapM "test" [string "a", string "b", string "c"]
      builderToText output `shouldBe` "abc"

    it "handles empty segment list" $ do
      output <-
        renderSegment Colour.WithoutColours $
          row mapM "empty" []
      builderToText output `shouldBe` ""

    it "handles single segment" $ do
      output <-
        renderSegment Colour.WithoutColours $
          row mapM "single" [string "only"]
      builderToText output `shouldBe` "only"

  describe "between" $ do
    it "wraps segments with start and end" $ do
      let segments = between (string "[") (string "]") [string "a", string "b"]
      output <-
        renderSegment Colour.WithoutColours $
          row mapM "wrapped" segments
      builderToText output `shouldBe` "[ab]"

    it "works with empty inner list" $ do
      let segments = between (string "<") (string ">") []
      output <-
        renderSegment Colour.WithoutColours $
          row mapM "empty-wrapped" segments
      builderToText output `shouldBe` "<>"

  describe "sh" $ do
    it "captures stdout" $ do
      output <- renderSegment Colour.WithoutColours (sh "test" "echo -n hello" Nothing)
      builderToText output `shouldBe` "hello"

    it "displays error on failure" $ do
      output <- renderSegment Colour.WithoutColours (sh "test-cmd" "false" Nothing)
      builderToText output `shouldBe` "Error on test-cmd"

    it "passes environment variables" $ do
      output <-
        renderSegment Colour.WithoutColours $
          sh "env-test" "echo -n $MY_VAR" (Just [("MY_VAR", "works")])
      builderToText output `shouldBe` "works"

  describe "time" $ do
    it "renders time with format" $ do
      output <- renderSegment Colour.WithoutColours (time "clock" "%H")
      let txt = builderToText output
      T.length txt `shouldBe` 2

  describe "explainSegment" $ do
    it "produces explanation for string" $ do
      output <- explainSegment Colour.WithoutColours (string "test")
      let txt = builderToText output
      txt `shouldSatisfy` T.isInfixOf "Type: string"

builderToText :: B.Builder -> T.Text
builderToText = T.decodeUtf8 . BSL.toStrict . B.toLazyByteString
