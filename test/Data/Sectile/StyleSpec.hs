module Data.Sectile.StyleSpec (spec) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import Data.Sectile
import Data.Sectile.Tmux (Brightness (..), TerminalColour (..))
import qualified Data.Sectile.Tmux as Colour
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Optics.Core as Optics
import Test.Hspec

spec :: Spec
spec = do
  describe "between" $ do
    it "prepends start and appends end" $ do
      let result = between (string "[") (string "]") [string "a"]
      output <-
        renderSegment Colour.WithoutColours $
          row mapM Isolating "test" result
      builderToText output `shouldBe` "[a]"

  describe "changeStyle" $ do
    it "modifies incoming style" $ do
      output <-
        renderSegment Colour.WithoutColours $
          changeStyle resetStyle (string "hello")
      builderToText output `shouldBe` "hello"

  describe "forceStyle" $ do
    it "applies style to output" $ do
      output <-
        renderSegment Colour.WithoutColours $
          forceStyle resetStyle (string "hello")
      builderToText output `shouldBe` "hello"

  describe "resetStyle" $ do
    it "produces noStyle" $ do
      let result = resetStyle Colour.noStyle
      result `shouldBe` Colour.noStyle

    it "clears any existing style" $ do
      let styled = Colour.noStyle {Colour.chunkStyleItalic = Just True}
      resetStyle styled `shouldBe` Colour.noStyle

  describe "swapForegroundBackgroundStyle" $ do
    it "swaps foreground and background" $ do
      let original =
            Colour.noStyle
              { Colour.chunkStyleForeground = Just (Colour.Colour8 Dull Red),
                Colour.chunkStyleBackground = Just (Colour.Colour8 Bright Blue)
              }
          swapped = swapForegroundBackgroundStyle original
      Colour.chunkStyleForeground swapped `shouldBe` Just (Colour.Colour8 Bright Blue)
      Colour.chunkStyleBackground swapped `shouldBe` Just (Colour.Colour8 Dull Red)

    it "handles noStyle" $ do
      swapForegroundBackgroundStyle Colour.noStyle `shouldBe` Colour.noStyle

  describe "style optics" $ do
    it "styleItalic gets and sets" $ do
      let s = Optics.set styleItalic (Just True) Colour.noStyle
      Optics.view styleItalic s `shouldBe` Just True

    it "styleForeground gets and sets" $ do
      let colour = Colour.Colour8 Dull Green
          s = Optics.set styleForeground (Just colour) Colour.noStyle
      Optics.view styleForeground s `shouldBe` Just colour

    it "styleBackground gets and sets" $ do
      let colour = Colour.Colour8 Bright Yellow
          s = Optics.set styleBackground (Just colour) Colour.noStyle
      Optics.view styleBackground s `shouldBe` Just colour

builderToText :: B.Builder -> T.Text
builderToText = T.decodeUtf8 . BSL.toStrict . B.toLazyByteString
