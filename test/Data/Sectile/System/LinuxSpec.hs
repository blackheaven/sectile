module Data.Sectile.System.LinuxSpec (spec) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import Data.Sectile
import Data.Sectile.System.Linux
import qualified Data.Sectile.Tmux as Colour
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Test.Hspec

spec :: Spec
spec = do
  describe "uptime" $ do
    it "renders uptime from /proc/uptime" $ do
      output <- renderSegment Colour.WithoutColours (uptime "uptime")
      let txt = builderToText output
      -- Should contain "d", "h", "m" formatting
      txt `shouldSatisfy` T.isInfixOf "d "
      txt `shouldSatisfy` T.isInfixOf "h "
      txt `shouldSatisfy` T.isInfixOf "m"

  describe "memory" $ do
    it "renders memory usage from /proc/meminfo" $ do
      output <- renderSegment Colour.WithoutColours (memory "mem")
      let txt = builderToText output
      -- Should contain percentage and size units
      txt `shouldSatisfy` T.isInfixOf "%"
      txt `shouldSatisfy` (\t -> any (flip T.isInfixOf t) ["EiB", "TiB", "GiB", "MiB", "KiB"])

  describe "load" $ do
    it "renders load averages from /proc/loadavg" $ do
      output <- renderSegment Colour.WithoutColours (load "load")
      let txt = builderToText output
      -- Load averages are space-separated decimals
      length (T.words txt) `shouldBe` 3

  describe "cpu" $ do
    it "renders CPU usage percentage" $ do
      output <- renderSegment Colour.WithoutColours (cpu "cpu")
      let txt = builderToText output
      txt `shouldSatisfy` T.isInfixOf "%"

  describe "disk" $ do
    it "renders disk info for root mount" $ do
      output <- renderSegment Colour.WithoutColours (disk "disk" "/")
      let txt = builderToText output
      txt `shouldSatisfy` T.isInfixOf "/"

  describe "networkDown" $ do
    it "renders network receive bytes for lo" $ do
      output <- renderSegment Colour.WithoutColours (networkDown "net" "lo")
      let txt = builderToText output
      -- Should be a number (bytes) or error
      txt `shouldSatisfy` (\t -> T.all (\c -> c >= '0' && c <= '9') t || T.isInfixOf "Error" t)

  describe "networkUp" $ do
    it "renders network transmit bytes for lo" $ do
      output <- renderSegment Colour.WithoutColours (networkUp "net" "lo")
      let txt = builderToText output
      txt `shouldSatisfy` (\t -> T.all (\c -> c >= '0' && c <= '9') t || T.isInfixOf "Error" t)

builderToText :: B.Builder -> T.Text
builderToText = T.decodeUtf8 . BSL.toStrict . B.toLazyByteString
