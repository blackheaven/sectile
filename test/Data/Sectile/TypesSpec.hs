{-# LANGUAGE OverloadedStrings #-}

module Data.Sectile.TypesSpec (spec) where

import Control.Monad.State (evalState, execState)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Sectile.Tmux (ChunkStyle (..), noStyle)
import Data.Sectile.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "Env state helpers" $ do
    let emptyEnv = Env noStyle HashMap.empty

    it "currentStyle gets style" $ do
      evalState currentStyle emptyEnv `shouldBe` noStyle

    it "updateStyle modifies and returns new style" $ do
      let newStyle = noStyle {chunkStyleItalic = Just True}
      let res = evalState (updateStyle (const newStyle)) emptyEnv
      res `shouldBe` newStyle

    it "currentBindings gets bindings" $ do
      evalState currentBindings emptyEnv `shouldBe` HashMap.empty

    it "updateBindings modifies and returns new bindings" $ do
      let newBindings = HashMap.singleton "key" (Aeson.String "value")
      let res = evalState (updateBindings (const newBindings)) emptyEnv
      res `shouldBe` newBindings

    it "appendBindings adds bindings" $ do
      let initialEnv = Env noStyle (HashMap.singleton "k1" (Aeson.String "v1"))
      let newBindings = HashMap.singleton "k2" (Aeson.String "v2")
      let res = evalState (appendBindings newBindings) initialEnv
      res `shouldBe` HashMap.fromList [("k1", Aeson.String "v1"), ("k2", Aeson.String "v2")]

    it "scopeBindings prefixes child bindings and preserves parent bindings" $ do
      let initialEnv = Env noStyle (HashMap.singleton "parent" (Aeson.String "pv"))
      let action = do
            _ <- appendBindings (HashMap.singleton "child" (Aeson.String "cv"))
            pure ("result" :: String)
      let finalEnv = execState (scopeBindings "test" action) initialEnv
      bindings finalEnv
        `shouldBe` HashMap.fromList
          [ ("parent", Aeson.String "pv"),
            ("test.child", Aeson.String "cv")
          ]

  describe "Binding helpers" $ do
    it "unitBindings generates correctly for KB" $ do
      let bnds = unitBindings "B" "disk" 2048
      bnds
        `shouldBe` HashMap.fromList
          [ ("disk", Aeson.String "2.0KiB"),
            ("disk.raw", Aeson.Number 2048),
            ("disk.value.full", Aeson.String "2.0"),
            ("disk.value.round", Aeson.String "2"),
            ("disk.unit.full", Aeson.String "KiB"),
            ("disk.unit.base", Aeson.String "B"),
            ("disk.unit.prefix", Aeson.String "Ki")
          ]

    it "percentBindings generates correctly" $ do
      let bnds = percentBindings "usage" 0.426
      bnds
        `shouldBe` HashMap.fromList
          [ ("usage", Aeson.String "42.6%"),
            ("usage.raw", Aeson.Number (realToFrac (0.426 :: Double))),
            ("usage.absolute", Aeson.String "0.43"),
            ("usage.percent.full", Aeson.String "42.6"),
            ("usage.percent.round", Aeson.String "43")
          ]
