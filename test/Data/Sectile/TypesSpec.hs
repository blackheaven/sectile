{-# LANGUAGE OverloadedStrings #-}

module Data.Sectile.TypesSpec (spec) where

import Control.Monad.State (evalState, execState)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Sectile.Tmux (noStyle, ChunkStyle(..))
import Data.Sectile.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "Env state helpers" $ do
    let emptyEnv = Env noStyle HashMap.empty
    
    it "currentStyle gets style" $ do
      evalState currentStyle emptyEnv `shouldBe` noStyle

    it "updateStyle modifies and returns new style" $ do
      let newStyle = noStyle { chunkStyleItalic = Just True }
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
      bindings finalEnv `shouldBe` HashMap.fromList 
        [ ("parent", Aeson.String "pv")
        , ("test.child", Aeson.String "cv")
        ]
