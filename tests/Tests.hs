{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (liftM)
import Data.Monoid ((<>))
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.List as List
import Data.Text as Text
import qualified Data.Text as ST

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Gen, Property, Arbitrary(..), elements, sized, oneof,
                        listOf)
import Test.QuickCheck.Monadic (assert)
import Data.Yaml(Object, Value(Object, Null))
import qualified Data.HashMap.Strict as HashMap

import Data.Yaml.Config.Internal (Config(Config), Key, subconfig, keys,
                                  fullpath)

type CorrectPath = [Key]

data DeepObject = DeepObject Config CorrectPath deriving (Eq, Show)

instance Arbitrary Text where
    arbitrary = liftM Text.pack arbitrary

instance Arbitrary DeepObject where
    arbitrary = deepConfig

newObject :: [(Key, Value)] -> Object
newObject = HashMap.fromList

deepConfig :: Gen DeepObject
deepConfig = sized deepConfig' >>= \(keys, obj) ->
    return $ DeepObject (Config [] obj) $ List.tail keys

deepConfig' :: Int -> Gen ([Key], Object)
deepConfig' 0 = arbitrary >>= \newKey -> return $ ([newKey], newObject [])
deepConfig' n | n > 0 = do
    newKey <- arbitrary
    (keys, newNode) <- deepConfig' (pred n)
    return $ (newKey : keys, newObject [(List.head keys , Object newNode)])

testCorrectSubconfigPath :: DeepObject -> Bool
testCorrectSubconfigPath (DeepObject _ []) = True
testCorrectSubconfigPath (DeepObject config (key : others)) =
    maybe False (`subcheck` others) $ subconfig config key
  where
    subcheck sc keys = testCorrectSubconfigPath $ DeepObject sc keys

testWrongSubconfigPath :: DeepObject -> Bool
testWrongSubconfigPath (DeepObject config []) = List.null $ keys config
testWrongSubconfigPath (DeepObject config (nextKey : others)) =
    maybe nextCheck (const False) $ subconfig config wrongKey
  where
    nextCheck = maybe False (testWrongSubconfigPath . nextDeep) $
        subconfig config nextKey
    nextDeep = flip DeepObject others
    wrongKey = nextKey <> "_"

testCorrectPath :: DeepObject -> Bool
testCorrectPath = testCorrectPath' []
testCorrectPath' path (DeepObject config (nextKey : others)) = checkPath config
    && (maybe False (`subcheck` others) $ subconfig config nextKey)
  where
    subcheck sc keys = testCorrectPath' (nextKey : path) $ DeepObject sc keys
    checkPath c = fullpath c nextKey ==
                  (ST.intercalate "." $ List.reverse $ nextKey : path)
testCorrectPath' _ (DeepObject config []) = List.null $ keys config

main :: IO ()
main = defaultMain
    [
        testGroup "lookup"
            [ testProperty "wrongSubconfigPath" testWrongSubconfigPath
            , testProperty "correctSubconfigPath" testCorrectSubconfigPath
            , testProperty "correctPath" testCorrectPath
            ]
    ]
