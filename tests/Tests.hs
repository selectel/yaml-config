{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (liftM)
import Data.List as List
import Data.Monoid ((<>))
import Data.Text as Text

import qualified Data.HashMap.Strict as HashMap
import Data.Yaml (Object, Value (Object))
import Test.QuickCheck (Arbitrary (..), Gen, expectFailure, sized)
import Test.QuickCheck.Monadic (monadicIO, run)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty)

import Data.Yaml.Config.Internal (Config (Config), Key, fullpath, keys, subconfig)

type CorrectPath = [Key]

data DeepObject = DeepObject Config CorrectPath deriving (Eq, Show)

instance Arbitrary Text where
    arbitrary = liftM Text.pack arbitrary

instance Arbitrary DeepObject where
    arbitrary = deepConfig

newObject :: [(Key, Value)] -> Object
newObject = HashMap.fromList

deepConfig :: Gen DeepObject
deepConfig = sized deepConfig' >>= \(ks, obj) ->
    return $ DeepObject (Config [] obj) $ List.tail ks

deepConfig' :: Int -> Gen ([Key], Object)
deepConfig' 0 = arbitrary >>= \newKey -> return ([newKey], newObject [])
deepConfig' n | n > 0 = do
    newKey <- arbitrary
    (ks, newNode) <- deepConfig' (pred n)
    return (newKey : ks, newObject [(List.head ks , Object newNode)])

testCorrectSubconfigPath :: DeepObject -> Bool
testCorrectSubconfigPath (DeepObject _ []) = True
testCorrectSubconfigPath (DeepObject config (key : others)) =
    maybe False (`subcheck` others) $ subconfig key config
  where
    subcheck sc = testCorrectSubconfigPath . DeepObject sc

testWrongSubconfigPath' :: DeepObject -> Property
testWrongSubconfigPath' = expectFailure . monadicIO . run . testWrongSubconfigPath

testWrongSubconfigPath :: DeepObject -> IO Bool
testWrongSubconfigPath (DeepObject config []) = return . List.null $ keys config
testWrongSubconfigPath (DeepObject config (nextKey : others)) =
    maybe nextCheck (const $ return False) $ subconfig wrongKey config
  where
    nextCheck = maybe (return False) (testWrongSubconfigPath . nextDeep) $
        subconfig nextKey config
    nextDeep = flip DeepObject others
    wrongKey = nextKey <> "_"

testCorrectPath :: DeepObject -> Bool
testCorrectPath = testCorrectPath' []
  where
    testCorrectPath' path (DeepObject config (nextKey : others)) = checkPath config
        && maybe False (`subcheck` others) (subconfig nextKey config)
      where
        subcheck sc = testCorrectPath' (nextKey : path) . DeepObject sc
        checkPath c = fullpath c nextKey ==
            Text.intercalate "." (List.reverse $ nextKey : path)
    testCorrectPath' _ (DeepObject config []) = List.null $ keys config

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ testProperty "wrongSubconfigPath" testWrongSubconfigPath'
    , testProperty "correctSubconfigPath" testCorrectSubconfigPath
    , testProperty "correctPath" testCorrectPath
    ]
