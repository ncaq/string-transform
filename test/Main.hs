module Main (main) where

import           Data.String.Convert
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
    [ testGroup "SmallCheck" smallCheckTest
    , testGroup "Unit tests" hunitTest
    ]

smallCheckTest :: [TestTree]
smallCheckTest =
    [ testProperty "toByteStringStrict" prop_toByteStringStrict_toString
    , testProperty "toByteStringLazy" prop_toByteStringLazy_toString
    , testProperty "toTextStrict" prop_toTextStrict_toString
    , testProperty "toTextLazy" prop_toTextLazy_toString
    ]

hunitTest :: [TestTree]
hunitTest =
    [ testCase "toByteStringStrict 日本語" case_japanese_toByteStrictStrict_toString
    ]

prop_toByteStringStrict_toString :: String -> Bool
prop_toByteStringStrict_toString string = string == toString (toByteStringStrict string)

prop_toByteStringLazy_toString :: String -> Bool
prop_toByteStringLazy_toString string = string == toString (toByteStringLazy string)

prop_toTextStrict_toString :: String -> Bool
prop_toTextStrict_toString string = string == toString (toTextStrict string)

prop_toTextLazy_toString :: String -> Bool
prop_toTextLazy_toString string = string == toString (toTextLazy string)

case_japanese_toByteStrictStrict_toString :: Assertion
case_japanese_toByteStrictStrict_toString = toString (toByteStringStrict "日本語") @?= "日本語"
