{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Data.String.Transform
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
    [ testProperty "s == toString (toByteStringStrict s)"
        (\(s :: String) -> s == toString (toByteStringStrict s))
    , testProperty "s == toString (toByteStringLazy s)"
        (\(s :: String) -> s == toString (toByteStringLazy s))
    , testProperty "s == toString (toTextStrict s)"
        (\(s :: String) -> s == toString (toTextStrict s))
    , testProperty "s == toString (toTextLazy s)"
        (\(s :: String) -> s == toString (toTextLazy s))
    , testProperty "a == b ⇔ toByteStringStrict a == toByteStringStrict b"
        (\(a :: String) (b :: String) ->
             let p = a == b
                 q = toByteStringStrict a == toByteStringStrict b
             in p == q)
    ]

hunitTest :: [TestTree]
hunitTest =
    [ testCase "toString 1" (toString 1 @?= "1")
    , testCase "toByteStringStrict 1" (toByteStringStrict 1 @?= toByteStringStrict "1")
    , testCase "toTextStrict 1" (toTextStrict 1 @?= toTextStrict "1")
    , testCase "toByteStringStrict 日本語" (toString (toByteStringStrict "日本語") @?= "日本語")
    ]
