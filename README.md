# deprecated

Other libraries are better.

* [convertible :: Stackage Server](https://www.stackage.org/package/convertible)
* [string-conversions :: Stackage Server](https://www.stackage.org/package/string-conversions)

[Haskellの文字列型同士の変換が欲しい場合はconvertibleかstring-conversionsを使うと良い感じです - ncaq](https://www.ncaq.net/2021/09/21/13/56/20/)

# string-transform

[![Build Status](https://travis-ci.org/ncaq/string-transform.svg?branch=master)](https://travis-ci.org/ncaq/string-transform)
![Hackage](https://img.shields.io/hackage/v/string-transform.svg)

simple and easy haskell string transform wrapper.

# provide function

* `toString`
* `toByteStringStrict`
* `toByteStringLazy`
* `toShortByteString`
* `toTextStrict`
* `toTextLazy`

# note

It wrapper expect that `ByteString` encode is utf-8.
