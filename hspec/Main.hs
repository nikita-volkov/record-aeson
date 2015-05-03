module Main where

import BasePrelude
import Test.Hspec

import Record
import Record.Aeson
import Data.Aeson


main =
  hspec $ do
    it "Encoding" $ do
      shouldBe
        (encode (toJSON ([r| { a = 'a', b = { c = 'c', d = 'd' } } |]))) 
        "{\"a\":\"a\",\"b\":{\"d\":\"d\",\"c\":\"c\"}}"
