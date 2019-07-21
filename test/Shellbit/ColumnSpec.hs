module Shellbit.ColumnSpec (spec) where

import Shellbit.Column (grid)
import Test.Hspec      (Spec, describe, it, shouldBe)


spec :: Spec
spec =
  describe "grid" $ do
    it "is empty when given an empty list" $
      grid [] 16 `shouldBe` []

    it "is empty when all elements are empty" $
      grid [""] 16 `shouldBe` []

    it "is 1 x 1 when given a singleton list" $
      grid ["a"] 16 `shouldBe` [ [("a","")] ]

    it "has one row when all items can fit on one line" $
      grid ["a", "b"] 16 `shouldBe`
        [ [("a","\t"), ("b","")] ]

    it "has one column if any item is >= width" $
      grid ["a", "b234567812345678", "c"] 16 `shouldBe`
        [ [("a","")]
        , [("b234567812345678","")]
        , [("c","")]
        ]

    it "excludes empty items from single-column result" $
      grid ["a", "", "b234567812345678", "c"] 16 `shouldBe`
        [ [("a","")]
        , [("b234567812345678","")]
        , [("c","")]
        ]

    it "fills columns before rows" $
      grid ["a", "b", "c", "d", "e"] 32 `shouldBe`
        [ [("a","\t"), ("c","\t"), ("e","")]
        , [("b","\t"), ("d","")]
        ]

    it "excludes empty items from multi-column result" $
      grid ["a", "", "b", "c", "d"] 16 `shouldBe`
        [ [("a","\t"), ("c","")]
        , [("b","\t"), ("d","")]
        ]

    it "has a partial row for overflow items" $
      grid ["a", "b", "c", "d", "e"] 16 `shouldBe`
        [ [("a","\t"), ("d","")]
        , [("b","\t"), ("e","")]
        , [("c","")]
        ]

    it "applies padding to maintain a consistent column width" $
      grid ["a", "b", "c", "d2345678", "e", "f"] 48 `shouldBe`
        [ [("a","\t\t"), ("c",     "\t\t"), ("e","")]
        , [("b","\t\t"), ("d2345678","\t"), ("f","")]
        ]
