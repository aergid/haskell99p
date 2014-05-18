module Questions.ListsSpec where

import Test.Hspec
import Questions.Lists

spec :: Spec
spec = do
  describe "last" $ do
    it "should return last element in a list" $
      myLast [1,2,3] `shouldBe` (3::Int)

  describe "last'" $ do
    it "should return last element in a list" $
      myLast [1,2,3,4] `shouldBe` (4::Int)



main :: IO ()
main = hspec spec
