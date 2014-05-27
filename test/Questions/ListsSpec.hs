module Questions.ListsSpec where

import           Control.Exception (evaluate)
import           Questions.Lists
import           Test.Hspec
import           Test.QuickCheck

specLast :: [Spec]
specLast = do
    myLastImpl <- [myLast, myLast', myLast'']
    return $ describe "last" $ do
        it "returns the last element of an *nonempty* list" $
              property $ \x xs -> myLastImpl (x:xs) == (last (x:xs) :: Int)

        it "throws an exception if used with an empty list" $
              evaluate (myLast'' []) `shouldThrow` anyException

        {-it "should throw an error on empty list" $-}
         {-evaluate(myLast []) `shouldThrow` errorCall "Empty list has no last element"-}

specButLast :: [Spec]
specButLast = do
    butLast <- [myButLast, myButLast', myButLast'']
    return $ describe "butLast" $ do
        it "returns last but one element from list" $
            butLast [1,2,3,4,5] `shouldBe` (4::Int)

        it "shold fail on empty list" $
            evaluate(myButLast []) `shouldThrow` anyException

        it "shold fail on list with 1 element" $
            evaluate(myButLast [1::Int]) `shouldThrow` anyException

specAt :: [Spec]
specAt = do
    myAt <- [elementAt, elementAt', elementAt'', elementAt''']
    return $ describe "at" $
        it "should return at given index in list" $
          myAt [1,2,3,4,5] 3 `shouldBe` (3::Int)

specLength :: [Spec]
specLength = do
    myLen <- [myLength, myLength', myLength'', myLength''']
    return $ describe "length" $
        it "Hello world example is 13 chars length" $
            myLen "Hello, world!" `shouldBe` (13::Int)

specReverse :: [Spec]
specReverse = do
    myRev <- [myReverse]
    return $ describe "reverse" $
        it "produces list with elements in back order" $
            myRev [1,2,3,4] `shouldBe` ([4,3,2,1]::[Int])

specs :: [Spec]
specs = specLast
        ++ specButLast
        ++ specAt
        ++ specLength
        ++ specReverse

spec :: Spec
spec = foldr1 (>>) specs

main :: IO ()
main = hspec spec
