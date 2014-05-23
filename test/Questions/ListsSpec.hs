module Questions.ListsSpec where

import           Control.Exception (evaluate)
import           Questions.Lists
import           Test.Hspec
import           Test.QuickCheck

specs :: [Spec]
specs = specLast ++ specButLast ++ specAt

spec :: Spec
spec = foldr1 (>>) $ specs

specLast :: [Spec]
specLast = do
    myLastImpl <- [myLast, myLast', myLast'']
    return $ describe "last" $ do
        it "returns the last element of an *nonempty* list" $
              property $ \x xs -> myLastImpl (x:xs) == (last (x:xs) :: Int)

        it "throws an exception if used with an empty list" $ do
              evaluate (myLast'' []) `shouldThrow` anyException

        {-it "should throw an error on empty list" $-}
         {-evaluate(myLast []) `shouldThrow` errorCall "Empty list has no last element"-}

specButLast :: [Spec]
specButLast = do
    butLast <- [myButLast, myButLast', myButLast'']
    return $ describe "butLast" $ do
        it "returns last but one element from list" $
            butLast [1,2,3,4,5] `shouldBe` (4::Int)

        it "shold fail on empty list" $ do
            evaluate(myButLast []) `shouldThrow` anyException

        it "shold fail on list with 1 element" $ do
            evaluate(myButLast [1::Int]) `shouldThrow` anyException

specAt :: [Spec]
specAt = do
    myAt <- [element_at, element_at', element_at'', element_at''']
    return $ describe "at" $ do
        it "should return at given index in list" $ 
          myAt [1,2,3,4,5] 3 `shouldBe` (3::Int)


main :: IO ()
{-main = foldr (>>) (return ()) $ map hspec specLast-}
main = hspec spec
