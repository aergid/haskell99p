module Questions.ListsSpec where

import           Control.Exception (evaluate)
import           Questions.Lists
import           Test.Hspec
import           Test.QuickCheck

specList :: [Spec]
specList = do
    myLastImpl <- [myLast, myLast', myLast'']
    return $ describe "last" $ do
        it "should return last element in a list" $
             myLastImpl [1,2,3] `shouldBe` (3::Int)

    {-it "should throw an error on empty list" $-}
     {-evaluate(myLast []) `shouldThrow` errorCall "Empty list has no last element"-}


  {-describe "last'" $ do-}
    {-it "should return last element in a list" $-}
      {-myLast [1,2,3,4] `shouldBe` (4::Int)-}

  {-describe "last''" $ do-}
    {-it "returns the last element of an *nonempty* list" $-}
          {-property $ \x xs -> myLast'' (x:xs) == (last (x:xs) :: Int)-}

    {-it "throws an exception if used with an empty list" $ do-}
          {-evaluate (myLast'' []) `shouldThrow` anyException-}

  {-describe "at" $ do-}
    {-it "should return at given index in list" $ -}
      {-element_at [1,2,3,4,5] 3 `shouldBe` (3::Int)-}

  {-describe "at'" $ do-}
    {-it "should return at given index in list" $ -}
      {-element_at' [1,2,3,4,5] 4 `shouldBe` (4::Int)-}

  {-describe "at''" $ do-}
    {-it "should return at given index in list" $ -}
      {-element_at'' [1,2,3,4,5] 4 `shouldBe` (4::Int)-}

  {-describe "at'''" $ do-}
    {-it "should return at given index in list" $ -}
      {-element_at''' [1,2,3,4,5] 4 `shouldBe` (4::Integer)-}

{-main :: IO ()-}
{-main = hspec spec-}
{-main = undefined-}

main = foldr (>>) (return ()) $ map hspec specList
