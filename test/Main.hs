module Main where

import Test.Hspec
import Brick.Widgets.Calendar
import Data.Time

main :: IO ()
main = hspec $ do
  describe "Calendar Navigation" $ do
    let standardState year month day = 
          CalendarState year month (Just $ fromGregorian year month day) defaultCalendarConfig
    
    describe "Directional Movement" $ do
      it "correctly moves selection up" $ do
        let initialState = standardState 2025 5 15
        let newState = moveUp initialState
        calSelectedDay newState `shouldBe` Just (fromGregorian 2025 5 8)
        
      it "correctly moves selection down" $ do
        let initialState = standardState 2025 5 15
        let newState = moveDown initialState
        calSelectedDay newState `shouldBe` Just (fromGregorian 2025 5 22)
        
      it "correctly moves selection left" $ do
        let initialState = standardState 2025 5 15
        let newState = moveLeft initialState
        calSelectedDay newState `shouldBe` Just (fromGregorian 2025 5 14)
        
      it "correctly moves selection right" $ do
        let initialState = standardState 2025 5 15
        let newState = moveRight initialState
        calSelectedDay newState `shouldBe` Just (fromGregorian 2025 5 16)
    
    describe "Month Navigation" $ do
      it "correctly navigates to previous month" $ do
        let initialState = standardState 2025 5 15
        let newState = setMonthBefore initialState
        calMonth newState `shouldBe` 4
        calYear newState `shouldBe` 2025
        
      it "correctly navigates to next month" $ do
        let initialState = standardState 2025 5 15
        let newState = setMonthAfter initialState
        calMonth newState `shouldBe` 6
        calYear newState `shouldBe` 2025 

    describe "Year Navigation" $ do
      it "correctly navigates to previous year" $ do
        let initialState = standardState 2025 5 15
        let newState = setYearBefore initialState
        calMonth newState `shouldBe` 5
        calYear newState `shouldBe` 2024

      it "correctly navigates to next year" $ do
        let initialState = standardState 2025 5 15
        let newState = setYearAfter initialState
        calMonth newState `shouldBe` 5
        calYear newState `shouldBe` 2026
    
    describe "Edge Cases" $ do
      describe "Leap Year Handling" $ do
        it "correctly handles date selection in leap year" $ do
          let initialState = standardState 2024 2 28
          let newState = moveRight initialState
          calSelectedDay newState `shouldBe` Just (fromGregorian 2024 2 29)
          
        it "correctly handles date selection across leap year boundary" $ do
          let initialState = standardState 2024 2 29
          let newState = moveRight initialState
          calSelectedDay newState `shouldBe` Just (fromGregorian 2024 3 1)
          calMonth newState `shouldBe` 3
          
        it "correctly navigates from leap day to next month" $ do
          let initialState = standardState 2024 2 29
          let newState = setMonthAfter initialState
          calSelectedDay newState `shouldBe` Just (fromGregorian 2024 3 29)
          calMonth newState `shouldBe` 3
          calYear newState `shouldBe` 2024
          
        it "correctly navigates from leap day to next year (non-leap year)" $ do
          let initialState = standardState 2024 2 29
          let newState = setYearAfter initialState
          calSelectedDay newState `shouldBe` Just (fromGregorian 2025 2 28)
          calMonth newState `shouldBe` 2
          calYear newState `shouldBe` 2025
      
      describe "Year Boundary Handling" $ do
        it "correctly handles next month at year boundary" $ do
          let initialState = standardState 2025 12 15
          let newState = setMonthAfter initialState
          calMonth newState `shouldBe` 1
          calYear newState `shouldBe` 2026
          
        it "correctly handles previous month at year boundary" $ do
          let initialState = standardState 2025 1 15
          let newState = setMonthBefore initialState
          calMonth newState `shouldBe` 12
          calYear newState `shouldBe` 2024
          
        it "correctly handles next day at year boundary" $ do
          let initialState = standardState 2025 12 31
          let newState = moveRight initialState
          calSelectedDay newState `shouldBe` Just (fromGregorian 2026 1 1)
          calMonth newState `shouldBe` 1
          calYear newState `shouldBe` 2026
          
        it "correctly handles previous day at year boundary" $ do
          let initialState = standardState 2025 1 1
          let newState = moveLeft initialState
          calSelectedDay newState `shouldBe` Just (fromGregorian 2024 12 31)
          calMonth newState `shouldBe` 12
          calYear newState `shouldBe` 2024