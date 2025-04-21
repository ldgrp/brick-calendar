module Brick.Widgets.Calendar.Internal.Actions
  ( moveUp
  , moveDown
  , moveLeft
  , moveRight
  , setMonthBefore
  , setMonthAfter
  , setYearBefore
  , setYearAfter
  ) where

import Data.Time
import Data.Time.Calendar.Month

import Brick.Widgets.Calendar.Internal.Core

moveUp :: CalendarState -> CalendarState
moveUp =
  navigateSelection (addDays (-7))

moveDown :: CalendarState -> CalendarState
moveDown =
  navigateSelection (addDays 7)

moveLeft :: CalendarState -> CalendarState
moveLeft =
  navigateSelection (addDays (-1))

moveRight :: CalendarState -> CalendarState
moveRight =
  navigateSelection (addDays 1)

setMonthBefore :: CalendarState -> CalendarState
setMonthBefore =
  navigateMonth (addMonths (-1))

setMonthAfter :: CalendarState -> CalendarState
setMonthAfter =
  navigateMonth (addMonths 1)

setYearBefore :: CalendarState -> CalendarState
setYearBefore =
  navigateMonth (addMonths (-12))

setYearAfter :: CalendarState -> CalendarState
setYearAfter =
  navigateMonth (addMonths 12)

navigateSelection :: (Day -> Day) -> CalendarState -> CalendarState
navigateSelection dayTransform s =
  case calSelectedDay s of
    Nothing -> 
      -- When no day is selected, select the first day of the current month
      s { calSelectedDay = Just $ fromGregorian (calYear s) (calMonth s) 1 }
    Just day ->
      let newDay = dayTransform day
          (y, m, _) = toGregorian newDay
      in if m /= calMonth s || y /= calYear s
         -- If we've moved to a different month, update the view
         then s { calYear = y, calMonth = m, calSelectedDay = Just newDay }
         -- Otherwise just update the selected day
         else s { calSelectedDay = Just newDay }

navigateMonth :: (Month -> Month) -> CalendarState -> CalendarState
navigateMonth monthTransform s = 
  let currentYM = YearMonth (calYear s) (calMonth s)
      newYM = monthTransform currentYM
      YearMonth newYear newMonth = newYM
      
      -- Keep the same day if possible in the new month
      newDay = case calSelectedDay s of
        Nothing -> Nothing
        Just day -> 
          let (_, _, d) = toGregorian day
              lastDayInNewMonth = periodLength newYM
              adjustedDay = min d lastDayInNewMonth
          in Just $ fromGregorian newYear newMonth adjustedDay
          
  in s { calYear = newYear, calMonth = newMonth, calSelectedDay = newDay }