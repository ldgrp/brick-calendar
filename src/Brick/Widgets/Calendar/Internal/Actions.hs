{-# LANGUAGE LambdaCase #-}

module Brick.Widgets.Calendar.Internal.Actions
  ( -- * Navigation actions
    moveUp
  , moveDown
  , moveLeft
  , moveRight
  , setMonthBefore
  , setMonthAfter
  , setYearBefore
  , setYearAfter
    -- * Event handler for common calendar navigation
    -- You can also use the individual actions above to create your own custom event handler
  , handleCalendarEvent
  ) where

import Data.Time
import Data.Time.Calendar.Month
import Brick ( modify, EventM, BrickEvent(..) )
import qualified Graphics.Vty as V

import Brick.Widgets.Calendar.Internal.Core

moveUp :: CalendarState n -> CalendarState n
moveUp =
  navigateSelection (addDays (-7))

moveDown :: CalendarState n -> CalendarState n
moveDown =
  navigateSelection (addDays 7)

moveLeft :: CalendarState n -> CalendarState n
moveLeft =
  navigateSelection (addDays (-1))

moveRight :: CalendarState n -> CalendarState n
moveRight =
  navigateSelection (addDays 1)

setMonthBefore :: CalendarState n -> CalendarState n
setMonthBefore =
  navigateMonth (addMonths (-1))

setMonthAfter :: CalendarState n -> CalendarState n
setMonthAfter =
  navigateMonth (addMonths 1)

setYearBefore :: CalendarState n -> CalendarState n
setYearBefore =
  navigateMonth (addMonths (-12))

setYearAfter :: CalendarState n -> CalendarState n
setYearAfter =
  navigateMonth (addMonths 12)

navigateSelection :: (Day -> Day) -> CalendarState n -> CalendarState n
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

navigateMonth :: (Month -> Month) -> CalendarState n -> CalendarState n
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

handleCalendarEvent :: BrickEvent n e -> EventM n (CalendarState n) ()
handleCalendarEvent = \case
  -- Navigate between days
  VtyEvent (V.EvKey V.KUp [])      -> modify moveUp
  VtyEvent (V.EvKey V.KDown [])    -> modify moveDown
  VtyEvent (V.EvKey V.KLeft [])    -> modify moveLeft
  VtyEvent (V.EvKey V.KRight [])   -> modify moveRight
  VtyEvent (V.EvKey (V.KChar 'h') []) -> modify moveLeft
  VtyEvent (V.EvKey (V.KChar 'l') []) -> modify moveRight
  VtyEvent (V.EvKey (V.KChar 'j') []) -> modify moveDown
  VtyEvent (V.EvKey (V.KChar 'k') []) -> modify moveUp
  
  -- Navigate between months
  VtyEvent (V.EvKey (V.KChar '[') []) -> modify setMonthBefore
  VtyEvent (V.EvKey (V.KChar ']') []) -> modify setMonthAfter
  VtyEvent (V.EvKey (V.KChar 'H') []) -> modify setMonthBefore
  VtyEvent (V.EvKey (V.KChar 'L') []) -> modify setMonthAfter
  
  -- Navigate between years
  VtyEvent (V.EvKey (V.KChar '{') []) -> modify setYearBefore
  VtyEvent (V.EvKey (V.KChar '}') []) -> modify setYearAfter
  VtyEvent (V.EvKey (V.KChar 'J') []) -> modify setYearBefore
  VtyEvent (V.EvKey (V.KChar 'K') []) -> modify setYearAfter

  _ -> return ()