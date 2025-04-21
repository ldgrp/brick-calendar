{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Brick.Widgets.Calendar.Internal.Month
  (
    renderCalendar
  ) where

import Brick
import Brick.Widgets.Center

import Data.Time
import Data.Time.Calendar.Month
import Lens.Micro

import Brick.Widgets.Calendar.Internal.Core
import Brick.Widgets.Calendar.Internal.Utils

-- | Render a month calendar widget
renderCalendar :: CalendarState -> Widget CalendarResource
renderCalendar state@CalendarState{..} =
  vBox [ renderHeader state
       , if calConfig ^. showDayLabels
         then renderDayLabels calConfig
         else emptyWidget
       , renderDays calConfig calYear calMonth calSelectedDay
       ]

-- | Render the calendar header with month/year and navigation buttons
renderHeader :: CalendarState -> Widget CalendarResource
renderHeader CalendarState{..} =
  let monthText = getMonthLabel calConfig calYear calMonth
      prevButton = clickable CalendarPrev $ 
                   withAttr (attrName "calendar.nav") $ 
                   str " << "
      monthLabel = clickable (CalendarMonth (fromIntegral calYear) calMonth) $ 
                   txt monthText
      nextButton = clickable CalendarNext $ 
                   withAttr (attrName "calendar.nav") $ 
                   str " >> "
  in hLimit 20 $ hCenter $ hBox [prevButton, monthLabel, nextButton]

-- | Render the day labels (S M T W T F S)
renderDayLabels :: CalendarConfig -> Widget CalendarResource
renderDayLabels config =
  let labels = getWeekDayLabels config
      -- Create evenly spaced day labels using padRight
      paddedLabels = init labels `zip` repeat (Pad 1) ++ [(last labels, Pad 0)]
      makeLabel (l, p) = padRight p $ 
                         withAttr (attrName "calendar.dayLabel") $
                         txt l
  in hBox $ map makeLabel paddedLabels

-- | Render the days of the month
renderDays :: CalendarConfig -> Integer -> Int -> Maybe Day -> Widget CalendarResource
renderDays config year month selectedDay =
  let yearMonth = YearMonth year month
      daysInMonth = periodLength yearMonth
      firstDay = getFirstDayOfMonth year month
      
      -- Get days of previous month that need to be displayed
      prevYearMonth = addMonths (-1) yearMonth
      YearMonth prevYear prevMonth = prevYearMonth
      prevMonthDays = periodLength prevYearMonth
      
      -- Calculate start day offset more elegantly using modular arithmetic
      -- This avoids nested case statements
      firstDayInt = fromEnum firstDay
      weekStartInt = fromEnum $ config ^. weekStart
      startDayNum = (firstDayInt - weekStartInt) `mod` 7
      
      -- Days from previous month to display
      prevDays = 
        if startDayNum > 0
        then map (\d -> (prevYear, prevMonth, d, True)) 
             [prevMonthDays - startDayNum + 1 .. prevMonthDays]
        else []
      
      -- Days from current month
      currentDays = map (\d -> (year, month, d, False)) [1..daysInMonth]
      
      -- Calculate extra days needed from next month
      nextYearMonth = addMonths 1 yearMonth
      YearMonth nextYear nextMonth = nextYearMonth
      
      -- Calculate days needed for a standard 6-row calendar (42 days total)
      -- Direct calculation without intermediate variables
      totalDays = length prevDays + length currentDays
      daysNeeded = 42 - totalDays  -- 6 rows × 7 days
      
      -- Create next month days to fill exactly 6 rows
      nextDays = map (\d -> (nextYear, nextMonth, d, True)) [1..daysNeeded]
      
      -- All days to display
      allDays = prevDays ++ currentDays ++ nextDays
      
      -- Chunked into weeks (always 6 weeks)
      weeks = chunksOf 7 allDays
      
      -- Render a single day with appropriate padding
      renderDay (dayInfo, isLast) =
        let (y, m, d, isOutside) = dayInfo
            day = fromGregorian y m d
            isSelected = maybe False (== day) selectedDay
            
            -- Decide attribute based on whether day is outside current month
            attr = if isOutside 
                   then case config ^. outsideMonthDisplay of
                          Hide -> attrName "calendar.hidden"
                          ShowDimmed -> attrName "calendar.outsideMonth"
                          ShowNormal -> attrName "calendar.day"
                   else attrName "calendar.day"
                   
            -- Add selected attribute if day is selected
            finalAttr = if isSelected 
                        then attr <> attrName "selected"
                        else attr
                        
            -- Format day as text
            dayText = if isOutside && config ^. outsideMonthDisplay == Hide
                      then "  "  -- Two spaces for hidden days
                      else formatDayNumber config day
                      
            -- Create clickable day widget with appropriate resource identifier
            baseDayWidget = clickable (CalendarDay (fromIntegral y) m d) $
                           hLimit 3 $ withAttr finalAttr (txt dayText)
                           
            -- Add padding except for the last item in each row
            dayWidget = if isLast
                        then baseDayWidget
                        else padRight (Pad 1) baseDayWidget
        in dayWidget
      
      -- Render a week by adding padding between days but not after the last one
      renderWeek days = 
        let daysWithIsLast = zip days (replicate 6 False ++ [True])
        in hBox $ map renderDay daysWithIsLast
      
  in vBox $ map renderWeek weeks