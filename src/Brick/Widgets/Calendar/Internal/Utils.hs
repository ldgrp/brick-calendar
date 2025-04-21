{-# LANGUAGE OverloadedStrings #-}

module Brick.Widgets.Calendar.Internal.Utils
  ( -- * Date helpers
    getFirstDayOfMonth
  , getDayLabel
  , getMonthLabel
  , getWeekDayLabels
  
    -- * Date formatting
  , formatDate
  , formatDayNumber

    -- * List helpers
  , chunksOf
  ) where

import Data.Time
import Data.Time.Calendar.Month
import Data.Text (Text)
import qualified Data.Text as T
import Brick.Widgets.Calendar.Internal.Core
import Lens.Micro
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

-- | Split a list into chunks of a given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Get the day of week for the first day of the given month
getFirstDayOfMonth :: Integer -> Int -> DayOfWeek
getFirstDayOfMonth year month = 
  dayOfWeek $ fromGregorian year month 1

-- | Get the label for a day based on the day of week and label style
getDayLabel :: DayLabelStyle -> DayOfWeek -> Text
getDayLabel style dow = case (style, dow) of
  (SingleChar, Monday)    -> "M "
  (SingleChar, Tuesday)   -> "T "
  (SingleChar, Wednesday) -> "W "
  (SingleChar, Thursday)  -> "T "
  (SingleChar, Friday)    -> "F "
  (SingleChar, Saturday)  -> "S "
  (SingleChar, Sunday)    -> "S "
  
  (DoubleChar, Monday)    -> "Mo"
  (DoubleChar, Tuesday)   -> "Tu"
  (DoubleChar, Wednesday) -> "We"
  (DoubleChar, Thursday)  -> "Th"
  (DoubleChar, Friday)    -> "Fr"
  (DoubleChar, Saturday)  -> "Sa"
  (DoubleChar, Sunday)    -> "Su"
  
  (DistinctInitials, Monday)     -> "M "
  (DistinctInitials, Tuesday)    -> "T "
  (DistinctInitials, Wednesday)  -> "W "
  (DistinctInitials, Thursday)   -> "Th"
  (DistinctInitials, Friday)     -> "F "
  (DistinctInitials, Saturday)   -> "S "
  (DistinctInitials, Sunday)     -> "Su"

-- | Get the month label as text (e.g., "Apr 2025")
getMonthLabel :: CalendarConfig -> Integer -> Int -> Text
getMonthLabel config year month =
  let m = YearMonth year month
      day = MonthDay m 1
      fmt = case config ^. dateFormat of
              DefaultFormat -> "%b %Y"
              CustomFormat{headerFormat=f} -> f
  in T.pack $ formatTime defaultTimeLocale fmt day

-- | Get the labels for all days of the week based on configuration
getWeekDayLabels :: CalendarConfig -> [Text]
getWeekDayLabels config =
  let startDay = config ^. weekStart
      style = config ^. dayLabelStyle
      allDays = [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]
      startIdx = fromMaybe 0 $ elemIndex startDay allDays
      -- Rotate the list so startDay is first
      weekDays = drop startIdx allDays ++ take startIdx allDays
  in map (getDayLabel style) weekDays

-- | Format a date as text using the configured format
formatDate :: CalendarConfig -> Day -> Text
formatDate config day = 
  let fmt = case config ^. dateFormat of
              DefaultFormat -> "%Y-%m-%d"
              CustomFormat{dayFormat=f} -> f
  in T.pack $ formatTime defaultTimeLocale fmt day

-- | Format a day number based on configuration (for calendar display)
formatDayNumber :: CalendarConfig -> Day -> Text
formatDayNumber config day =
  let (_, _, d) = toGregorian day
      fmt = case config ^. dateFormat of
              DefaultFormat -> T.justifyLeft 2 ' ' (T.pack $ show d)
              CustomFormat{dayFormat=f} -> T.pack $ formatTime defaultTimeLocale f day
  in fmt 