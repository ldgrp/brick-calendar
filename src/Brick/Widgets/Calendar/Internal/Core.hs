{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Brick.Widgets.Calendar.Internal.Core
  ( -- * Types
    CalendarConfig(..)
  , DayLabelStyle(..)
  , OutsideMonthDisplay(..)
  , DateFormat(..)
  , CalendarState(..)
  , defaultCalendarConfig

    -- * Lenses
  , weekStart
  , dayLabelStyle
  , showDayLabels
  , outsideMonthDisplay
  , dateFormat

    -- * Resource name
  , CalendarResource(..)
  ) where

import Data.Time (DayOfWeek(..), Day)
import Lens.Micro.TH ( makeLenses )

-- | Style for displaying day labels.
data DayLabelStyle = 
    SingleChar      -- ^ Use single characters: S M T W T F S
  | DoubleChar      -- ^ Use two characters: Su Mo Tu We Th Fr Sa
  | DistinctInitials       -- ^ Use single chars with Th for Thursday: S M T W Th F S
  deriving (Show, Eq)

-- | How to display days outside the current month.
data OutsideMonthDisplay =
    Hide            -- ^ Don't show days outside current month
  | ShowDimmed      -- ^ Show days outside month with dimmed styling
  | ShowNormal      -- ^ Show days outside month with normal styling
  deriving (Show, Eq)

-- | Format options for displaying dates in the calendar.
data DateFormat =
    DefaultFormat   -- ^ Use default format (%b %Y for month header, digits for days)
  | CustomFormat    
    { headerFormat :: String  -- ^ Format string for month/year header (e.g., "%B %Y")
    , dayFormat :: String     -- ^ Format string for days (e.g., "%d")
    } -- ^ Use custom format strings from Data.Time.Format (%a, %d, etc.)
  deriving (Show, Eq)

-- | Configuration for the calendar widget.
data CalendarConfig = CalendarConfig
  { _weekStart :: DayOfWeek                -- ^ Day of week to start the calendar
  , _dayLabelStyle :: DayLabelStyle        -- ^ Style for day labels
  , _showDayLabels :: Bool                 -- ^ Whether to show day labels at all
  , _outsideMonthDisplay :: OutsideMonthDisplay -- ^ How to display days outside current month
  , _dateFormat :: DateFormat              -- ^ Format to use for dates
  } deriving (Show, Eq)

-- | Default calendar configuration.
defaultCalendarConfig :: CalendarConfig
defaultCalendarConfig = CalendarConfig
  { _weekStart = Sunday
  , _dayLabelStyle = SingleChar
  , _showDayLabels = True
  , _outsideMonthDisplay = ShowDimmed
  , _dateFormat = DefaultFormat
  }

-- | Resource name for calendar widget elements.
data CalendarResource =
    CalendarDay Int Int Int   -- ^ Resource for a day (year, month, day)
  | CalendarMonth Int Int     -- ^ Resource for month header (year, month)
  | CalendarPrev              -- ^ Resource for previous month button
  | CalendarNext              -- ^ Resource for next month button
  deriving (Show, Eq, Ord)

-- | The state of the calendar widget. Make this part of your application state.
data CalendarState n = CalendarState
  { calYear :: Integer                     -- ^ Current year
  , calMonth :: Int                        -- ^ Current month (1-12)
  , calSelectedDay :: Maybe Day            -- ^ Currently selected day, if any
  , calConfig :: CalendarConfig            -- ^ Calendar configuration
  , calendarName :: CalendarResource -> n  -- ^ Constructor for wrapping calendar resources in the application's resource name type
  }

makeLenses ''CalendarConfig 