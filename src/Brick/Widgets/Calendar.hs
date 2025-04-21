{-# LANGUAGE OverloadedStrings #-}

-- | This module provides a month calendar widget for Brick applications.
--
-- == Usage
--
-- The widget provides:
--
-- * A render function to display the calendar
-- * A set of actions for navigation (moveUp, moveDown, etc.)
-- * Configuration options
--
-- Applications should connect the provided actions to keyboard events in their event handlers.
-- Common navigation patterns include:
--
-- * Arrow keys: Use moveUp, moveDown, moveLeft, moveRight to navigate between days
-- * Month navigation: Use setMonthBefore, setMonthAfter to change months
-- * Year navigation: Use setYearBefore, setYearAfter to change years
--
-- When a navigation action is applied and no day is selected, the first day of the current month will be selected.
-- When navigating between days, if moving to a date outside the current month, the view will automatically
-- shift to that month.
module Brick.Widgets.Calendar
  ( -- * Render
    renderCalendar
  , CalendarState(..)

    -- * Actions
  , moveUp
  , moveDown
  , moveLeft
  , moveRight
  , setMonthBefore
  , setMonthAfter
  , setYearBefore
  , setYearAfter

  -- * Configuration
  , CalendarConfig(..)
  , DayLabelStyle(..)
  , OutsideMonthDisplay(..)
  , defaultCalendarConfig

    -- * Lenses
  , weekStart
  , dayLabelStyle
  , showDayLabels
  , outsideMonthDisplay


    -- * Resource name
  , CalendarResource(..)

    -- * Utilities
  , getFirstDayOfMonth
  , getDayLabel
  , getMonthLabel
  , getWeekDayLabels
  , formatDate
  , defaultCalendarAttrMap
  ) where

import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import Brick.Util (fg)

import Brick.Widgets.Calendar.Internal.Actions
import Brick.Widgets.Calendar.Internal.Core
import Brick.Widgets.Calendar.Internal.Month
import Brick.Widgets.Calendar.Internal.Utils

-- | Attribute map for calendar widgets.
defaultCalendarAttrMap :: A.AttrMap
defaultCalendarAttrMap = A.attrMap V.defAttr
  [ (A.attrName "calendar.day", fg V.white)
  , (A.attrName "calendar.day" <> A.attrName "selected", fg V.black `V.withBackColor` V.blue)
  , (A.attrName "calendar.outsideMonth", fg V.brightBlack)
  , (A.attrName "calendar.outsideMonth" <> A.attrName "selected", fg V.black `V.withBackColor` V.blue)
  , (A.attrName "calendar.hidden", V.currentAttr)
  , (A.attrName "calendar.nav", fg V.blue)
  , (A.attrName "calendar.header", V.defAttr `V.withStyle` V.bold)
  ] 