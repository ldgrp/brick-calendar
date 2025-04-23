{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Widgets.Border
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Calendar
import qualified Graphics.Vty as V
import Data.Time
import System.IO
import Lens.Micro.Platform

data AppName = 
    Calendar1 CalendarResource   -- ^ Resources for first calendar
  | Calendar2 CalendarResource   -- ^ Resources for second calendar
  deriving (Show, Eq, Ord)

data ActiveCalendar = ActiveCalendar1 | ActiveCalendar2
  deriving (Eq)

-- | Application state with two calendars
data AppState = AppState
    { calendar1 :: CalendarState AppName  -- ^ Left calendar
    , calendar2 :: CalendarState AppName  -- ^ Right calendar
    , activeCalendar :: ActiveCalendar    -- ^ Currently active calendar
    }

calendar1L :: Lens' AppState (CalendarState AppName)
calendar1L = lens calendar1 (\s a -> s { calendar1 = a })

calendar2L :: Lens' AppState (CalendarState AppName)
calendar2L = lens calendar2 (\s a -> s { calendar2 = a })

activeCalendarL :: Lens' AppState ActiveCalendar
activeCalendarL = lens activeCalendar (\s a -> s { activeCalendar = a })

-- | Initialize a calendar state for the first (left) calendar
mkCalendar1 :: Day -> CalendarState AppName
mkCalendar1 day = 
  let (year, month, _) = toGregorian day
      config = defaultCalendarConfig
                { _showDayLabels = True
                , _dayLabelStyle = DistinctInitials
                , _outsideMonthDisplay = ShowDimmed
                , _weekStart = Monday
                }
  in CalendarState year month (Just day) config Calendar1

-- | Initialize a calendar state for the second (right) calendar
mkCalendar2 :: Day -> CalendarState AppName
mkCalendar2 day = 
  let (year, month, _) = toGregorian day
      config = defaultCalendarConfig
                { _showDayLabels = True
                , _dayLabelStyle = DoubleChar
                , _outsideMonthDisplay = ShowNormal
                , _weekStart = Sunday
                }
  in CalendarState year month (Just day) config Calendar2

-- | Main app definition
app :: App AppState e AppName
app = App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const defaultCalendarAttrMap
    }

-- | Draw the UI with both calendars side-by-side
drawUI :: AppState -> [Widget AppName]
drawUI s = 
  let 
      -- Apply highlight to the active calendar
      cal1Border = if activeCalendar s == ActiveCalendar1 
                   then withAttr (attrName "active") . border 
                   else withAttr (attrName "inactive") . border
      
      cal2Border = if activeCalendar s == ActiveCalendar2 
                   then withAttr (attrName "active") . border 
                   else withAttr (attrName "inactive") . border
      
      cal1Widget = cal1Border $ padAll 1 $ renderCalendar (calendar1 s)
      cal2Widget = cal2Border $ padAll 1 $ renderCalendar (calendar2 s)
      
      -- The main layout
      mainLayout = vBox 
                   [ C.hCenter $ str "Tab to switch focus"
                   , hBorder
                   ,C.hCenter $ hBox [C.center cal1Widget, vBorder, C.center cal2Widget]
                   ]
  in [C.center mainLayout]

-- | Handle events for both calendars
handleEvent :: BrickEvent AppName e -> EventM AppName AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt

-- Tab key switches focus between calendars
handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  modify $ \s -> s { activeCalendar = if activeCalendar s == ActiveCalendar1 then ActiveCalendar2 else ActiveCalendar1 }

-- Handle calendar events for the active calendar
handleEvent e = do
  active <- use activeCalendarL
  case active of
    ActiveCalendar1 -> zoom calendar1L $ handleCalendarEvent e
    ActiveCalendar2 -> zoom calendar2L $ handleCalendarEvent e

-- | Main function
main :: IO ()
main = do
    -- Ensure unicode support
    hSetEncoding stdout utf8
    
    -- Get today's date and one month later for the two calendars
    today <- utctDay <$> getCurrentTime
    let nextMonth = addGregorianMonthsClip 1 today
    
    -- Create initial state with two calendars
    let initialState = AppState 
                      { calendar1 = mkCalendar1 today
                      , calendar2 = mkCalendar2 nextMonth
                      , activeCalendar = ActiveCalendar1  -- First calendar is active initially
                      }
    
    -- Run the app
    finalState <- defaultMain app initialState
    
    -- Print the selected dates when the app exits
    putStrLn "Calendar 1 selected date: " 
    case calSelectedDay $ calendar1 finalState of
        Just day -> putStrLn $ showGregorian day
        Nothing -> putStrLn "No date selected"
        
    putStrLn "Calendar 2 selected date: "
    case calSelectedDay $ calendar2 finalState of
        Just day -> putStrLn $ showGregorian day
        Nothing -> putStrLn "No date selected" 