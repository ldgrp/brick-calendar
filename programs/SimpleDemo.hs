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

data AppName = CalName CalendarResource
  deriving (Show, Eq, Ord)

calendarL :: Lens' AppState (CalendarState AppName)
calendarL = lens calendar (\s a -> s { calendar = a })

-- | App state with calendar
newtype AppState = AppState
    { calendar :: CalendarState AppName
    }

-- Initialize the app state
mkCalendarState :: Day -> CalendarState AppName
mkCalendarState day = 
  let (year, month, _) = toGregorian day
      config = defaultCalendarConfig
                { _showDayLabels = True
                , _dayLabelStyle = DistinctInitials
                , _outsideMonthDisplay = ShowDimmed
                , _weekStart = Sunday
                }
  in CalendarState year month (Just day) config CalName

-- Main app definition
app :: App AppState e AppName
app = App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const defaultCalendarAttrMap
    }

-- Draw the UI
drawUI :: AppState -> [Widget AppName]
drawUI s = [C.center $ border $ padAll 1 $ renderCalendar (calendar s)]

-- | Handle events
handleEvent :: BrickEvent AppName e -> EventM AppName AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt

-- Use the built-in calendar event handler for navigation
handleEvent e = do
  zoom calendarL $ handleCalendarEvent e

-- Main function
main :: IO ()
main = do
    -- Ensure unicode support
    hSetEncoding stdout utf8
    
    -- Get today's date and create initial state
    today <- utctDay <$> getCurrentTime
    let initialState = AppState { calendar = mkCalendarState today }
    
    -- Run the app
    finalState <- defaultMain app initialState
    
    -- Print the selected date when the app exits
    case calSelectedDay $ calendar finalState of
        Just day -> putStrLn $ "Selected date: " ++ showGregorian day
        Nothing -> putStrLn "No date selected"