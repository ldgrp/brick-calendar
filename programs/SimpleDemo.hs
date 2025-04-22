{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Widgets.Border
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Calendar
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import Data.Time
import Control.Monad
import System.IO

-- App state with calendar
newtype AppState = AppState
    { calendar :: CalendarState
    }

-- Initialize the app state
mkCalendarState :: Day -> CalendarState
mkCalendarState day = 
  let (year, month, _) = toGregorian day
      config = defaultCalendarConfig
                { _showDayLabels = True
                , _dayLabelStyle = DistinctInitials
                , _outsideMonthDisplay = ShowDimmed
                , _weekStart = Sunday
                }
  in CalendarState year month (Just day) config

-- Main app definition
app :: App AppState e CalendarResource
app = App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const defaultCalendarAttrMap
    }

-- Draw the UI
drawUI :: AppState -> [Widget CalendarResource]
drawUI s = [C.center $ border $ padAll 1 $ renderCalendar (calendar s)]

-- Handle events
handleEvent :: BrickEvent CalendarResource e -> EventM CalendarResource AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KUp [])) = 
    modify $ \s -> s { calendar = moveUp (calendar s) }
handleEvent (VtyEvent (V.EvKey V.KDown [])) = 
    modify $ \s -> s { calendar = moveDown (calendar s) }
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = 
    modify $ \s -> s { calendar = moveLeft (calendar s) }
handleEvent (VtyEvent (V.EvKey V.KRight [])) = 
    modify $ \s -> s { calendar = moveRight (calendar s) }
handleEvent (VtyEvent (V.EvKey (V.KChar '[') [])) = 
    modify $ \s -> s { calendar = setMonthBefore (calendar s) }
handleEvent (VtyEvent (V.EvKey (V.KChar ']') [])) = 
    modify $ \s -> s { calendar = setMonthAfter (calendar s) }
handleEvent (VtyEvent (V.EvKey (V.KChar '{') [])) = 
    modify $ \s -> s { calendar = setYearBefore (calendar s) }
handleEvent (VtyEvent (V.EvKey (V.KChar '}') [])) = 
    modify $ \s -> s { calendar = setYearAfter (calendar s) }
handleEvent _ = return ()

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