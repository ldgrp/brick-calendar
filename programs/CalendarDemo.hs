{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Widgets.Core
import Brick.Widgets.Border
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Calendar
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import Data.Time
import Control.Monad
import System.IO
import Lens.Micro.Platform

-- | Data type to identify our widgets
data Name = Calendar1
    deriving (Eq, Ord, Show)

-- | App state
newtype AppState = AppState
    { calendar :: CalendarState
    }

-- | Lens for the calendar field of AppState
calendarL :: Lens' AppState CalendarState
calendarL = lens calendar (\s c -> s { calendar = c })

-- | Main app definition
app :: App AppState e CalendarResource
app = App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const defaultCalendarAttrMap
    }

-- | Initialize the app state
initialState :: IO AppState
initialState = do
    today <- utctDay <$> getCurrentTime
    let (year, month, _) = toGregorian today
    let calConfig = defaultCalendarConfig
                    { _showDayLabels = True
                    , _dayLabelStyle = DistinctInitials
                    , _outsideMonthDisplay = ShowDimmed
                    , _weekStart = Sunday
                    }
    return AppState
        { calendar = CalendarState year month (Just today) calConfig
        }

-- | Draw the UI
drawUI :: AppState -> [Widget CalendarResource]
drawUI s = [C.center ((border (padAll 1 ui) <=> config) <+> hLimit 40 help)]
  where
    config = vBox
        [
            displayConfig (calendar s)
        ]
    ui =
        padAll 1 $ renderCalendar (calendar s)
    help = vBox
        [ str "Controls"
        , hBorder
        , str "Navigation:"
        , str "arrow keys / hjkl - move selection"
        , str "[] / HL - previous / next month"
        , str "{} / JK - previous / next year"
        , hBorder
        , str "Settings:"
        , str "d - toggle day labels"
        , str "t - cycle label style"
        , str "o - cycle outside days"
        , str "w - toggle week start"
        , hBorder
        , str "Actions:"
        , str "Enter / Esc / q - select date and exit"
        ]

-- | Display the current configuration
displayConfig :: CalendarState -> Widget n
displayConfig s =
    let config = calConfig s
        weekStartText = show (config ^. weekStart)
        labelStyleText = case config ^. dayLabelStyle of
                            SingleChar -> "Single char"
                            DoubleChar -> "Double char"
                            DistinctInitials -> "Thursday as Th"
        showLabelsText = if config ^. showDayLabels then "Show" else "Hide"
        outsideText = case config ^. outsideMonthDisplay of
                         Hide -> "Hide"
                         ShowDimmed -> "Show dimmed"
                         ShowNormal -> "Show normal"
        selectedText = case calSelectedDay s of
                         Nothing -> "None"
                         Just day -> showGregorian day
    in withAttr (attrName "config") $
       vBox [ str $ "Week starts on: " ++ weekStartText
            , str $ "Day label style: " ++ labelStyleText
            , str $ "Day labels: " ++ showLabelsText
            , str $ "Outside month days: " ++ outsideText
            , str $ "Selected day: " ++ selectedText
            ]

-- | Handle events
handleEvent :: BrickEvent CalendarResource e -> EventM CalendarResource AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = halt

-- Toggle day labels
handleEvent (VtyEvent (V.EvKey (V.KChar 'd') [])) = do
    zoom calendarL $ do
        cal <- get
        let config = calConfig cal
        modify $ \s -> s { calConfig = config & showDayLabels %~ not }

-- Cycle day label style
handleEvent (VtyEvent (V.EvKey (V.KChar 't') [])) = do
    zoom calendarL $ do
        cal <- get
        let config = calConfig cal
            newStyle = case config ^. dayLabelStyle of
                         SingleChar -> DoubleChar
                         DoubleChar -> DistinctInitials
                         DistinctInitials -> SingleChar
        modify $ \s -> s { calConfig = config & dayLabelStyle .~ newStyle }

-- Cycle outside month display
handleEvent (VtyEvent (V.EvKey (V.KChar 'o') [])) = do
    zoom calendarL $ do
        cal <- get
        let config = calConfig cal
            newOutside = case config ^. outsideMonthDisplay of
                           Hide -> ShowDimmed
                           ShowDimmed -> ShowNormal
                           ShowNormal -> Hide
        modify $ \s -> s { calConfig = config & outsideMonthDisplay .~ newOutside }

-- Toggle week start
handleEvent (VtyEvent (V.EvKey (V.KChar 'w') [])) = do
    zoom calendarL $ do
        cal <- get
        let config = calConfig cal
            newStart = case config ^. weekStart of
                         Sunday -> Monday
                         Monday -> Sunday
                         _ -> error "Not implemented"
        modify $ \s -> s { calConfig = config & weekStart .~ newStart }

-- Handle calendar keyboard events
handleEvent (VtyEvent (V.EvKey V.KUp [])) = calendarL %= moveUp
handleEvent (VtyEvent (V.EvKey V.KDown [])) = calendarL %= moveDown
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = calendarL %= moveLeft
handleEvent (VtyEvent (V.EvKey V.KRight [])) = calendarL %= moveRight
handleEvent (VtyEvent (V.EvKey (V.KChar '[') [])) = calendarL %= setMonthBefore
handleEvent (VtyEvent (V.EvKey (V.KChar ']') [])) = calendarL %= setMonthAfter
handleEvent (VtyEvent (V.EvKey (V.KChar '{') [])) = calendarL %= setYearBefore
handleEvent (VtyEvent (V.EvKey (V.KChar '}') [])) = calendarL %= setYearAfter

-- Handle vim-like navigation
handleEvent (VtyEvent (V.EvKey (V.KChar 'h') [])) = calendarL %= moveLeft
handleEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = calendarL %= moveDown
handleEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = calendarL %= moveUp
handleEvent (VtyEvent (V.EvKey (V.KChar 'l') [])) = calendarL %= moveRight
handleEvent (VtyEvent (V.EvKey (V.KChar 'H') [])) = calendarL %= setMonthBefore
handleEvent (VtyEvent (V.EvKey (V.KChar 'L') [])) = calendarL %= setMonthAfter
handleEvent (VtyEvent (V.EvKey (V.KChar 'J') [])) = calendarL %= setYearBefore
handleEvent (VtyEvent (V.EvKey (V.KChar 'K') [])) = calendarL %= setYearAfter
handleEvent _ = return ()

-- | Main function
main :: IO ()
main = do
    -- Ensure unicode support
    hSetEncoding stdout utf8

    -- Run the app
    s <- initialState
    finalState <- defaultMain app s
    case calSelectedDay $ calendar finalState of
        Just day -> putStrLn $ "Selected date: " ++ showGregorian day
        Nothing -> putStrLn "No date selected"