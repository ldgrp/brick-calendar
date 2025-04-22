# Brick Calendar

![A screenshot of the brick calendar widget](./docs/image.png)

A calendar widget for [Brick](https://github.com/jtdaugherty/brick) terminal user interfaces.

## Features

- Configurable first day of week (Sunday, Monday, etc.)
- Configurable day-of-week label
  - Single char (S, M, T, W, T, F, S)
  - Double char (Su, Mo, Tu, We, Th, Fr, Sa)
  - Distinct initials (Su, M, T, W, Th, F, S)
  - Hidden
- Option to show/hide/dim days outside the current month
- Easy integration with existing Brick applications

## Installation

```
cabal install brick-calendar
```

## Usage

```haskell

-- 1. Create a calendar state from a date
mkCalendarState :: Day -> CalendarState
mkCalendarState day = 
  let (year, month, _) = toGregorian day
      config = defaultCalendarConfig
                { _weekStart = Monday
                , _dayLabelStyle = DistinctInitials
                , _showDayLabels = True
                , _outsideMonthDisplay = ShowDimmed
                }
  in CalendarState year month (Just day) config
  -- CalendarState takes:
  --   year: Integer - The year to display
  --   month: Int - The month to display (1-12)
  --   selected day: Maybe Day - Currently selected day (if any)
  --   config: CalendarConfig - Display configuration

-- 2. Render the calendar in your drawing function
-- renderCalendar returns a Widget that you can incorporate
-- into your UI layout like any other Brick widget
myDrawUI :: CalendarState -> [Widget n]
myDrawUI calState = 
  [center $ border $ renderCalendar calState]

-- 3. Handle navigation events in your event handler
handleCalendarEvent :: BrickEvent n e -> EventM n CalendarState ()
handleCalendarEvent = \case
  -- Navigate between days
  VtyEvent (V.EvKey V.KUp [])      -> modify moveUp
  VtyEvent (V.EvKey V.KDown [])    -> modify moveDown
  VtyEvent (V.EvKey V.KLeft [])    -> modify moveLeft
  VtyEvent (V.EvKey V.KRight [])   -> modify moveRight
  
  -- Navigate between months
  VtyEvent (V.EvKey (V.KChar '[') []) -> modify setMonthBefore
  VtyEvent (V.EvKey (V.KChar ']') []) -> modify setMonthAfter
  
  -- Navigate between years
  VtyEvent (V.EvKey (V.KChar '{') []) -> modify setYearBefore
  VtyEvent (V.EvKey (V.KChar '}') []) -> modify setYearAfter
  
  _ -> return ()

-- 4. Get the selected date
case calSelectedDay calState of
  Just day -> showGregorian day
  Nothing  -> "No date selected"
```

See `programs/SimpleDemo.hs` for a minimal working example.
