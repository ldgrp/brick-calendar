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
-- Define a resource name type
data AppName = CalName CalendarResource
  deriving (Show, Eq, Ord)

-- Create a calendar state from a date
mkCalendarState :: Day -> CalendarState AppName
mkCalendarState day = 
  let (year, month, _) = toGregorian day
      config = defaultCalendarConfig
                { _weekStart = Monday
                , _dayLabelStyle = DistinctInitials
                , _showDayLabels = True
                , _outsideMonthDisplay = ShowDimmed
                }
  in CalendarState year month (Just day) config CalName

-- Render the calendar
drawUI :: AppState -> [Widget AppName]
drawUI s = [center $ border $ padAll 1 $ renderCalendar (calendar s)]

-- Handle calendar navigation events
handleEvent :: BrickEvent AppName e -> EventM AppName AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent e = 
  zoom calendarL $ handleCalendarEvent e
```

See `programs/SimpleDemo.hs` for a complete working example.
