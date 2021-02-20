{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Foldable (traverse_)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import Text.Read hiding (lift)
import GI.Gtk.Objects.Widget (Widget)
import System.Log.Logger
  ( Priority (DEBUG),
    getLogger,
    saveGlobalLogger,
    setLevel,
  )
import System.Taffybar.Widget.Workspaces
import System.Taffybar.Util
import System.Taffybar (startTaffybar)
import System.Taffybar.Widget.SimpleClock(ClockConfig(..),ClockUpdateStrategy(..))
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Example (exampleTaffybarConfig)
import System.Taffybar.Information.CPU (cpuLoad)
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig (SimpleTaffyConfig (endWidgets), barHeight, defaultSimpleTaffyConfig, startWidgets, toTaffyConfig)
import System.Taffybar.Widget
import System.Taffybar.Widget.Battery 
import System.Taffybar.Widget.CommandRunner (commandRunnerNew)
import System.Taffybar.Widget.Generic.Graph
  ( GraphConfig (..),
    defaultGraphConfig,
    graphLabel,
  )
import System.Taffybar.Widget.Generic.PollingGraph (pollingGraphNew)
import System.Taffybar.Widget.Layout
  ( defaultLayoutConfig,
    layoutNew,
  )

main :: IO ()
main = do
  -- enableDebugLogging
  -- startTaffybar $ toTaffyConfig cfg

  startTaffybar exampleTaffybarConfig

cfg :: SimpleTaffyConfig
cfg = do
  defaultSimpleTaffyConfig
    { startWidgets =
        [ workspacesW
        ],
      endWidgets =
        [ batteryW,
          textBatteryNew "$percentage$%",
          clockW,
          memoryGraph,
          -- netGraphW,
          -- textBatteryNew "bat",
          -- menuWidgetNew Nothing,
          layoutNew defaultLayoutConfig,
          cpuGraphW
        ],
      barHeight = 20
    }

workspacesW :: TaffyIO Widget
workspacesW = do
  let myIcons = scaledWindowIconPixbufGetter $
                getWindowIconPixbufFromChrome <|||>
                unscaledDefaultGetWindowIconPixbuf <|||>
                (\size _ -> lift $ loadPixbufByName size "application-default-icon")
  workspacesNew defaultWorkspacesConfig {getWindowIconPixbuf = myIcons}

clockW :: TaffyIO Widget
clockW =
  textClockNewWith
    ClockConfig { clockTimeZone = Nothing,
      clockTimeLocale = Nothing,
      clockFormatString = "%a %b %_d %r",
      clockUpdateStrategy = RoundedTargetInterval 1 0.0
    }

batteryW :: TaffyIO Widget
batteryW = batteryIconNew

myGraphConfig =
  defaultGraphConfig
  { graphPadding = 0
  , graphBorderWidth = 0
  , graphWidth = 75
  , graphBackgroundColor = (0.0, 0.0, 0.0, 0.0)
  }


memoryGraph :: TaffyIO Widget
memoryGraph = 
  pollingGraphNew memCfg 0.5 memCallback
  where
    memCallback = do
      mi <- parseMeminfo
      return [memoryUsedRatio mi,memorySwapUsedRatio mi]
    memCfg = myGraphConfig
      { graphDataColors = [(0.129, 0.588, 0.953, 1),(1, 0, 1, 0.5)]
      , graphLabel = Just "mem"
      }

cpuGraphW :: TaffyIO Widget
cpuGraphW =
  pollingGraphNew cpuCfg 0.5 $ do
    (_, systemLoad, totalLoad) <- cpuLoad
    pure [totalLoad, systemLoad]
  where
    cpuCfg =
      myGraphConfig
        { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)],
          graphLabel = Just "cpu",
          graphWidth = 150
        }

netGraphW :: TaffyIO Widget
netGraphW = networkGraphNew netCfg Nothing
  where
    netCfg = myGraphConfig
      { graphDataColors = [yellow1, yellow2]
      , graphLabel = Just "net"
      }
    yellow1 = (242, 163, 54, 256)
    yellow2 = (254, 204, 83, 256)
    yellow3 = (227, 134, 18, 256)

-- cpuGraphW :: TaffyIO Widget
-- cpuGraphW =
--   pollingGraphNew cpuCfg 0.5 $ do
--     (_, systemLoad, totalLoad) <- cpuLoad
--     pure [totalLoad, systemLoad]
--   where
--     cpuCfg =
--       defaultGraphConfig
--         { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)],
--           graphLabel = Just "cpu",
--           graphWidth = 150
--         }

enableDebugLogging :: IO ()
enableDebugLogging = do
  traverse_ (saveGlobalLogger . setLevel DEBUG)
    =<< sequence
      [ getLogger "",
        getLogger "System.Taffybar",
        getLogger "StatusNotifier.Tray"
        -- getLogger "System.Taffybar.Widget.Battery"
      ]
