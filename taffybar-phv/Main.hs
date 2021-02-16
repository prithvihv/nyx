{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Foldable (traverse_)
import GI.Gtk.Objects.Widget (Widget)
import System.Log.Logger
  ( Priority (DEBUG),
    getLogger,
    saveGlobalLogger,
    setLevel,
  )
import System.Taffybar (startTaffybar)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.CPU (cpuLoad)
import System.Taffybar.SimpleConfig (SimpleTaffyConfig (endWidgets), barHeight, defaultSimpleTaffyConfig, startWidgets, toTaffyConfig)
import System.Taffybar.Widget (defaultClockConfig, defaultWorkspacesConfig, textClockNewWith, workspacesNew)
import System.Taffybar.Widget.Battery (batteryIconNew)
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
  startTaffybar $ toTaffyConfig cfg

cfg :: SimpleTaffyConfig
cfg =
  defaultSimpleTaffyConfig
    { startWidgets =
        [ workspacesW
        ],
      endWidgets =
        [ clockW,
          batteryW,
          -- scratchW,
          -- FIXME: doesn't work
          -- menuWidgetNew Nothing,
          layoutNew defaultLayoutConfig,
          cpuW
        ],
      barHeight = 50
    }

workspacesW :: TaffyIO Widget
workspacesW = workspacesNew defaultWorkspacesConfig

clockW :: TaffyIO Widget
clockW = textClockNewWith defaultClockConfig

batteryW :: TaffyIO Widget
batteryW = batteryIconNew

scratchW :: TaffyIO Widget
scratchW = commandRunnerNew 1.0 "uname" [] "Cmd failed"

cpuW :: TaffyIO Widget
cpuW =
  pollingGraphNew cpuCfg 0.5 $ do
    (_, systemLoad, totalLoad) <- cpuLoad
    pure [totalLoad, systemLoad]
  where
    cpuCfg =
      defaultGraphConfig
        { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)],
          graphLabel = Just "cpu",
          graphWidth = 150
        }

enableDebugLogging :: IO ()
enableDebugLogging = do
  traverse_ (saveGlobalLogger . setLevel DEBUG)
    =<< sequence
      [ getLogger "",
        getLogger "System.Taffybar",
        getLogger "StatusNotifier.Tray",
        getLogger "System.Taffybar.Widget.Battery"
      ]
