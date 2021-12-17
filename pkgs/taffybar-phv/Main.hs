{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Exception
import Control.Monad.Trans.Reader
import System.Taffybar
import System.Taffybar.Context (TaffybarConfig)
import System.Taffybar.Hooks
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Util ((<|||>))
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingGraph

-- myGraphConfig, memCfg, cpuCfg :: GraphConfig
-- myGraphConfig = defaultGraphConfig
--   { graphPadding = 0
--   , graphBorderWidth = 0
--   , graphBackgroundColor = (0, 0, 0, 0)
--   , graphDirection = RIGHT_TO_LEFT
--   }

-- memCallback :: IO [Double]
-- memCallback = do
--   mi <- parseMeminfo
--   return [memoryUsedRatio mi, memorySwapUsed mi]

-- cpuCallback :: IO [Double]
-- cpuCallback = do
--   (_, systemLoad, totalLoad) <- cpuLoad
--   return [totalLoad, systemLoad]

-- memCfg = myGraphConfig
--   { graphDataColors = [(1, 1, 1, 1), (0.5, 0.5, 0.5, 1)]
--   , graphLabel = Just "mem "
--   }

-- cpuCfg = myGraphConfig
--   { graphDataColors = [(1, 1, 1, 1), (0.5, 0.5, 0.5, 1)]
--   , graphLabel = Just "cpu "
--   }

-- mpris2 = mpris2New

-- cpu = pollingGraphNew cpuCfg 1 cpuCallback

-- mem = pollingGraphNew memCfg 1 memCallback

-- battery = batteryIconNew

tray = sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt

clock = textClockNewWith defaultClockConfig
  { clockFormatString = "<span fgcolor='gold'>%a %b %_d %r</span>"
  , clockUpdateStrategy = RoundedTargetInterval 1 0
  }

handleException :: WindowIconPixbufGetter -> WindowIconPixbufGetter
handleException getter = \size windowData ->
                           ReaderT $ \c ->
                                       catch (runReaderT (getter size windowData) c) $ \(_ :: SomeException)
                                                                                       -> return Nothing

myGetWindowIconPixbuf :: WindowIconPixbufGetter
myGetWindowIconPixbuf = scaledWindowIconPixbufGetter $
  handleException getWindowIconPixbufFromDesktopEntry <|||>
  handleException getWindowIconPixbufFromClass <|||>
  handleException getWindowIconPixbufFromEWMH

myWorkspacesConfig = defaultWorkspacesConfig
  { showWorkspaceFn = hideEmpty
  , getWindowIconPixbuf = myGetWindowIconPixbuf
  }

workspaces = workspacesNew myWorkspacesConfig

layout = layoutNew defaultLayoutConfig

windowsW = windowsNew defaultWindowsConfig

taffybarConfig :: TaffybarConfig
taffybarConfig =
  let myConfig = defaultSimpleTaffyConfig
        { startWidgets = workspaces : map (>>= buildContentsBox) [ layout, windowsW ]
        , endWidgets = map (>>= buildContentsBox)
          [ clock
          , tray
          -- , battery
          -- , mem
          -- , cpu
          ]
        , barHeight = 40
        }
  in withToggleServer $
     toTaffyConfig myConfig

main :: IO ()
main = startTaffybar taffybarConfig