{-# LANGUAGE FlexibleContexts #-}

-- import           XMonad.Util.Run                ( spawnPipe )

-- imports for pollybar

import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D
import Data.List (find)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xlib.Display
import XMonad
import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, docksEventHook, manageDocks)
import XMonad.Layout.Decoration
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Util.EZConfig (additionalKeys)

-----------------------------------------------------------------

-- Config Section Start
-- Contains bin names, programs expected by the env as a string
-- These programs are also listed in the xmonad.nix file, so that
-- nix knows to bake them in.
-----------------------------------------------------------------

-- terminal
myTerminal = "alacritty"

myGUILauncher = "rofi -dpi -show drun"

myAutorandrlauncher = "rofiAutorandr"

myToggleStatusBar = "togglePolybar"

myPasswordLauncher = "clipctl disable && rofi-pass &&  clipctl enable"
myBlueToothLauncher = "rofi-bluetooth.sh"

myEmojiLauncher = "rofimoji"

myClipboardLuncher = "clipmenu"

myLockScreen = "betterlockscreen -l --off 10" -- FIXME: not pure

-- xmonad
myBorderWidth = 1

myNormalBorderColor = "#000000"

myFocusedBorderColor = "#ffaa00"

-------------------------------------------------------------
-- keyboard comands
keyBoardBrightness_base = "brightnessctl -d 'intel_backlight' set "

keyBoardBrightness_variance = "5%"

keyBoardBrightnessUp = keyBoardBrightness_base ++ "+" ++ keyBoardBrightness_variance

keyBoardBrightnessDown = keyBoardBrightness_base ++ keyBoardBrightness_variance ++ "-"

keyboardVolume_base = "pactl set-sink-volume 0 "

keyboardVolume_variance = "2%"

keyboardVolumeUp = keyboardVolume_base ++ "+" ++ keyboardVolume_variance

keyboardVolumeDown = keyboardVolume_base ++ "-" ++ keyboardVolume_variance

keyboardVolumeMute = "pactl set-sink-volume 0 " ++ "0"

termWs = '\61728' : ":term" -- 

webWs = '\62057' : ":web" -- 

devWs = '\61729' : ":code" -- 

comWs = '\128489' : ":comm" --  🗩

devtoolsWs = '\61820' : ":dev" -- 

search = '\62498' : ":web" -- 

productivityToolsWs = '\61820' : ":tool" -- 

setWs = '\9881' : ":config" -- ⚙

setMoreWs = '\9881' : ":admin" -- ⚙

-- wrkWs = "wrk"
-- sysWs = "sys"
-- etcWs = "etc"

-- termWs= ""
-- webWs = ""
-- devWs = ""
-- comWs = "🗩"
-- setWs = "⚙"
-- devtoolsWs = ""
-- search = ""

myWS :: [WorkspaceId]
myWS = [termWs, webWs, devWs, comWs, devtoolsWs, search, productivityToolsWs, setWs, setMoreWs]

-----------------------------------------------------------------
-- Config Section End
-----------------------------------------------------------------

myKeys baseConfig@XConfig {XMonad.modMask = modMask} =
  M.fromList myConfig <> keys def baseConfig
  where
    scrotParams = "-e 'xclip -selection clipboard -t image/png -i  $f && mv $f ~/Pictures/sshots'"
    -- FIXME: make more generic
    pathToBinary = "/etc/profiles/per-user/phv/bin/xmonad-x86_64-linux"
    myConfig =
      [ ((controlMask, xK_Print), spawn ("sleep 0.2; scrot -s " ++ scrotParams)),
        ((modMask, xK_q), restart pathToBinary True),
        ((modMask, xK_p), spawn myGUILauncher),
        ((modMask .|. shiftMask, xK_b), spawn myToggleStatusBar),
        ((modMask .|. shiftMask, xK_d), spawn myAutorandrlauncher),
        ((modMask .|. shiftMask, xK_0), spawn myEmojiLauncher),
        ((modMask .|. shiftMask, xK_p), spawn myPasswordLauncher),
        ((modMask .|. shiftMask, xK_l), spawn myLockScreen),
        ((modMask .|. shiftMask, xK_h), spawn myBlueToothLauncher),
        ((modMask, xK_c), spawn myClipboardLuncher),
        ((modMask, xK_a), spawn "screenshot")
      ]
        -- DELL XPS
        ++ [ ((0, xF86XK_MonBrightnessUp), spawn keyBoardBrightnessUp),
             ((0, xF86XK_MonBrightnessDown), spawn keyBoardBrightnessDown),
             ((0, xF86XK_AudioRaiseVolume), spawn keyboardVolumeUp),
             ((0, xF86XK_AudioLowerVolume), spawn keyboardVolumeDown),
             ((0, xF86XK_AudioMute), spawn keyboardVolumeMute)
           ]

myStartupHook = do
  spawn "go env -w GOPRIVATE=github.com/gamezop" -- FIXME: this should be golang config

-- reference: https://github.com/gvolpe/nix-config/blob/master/home/programs/xmonad/config.hs
mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.log") opts
  return dbus
  where
    opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath = D.objectPath_ "/org/xmonad/Log"
      iname = D.interfaceName_ "org.xmonad.Log"
      mname = D.memberName_ "Update"
      signal = (D.signal opath iname mname)
      body = [D.toVariant $ UTF8.decodeString str]
   in D.emit dbus $ signal {D.signalBody = body}

findTitle :: [Char] -> String
findTitle t
  | isJust $ find (== '-') t = (last . splitOn "-") t
  | otherwise = shorten 100 t

-- Color Palette
bgDracula = "#44475a"

yellow = "#f1fa8c"

currDracula = "#6272a4"

orange = "#ffb86c"

cyan = "#8be9fd"

polybarHook :: D.Client -> PP
polybarHook dbus =
  let wrapper c s
        | s /= "NSP" = wrap ("%{F" <> c <> "} ") " %{F-}" s
        | otherwise = mempty
      purple = "#9058c7"
   in def
        { ppOutput = dbusOutput dbus,
          ppCurrent = wrapper yellow,
          ppVisible = wrapper cyan,
          ppUrgent = wrapper orange,
          ppHidden = wrapper currDracula,
          ppHiddenNoWindows = wrapper bgDracula,
          -- ppTitle = wrapper purple . findTitle
          ppTitle = const "" -- disable title
        }

myPolybarLogHook :: D.Client -> X ()
myPolybarLogHook dbus = dynamicLogWithPP (polybarHook dbus)

-- myLogHook <+> dynamicLogWithPP (polybarHook dbus)

-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook :: X ()
myLogHook = fadeInactiveLogHook 0.9

main :: IO ()
main = mkDbusClient >>= main'

myLayout =
  avoidStruts
    ( Tall 1 (3 / 100) (2 / 3)
        ||| Mirror (Tall 1 (3 / 100) (4 / 5))
        ||| tabbed shrinkText tabConfig
        ||| spiral (6 / 7)
        ||| Mirror (Tall 1 (3 / 100) (1 / 5))
    )
    ||| noBorders (fullscreenFull Full)
  where
    tabConfig =
      def
        { activeBorderColor = bgDracula,
          activeTextColor = yellow,
          activeColor = "#282a36",
          inactiveBorderColor = bgDracula,
          inactiveTextColor = "#f8f8f2",
          inactiveColor = "#282a36",
          fontName = "xft:Iosevka:size=3" -- FIXME: now working
        }

main' :: D.Client -> IO ()
main' client = do
  xmonad . docks . ewmh $
    def
      { modMask = mod4Mask,
        terminal = myTerminal,
        borderWidth = myBorderWidth,
        keys = myKeys,
        layoutHook = smartBorders myLayout,
        manageHook = manageDocks,
        handleEventHook = docksEventHook,
        startupHook = myStartupHook,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        logHook = myPolybarLogHook client,
        workspaces = myWS
      }