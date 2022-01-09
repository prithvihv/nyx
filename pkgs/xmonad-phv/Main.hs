{-# LANGUAGE FlexibleContexts #-}

-- import           XMonad.Util.Run                ( spawnPipe )
import qualified Data.Map.Strict as M
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Core
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, docksEventHook, manageDocks)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.EZConfig (additionalKeys)

-----------------------------------------------------------------

-- Config Section Start
-- Contains bin names, programs expected by the env as a string
-- These programs are also listed in the xmonad.nix file, so that
-- nix knows to bake them in.
-----------------------------------------------------------------

-- terminal
myTerminal = "alacritty"

myGUILuncher = "rofi -show drun"

myPasswordLuncher = "rofi-pass"

myEmojiLuncher = "rofimoji"

myClipboardLuncher = "clipmenu"

myLockScreen = "i3lock -i ~/Downloads/nix-glow-black-1080p.png" -- FIXME: not pure

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
        ((modMask, xK_p), spawn myGUILuncher),
        ((modMask .|. shiftMask, xK_0), spawn myEmojiLuncher),
        ((modMask .|. shiftMask, xK_p), spawn myPasswordLuncher),
        ((modMask .|. shiftMask, xK_l), spawn myLockScreen),
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
  spawn "feh --bg-scale ~/Downloads/nix-glow-black.png" -- FIXME: this should be passed as nix
  spawn "go env -w GOPRIVATE=github.com/gamezop" -- FIXME: this should be golang config

main =
  xmonad . docks . ewmh $
    def
      { modMask = mod4Mask,
        terminal = myTerminal,
        borderWidth = myBorderWidth,
        keys = myKeys,
        layoutHook = avoidStruts $ (smartBorders $ (layoutHook def)),
        manageHook = manageDocks,
        handleEventHook = docksEventHook,
        startupHook = myStartupHook,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor
      }