import XMonad
import qualified Data.Map.Strict as M

-----------------------------------------------------------------

-- Config Section Start
-- Contains bin names, programs expected by the env as a string
-- These programs are also listed in the xmonad.nix file, so that
-- nix knows to bake them in.
-----------------------------------------------------------------

-- terminal
myTerminal = "alacritty"
myGUILuncher = "rofi -show drun"

-- xmonad
myBorderWidth = 1
myNormalBorderColor = "#000000"
myFocusedBorderColor = "#ffaa00"

-----------------------------------------------------------------
-- Config Section End
-----------------------------------------------------------------

myKeys baseConfig@XConfig {XMonad.modMask = modMask} =
    M.fromList
      [ ((modMask, xK_q), restart "~/.nix-profiles/bin/xmonad" True)
        , ((modMask, xK_p), spawn myGUILuncher)
        , ((modMask, xK_a), spawn "screenshot")
      ] <> keys def baseConfig

main = xmonad def
  { modMask = mod4Mask
  , terminal = myTerminal
  , borderWidth = myBorderWidth
  , keys = myKeys
  }