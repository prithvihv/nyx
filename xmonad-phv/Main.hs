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

-- xmonad
myBorderWidth = 1
myNormalBorderColor = "#000000"
myFocusedBorderColor = "#ffaa00"

-----------------------------------------------------------------
-- Config Section End
-----------------------------------------------------------------

--          ((modKey, xK_b), sendMessage ToggleStruts)
myKeys baseConfig@XConfig {modMask = modKey} =
    keys def baseConfig
      <> M.fromList
        [ ((modKey, xK_q), restart "/run/current-system/sw/bin/xmonad" True),
          ((modKey, xK_f), spawn "screenshot")
        ]

main = xmonad def
	{ modMask = mod4Mask
        , terminal = myTerminal
        , borderWidth = myBorderWidth
        , keys = myKeys
	}
