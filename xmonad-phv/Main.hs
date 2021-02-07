import XMonad

-----------------------------------------------------------------

-- Config Section Start
-- Contains bin names, programs expected by the env as a string
-- These programs are also listed in the xmonad.nix file, so that
-- nix knows to bake them in.
-----------------------------------------------------------------

-- terminal
myTerminal = "alacritty"


-----------------------------------------------------------------
-- Config Section End
-----------------------------------------------------------------


main = xmonad def
	{ modMask = mod4Mask
        , terminal = myTerminal
	}
