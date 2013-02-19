import XMonad
import XMonad.Util.Cursor

main = do
    xmonad $ defaultConfig
        { borderWidth        = 2
        , startupHook        = setDefaultCursor xC_left_ptr
        }
