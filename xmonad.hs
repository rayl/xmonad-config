import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Cursor
import XMonad.Util.Run(spawnPipe)
import System.IO

home = "/home/local/.xmonad/"

main = do
    xmproc <- spawnPipe $ "/usr/bin/xmobar " ++ home ++ "xmobar-bottom"
    xmonad $ defaultConfig
        { borderWidth        = 2
        , startupHook        = setDefaultCursor xC_left_ptr
        , layoutHook         = avoidStruts $ layoutHook defaultConfig
        , manageHook         = manageHook defaultConfig <+> manageDocks
        , handleEventHook    = handleEventHook defaultConfig <+> docksEventHook
        , logHook            = dynamicLogWithPP defaultPP
                                 { ppOutput               = hPutStrLn xmproc
                                 , ppOrder                = \(ws:l:t:_) -> [l,ws,t]
                                 , ppSep                  = " "
                                 , ppLayout               = xmobarColor "black"  "#ccc"   . wrap "<" ">"
                                 , ppWsSep                = " "
                                 , ppCurrent              = xmobarColor "black"  "yellow" . shorten 9
                                 , ppHidden               = xmobarColor "white"  ""       . shorten 9
                                 , ppTitle                = xmobarColor "black"  "green"  . shorten 70 . wrap " " " "
                                 , ppUrgent               = xmobarColor "red"    "yellow"
                                 }
        }
