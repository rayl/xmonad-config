import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders
import XMonad.Util.Cursor
import XMonad.Util.Run(spawnPipe)
import System.IO

home = "/home/local/.xmonad/"

main = do
    xmproc <- spawnPipe $ "/usr/bin/xmobar " ++ home ++ "xmobar-bottom"
    xmonad $ defaultConfig
        { borderWidth        = 2
        , workspaces         = myWorkspaces
        , startupHook        = setDefaultCursor xC_left_ptr
        , layoutHook         = myLayoutHook
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


------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------
myWorkspaces = zipWith (\n l -> show n ++ "-" ++ l)
               [1..9]
               ["goog","todo","book","hask","*","*","*","*","mp3s"]


------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
myLayoutHook = myTransforms myLayouts

myLayouts = named "3COL" (multiCol [1,1] 8 0.01 0.33)
        ||| named "2COL" (multiCol [1,2] 8 0.01 0.50)
        ||| named "FULL" (Full)
        ||| named "DRAG" (mouseResizableTile { draggerType = BordersDragger })

myTransforms = smartBorders
             . avoidStruts
             . mkToggle (single NBFULL)

