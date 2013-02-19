import qualified Data.Map as M
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CycleWS (nextWS, prevWS, toggleWS, toggleOrView)
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.WindowBringer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.Run(spawnPipe)

home = "/home/local/.xmonad/"

main = do
    xmproc <- spawnPipe $ "/usr/bin/xmobar " ++ home ++ "xmobar-bottom"
    xmonad $ ewmh $ defaultConfig
        { borderWidth        = 2
        , workspaces         = myWorkspaces
        , layoutHook         = myLayoutHook
        , modMask            = myModMask
        , keys               = myKeys
        , logHook            = myLogHook xmproc
        , startupHook        = myStartupHook
        , mouseBindings      = myMouseBindings
        , manageHook         = myManageHook defaultConfig
        , handleEventHook    = myHandleEventHook defaultConfig
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


------------------------------------------------------------------------
-- MANAGE
------------------------------------------------------------------------
myManageHook c =  manageHook c
              <+> manageDocks


------------------------------------------------------------------------
-- EVENTS
------------------------------------------------------------------------
myHandleEventHook c =  handleEventHook c
                   <+> docksEventHook
                   <+> fullscreenEventHook

------------------------------------------------------------------------
-- BINDINGS
------------------------------------------------------------------------
modm = mod4Mask
modS = modm .|. shiftMask
modC = modm .|. controlMask

myModMask = modm

-- Key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
	[ ((modm, xK_q       ), restart "xmonad" True)
        , ((modS, xK_q       ), io (exitWith ExitSuccess))
        , ((modm, xK_t       ), withFocused $ windows . W.sink)
        , ((modm, xK_u       ), prevWS)
        , ((modm, xK_i       ), toggleWS)
        , ((modm, xK_o       ), nextWS)

        , ((modm, xK_a       ), spawn $ XMonad.terminal conf)
        , ((modS, xK_a       ), spawn "dmenu_run")
        , ((modm, xK_s       ), gotoMenu)
        , ((modS, xK_s       ), bringMenu)
        , ((modm, xK_h       ), sendMessage Shrink)
        , ((modS, xK_h       ), sendMessage ShrinkSlave)
        , ((modm, xK_j       ), windows W.focusDown)
        , ((modS, xK_j       ), windows W.swapDown)
        , ((modm, xK_k       ), windows W.focusUp)
        , ((modS, xK_k       ), windows W.swapUp)
        , ((modm, xK_l       ), sendMessage Expand)
        , ((modS, xK_l       ), sendMessage ExpandSlave)
        , ((modm, xK_Return  ), windows W.swapMaster)
        , ((modS, xK_Return  ), windows W.shiftMaster)

        , ((modm, xK_n       ), refresh)
        , ((modm, xK_m       ), windows W.focusMaster)
        , ((modm, xK_comma   ), sendMessage (IncMasterN 1))
        , ((modm, xK_period  ), sendMessage (IncMasterN (-1)))

        , ((modm, xK_z       ), sendMessage (Toggle NBFULL))
        , ((modm, xK_x       ), sendMessage ToggleStruts)
        , ((modS, xK_c       ), kill)
        , ((modm, xK_space   ), sendMessage NextLayout)
        , ((modS, xK_space   ), setLayout $ XMonad.layoutHook conf)
        ]
        ++
        [((modm .|. m, k), windows $ f i)
                | (i, k) <- zip (workspaces conf) [xK_1..]
                , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
        ++
        [((modC, k), windows $ swapWithCurrent i)
                | (i, k) <- zip (workspaces conf) [xK_1..]]
        ++
        [((modm .|. m, k), screenWorkspace sc >>= flip whenJust (windows . f))
                | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..]
                , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


-- Mouse bindings
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
        [ ((modm, button1), (\w -> focus w >> windows W.shiftMaster))
        , ((modm, button2), (\w -> focus w))
        , ((modm, button3), (\w -> focus w))
        , ((modm, button4), (\_ -> prevWS))
        , ((modm, button5), (\_ -> nextWS))
        ]


------------------------------------------------------------------------
-- LOGGING
------------------------------------------------------------------------
myLogHook pipe = dynamicLogWithPP defaultPP
        { ppOutput   = hPutStrLn pipe
        , ppOrder    = \(ws:l:t:_) -> [l,ws,t]
        , ppSep      = " "
        , ppLayout   = xmobarColor "black"  "#ccc"   . wrap "<" ">"
        , ppWsSep    = " "
        , ppCurrent  = xmobarColor "black"  "yellow" . shorten 9
        , ppHidden   = xmobarColor "white"  ""       . shorten 9
        , ppTitle    = xmobarColor "black"  "green"  . shorten 70 . wrap " " " "
        , ppUrgent   = xmobarColor "red"    "yellow"
        }


------------------------------------------------------------------------
-- STARTUP
------------------------------------------------------------------------
myStartupHook = setDefaultCursor xC_left_ptr

