import Data.Char (toUpper)
import qualified Data.Map as M
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CycleWS (nextWS, prevWS, toggleWS, toggleOrView)
import XMonad.Actions.Search
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.WindowBringer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.IM
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.Run(spawnPipe)

home = "/home/local/.xmonad/"

main = do
    topBar    <- spawnPipe myTopBar
    bottomBar <- spawnPipe myBottomBar
    xmonad $ ewmh $ defaultConfig
        { borderWidth        = myBorderWidth
        , workspaces         = myWorkspaces
        , layoutHook         = myLayoutHook
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , modMask            = myModMask
        , keys               = myKeys
        , logHook            = myLogHook defaultConfig topBar bottomBar
        , startupHook        = myStartupHook
        , mouseBindings      = myMouseBindings
        , manageHook         = myManageHook defaultConfig
        , handleEventHook    = myHandleEventHook defaultConfig
        }

------------------------------------------------------------------------
-- DECORATION
------------------------------------------------------------------------
myBorderWidth = 2
myNormalBorderColor = "#222"
myFocusedBorderColor = "#ff4"

------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------
myWorkspaces = zipWith (\n l -> show n ++ "-" ++ l)
               [1..9]
               ["goog","todo","book","hask","gimp","*","*","*","mp3s"]


------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
myLayoutHook = avoidStruts
             $ smartBorders
             $ mkToggle (single NBFULL)
             $ onWorkspace "5-gimp" l_GIMP
             $ l_3COL ||| l_2COL ||| l_FULL ||| l_DRAG

l_3COL = named "3COL" $ multiCol [1,1] 8 0.01 0.33
l_2COL = named "2COL" $ multiCol [1,2] 8 0.01 0.50
l_FULL = named "FULL" $ Full
l_DRAG = named "DRAG" $ mouseResizableTile { draggerType = BordersDragger }
l_TALL = named "TALL" $ ResizableTall 2 (1/118) (11/20) [1]
l_GIMP = named "GIMP" $ withIM (0.15) (Role "gimp-toolbox")
                      $ reflectHoriz $ withIM (0.15) (Role "gimp-dock")
                      $ l_TALL ||| l_FULL


------------------------------------------------------------------------
-- MANAGE
------------------------------------------------------------------------
myManageHook c =  myManageHooks
              <+> manageDocks

myManageHooks = composeAll
    [ className =? "Gimp"             --> unfloat
    , className =? "foo"              --> unfloat
    ]
    where
       unfloat = ask >>= doF . W.sink


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
        , ((modS, xK_i       ), toggleOrView "1-goog")
        , ((modm, xK_o       ), nextWS)

        , ((modm, xK_a       ), spawn $ XMonad.terminal conf)
        , ((modS, xK_a       ), spawn "dmenu_run")
        , ((modm, xK_s       ), promptSearch defaultXPConfig google)
        , ((modS, xK_s       ), selectSearch google)
        , ((modm, xK_d       ), gotoMenu)
        , ((modS, xK_d       ), bringMenu)
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
        , ((modS, xK_z       ), kill)
        , ((modm, xK_x       ), sendMessage ToggleStruts)
        , ((modm, xK_space   ), sendMessage NextLayout)
        , ((modS, xK_space   ), setLayout $ XMonad.layoutHook conf)
        ]
        ++
        [((modm .|. m, k), windows $ f i)
                | (i, k) <- zip (workspaces conf) [xK_1..]
                , (f, m) <- [(W.greedyView,0), (W.shift,shiftMask), (swapWithCurrent,controlMask)]]
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
myTopBar    = "/usr/bin/xmobar " ++ home ++ "xmobar-top"
myBottomBar = "/usr/bin/xmobar " ++ home ++ "xmobar-bottom"

myLogHook c u d = logHook c

    -- top status bar
    <+> dynamicLogWithPP defaultPP
        { ppOutput   = hPutStrLn u
        , ppOrder    = \(ws:l:t:_) -> [t]
        , ppTitle    = xmobarColor "black"  "green" . wrap "  " "  "
        }

    -- bottom status bar
    <+> dynamicLogWithPP defaultPP
        { ppOutput   = hPutStrLn d
        , ppOrder    = \(ws:l:t:_) -> [l,ws]
        , ppSep      = " "
        , ppLayout   = xmobarColor "black"  "#ccc"   . wrap "<" ">"
        , ppWsSep    = " "
        , ppCurrent  = xmobarColor "black"  "yellow" . map toUpper
        , ppHidden   = xmobarColor "white"  ""
        , ppUrgent   = xmobarColor "red"    "yellow"
        }


------------------------------------------------------------------------
-- STARTUP
------------------------------------------------------------------------
myStartupHook =  setDefaultCursor xC_left_ptr
             <+> setWMName "LG3D"
