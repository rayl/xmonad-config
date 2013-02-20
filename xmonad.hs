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
import XMonad.Util.EZConfig (mkKeymap)
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
myWorkspaces = ["goog","todo","book","read","hask","gimp","mp3s"]


------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
myLayoutHook = avoidStruts
             $ smartBorders
             $ mkToggle (single NBFULL)
             $ onWorkspace "gimp" l_GIMP
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
myModMask = mod4Mask

myKeys conf = mkKeymap conf $ concat
                [ tableKeys conf
                , screenKeys
                , workspaceKeys
                ]

tableKeys conf = concat
  --  keysym         M-               M-S-             M-C-             M-S-C-
  [ k "<grave>"      toggleWorkspace  __               __               __
  , k "-"            __               __               __               __
  , k "<Backspace>"  __               __               __               __

  , k "q"            restartXmonad    quitXmonad       __               __
  , k "w"            __               __               __               __
  , k "e"            __               __               __               __
  , k "r"            __               __               __               __
  , k "t"            sinkWindow       __               __               __
  , k "y"            __               __               __               __
  , k "u"            prevWorkspace    __               __               __
  , k "i"            googleWorkspace  __               __               __
  , k "o"            nextWorkspace    __               __               __
  , k "p"            __               __               __               __

  , k "a"            openTerminal     openDmenu        __               __
  , k "s"            searchPrompt     searchSelection  __               __
  , k "d"            gotoMenu'        bringMenu'       __               __
  , k "f"            __               __               __               __
  , k "g"            __               __               __               __
  , k "h"            shrinkMaster     shrinkSlave      __               __
  , k "j"            focusDown        swapDown         __               __
  , k "k"            focusUp          swapUp           __               __
  , k "l"            expandMaster     expandSlave      __               __
  , k "<Return>"     swapMaster       shiftMaster      __               __

  , k "z"            fullscreen       closeWindow      __               __
  , k "x"            toggleStruts     __               __               __
  , k "c"            __               __               __               __
  , k "v"            __               __               __               __
  , k "b"            __               __               __               __
  , k "n"            refresh'         __               __               __
  , k "m"            focusMaster      __               __               __
  , k "<comma>"      incMaster        __               __               __
  , k "<period>"     decMaster        __               __               __

  , k "<Space>"      __               __               __               __
  ]
  where
    k key m ms mc msc =
        [ bind "M-"      key m
        , bind "M-S-"    key ms
        , bind "M-C-"    key mc
        , bind "M-S-C-"  key msc
        ]
    bind mod key cmd = (mod ++ key, cmd)

    __               = return ()

    --------------------------------------------------------------------
    -- xmonad
    restartXmonad    = restart "xmonad" True
    quitXmonad       = io $ exitWith ExitSuccess

    --------------------------------------------------------------------
    -- screen

    --------------------------------------------------------------------
    -- workspace
    nextWorkspace    = nextWS
    prevWorkspace    = prevWS
    toggleWorkspace  = toggleWS
    googleWorkspace  = toggleOrView "goog"

    --------------------------------------------------------------------
    -- layout
    refresh'         = refresh
    resetLayout      = setLayout $ layoutHook conf
    nextLayout       = sendMessage NextLayout
    fullscreen       = sendMessage $ Toggle NBFULL
    toggleStruts     = sendMessage ToggleStruts

    expandMaster     = sendMessage Expand
    shrinkMaster     = sendMessage Shrink
    expandSlave      = sendMessage ExpandSlave
    shrinkSlave      = sendMessage ShrinkSlave
    incMaster        = sendMessage (IncMasterN 1)
    decMaster        = sendMessage (IncMasterN (-1))
    closeWindow      = kill
    sinkWindow       = withFocused $ windows . W.sink

    gotoMenu'        = gotoMenu
    bringMenu'       = bringMenu

    focusDown        = windows W.focusDown
    swapDown         = windows W.swapDown
    focusUp          = windows W.focusUp
    swapUp           = windows W.swapUp
    swapMaster       = windows W.swapMaster
    shiftMaster      = windows W.shiftMaster
    focusMaster      = windows W.focusMaster

    --------------------------------------------------------------------
    -- tools
    openTerminal     = spawn $ terminal conf
    openDmenu        = spawn "dmenu_run"
    searchPrompt     = promptSearch defaultXPConfig google
    searchSelection  = selectSearch google

workspaceKeys =
   [(mod ++ key, windows $ cmd tag)
       | (tag, key) <- zip myWorkspaces $ map show [1..]
       , (cmd, mod) <- [(W.greedyView,"M-"), (W.shift,"M-S-"), (swapWithCurrent,"M-C-")]]

screenKeys =
   [(mod ++ key, screenWorkspace scr >>= flip whenJust (windows . cmd))
       | (key, scr) <- zip ["w", "e", "r"] [0..]
       , (cmd, mod) <- [(W.view, "M-"), (W.shift, "M-S-")]]


-- Mouse bindings
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
        [ ((myModMask, button1), (\w -> focus w >> windows W.shiftMaster))
        , ((myModMask, button2), (\w -> focus w))
        , ((myModMask, button3), (\w -> focus w))
        , ((myModMask, button4), (\_ -> prevWS))
        , ((myModMask, button5), (\_ -> nextWS))
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
        , ppCurrent  = xmobarColor "black"  "yellow" . map toUpper . shortcut
        , ppHidden   = xmobarColor "white"  "" . shortcut
        , ppUrgent   = xmobarColor "red"    "yellow"
        }
        where
           labels = M.fromList $ zip myWorkspaces (map show [1..])
           shortcut x = case (M.lookup x labels) of
               Just i  -> i ++ "-" ++ x
               Nothing -> x

------------------------------------------------------------------------
-- STARTUP
------------------------------------------------------------------------
myStartupHook =  setDefaultCursor xC_left_ptr
             <+> setWMName "LG3D"
