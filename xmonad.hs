
import Data.Char                         (toUpper)
import qualified Data.Map as M           (Map,fromList,lookup)
import Data.Monoid                       (All)
import System.Exit                       (exitWith,ExitCode(ExitSuccess))
import System.IO                         (hPutStrLn, Handle)
import XMonad                            -- many
import XMonad.Actions.CycleWS            (nextWS, prevWS, toggleWS, toggleOrView,
                                          shiftToNext, shiftToPrev)
import XMonad.Actions.Search             (promptSearch,selectSearch,google)
import XMonad.Actions.SwapWorkspaces     (swapWithCurrent)
import XMonad.Actions.WindowBringer      (bringMenu,gotoMenu)
import XMonad.Hooks.DynamicLog           -- many
import XMonad.Hooks.EwmhDesktops         (ewmh,fullscreenEventHook)
import XMonad.Hooks.ManageDocks          (manageDocks,docksEventHook,
                                          avoidStruts,ToggleStruts(..))
import XMonad.Hooks.SetWMName            (setWMName)
import XMonad.Layout.IM                  (withIM,Property(Role))
import XMonad.Layout.MouseResizableTile  (mouseResizableTile, draggerType,
                                          DraggerType(BordersDragger),
                                          MRTMessage(ExpandSlave,ShrinkSlave))
import XMonad.Layout.MultiColumns        (multiCol)
import XMonad.Layout.MultiToggle         (mkToggle,single,Toggle(..))
import XMonad.Layout.MyTransformers      (MyTransformers(ZOOM))
import XMonad.Layout.NoBorders           (smartBorders)
import XMonad.Layout.PerWorkspace        (onWorkspace)
import XMonad.Layout.Reflect             (reflectHoriz)
import XMonad.Layout.Renamed             (renamed,Rename(Replace,CutWordsLeft))
import XMonad.Layout.ResizableTile       (ResizableTall(..))
import XMonad.Prompt                     (defaultXPConfig)
import qualified XMonad.StackSet as W    -- many
import XMonad.Util.Cursor                (setDefaultCursor)
import XMonad.Util.EZConfig              (mkKeymap)
import XMonad.Util.Run                   (spawnPipe)

home :: String
home = "/home/local/.xmonad/"

main :: IO ()
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
myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColor :: String
myNormalBorderColor = "#222"

myFocusedBorderColor :: String
myFocusedBorderColor = "#ff4"


------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["dash","todo","news","book","tune"]


------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
myLayoutHook = avoidStruts
             $ smartBorders
             $ mkToggle (single ZOOM)
             $ onWorkspace "gimp" (gimpModify gimpLayouts)
             $ onWorkspace "term" termLayouts
             $ mainLayouts
  where

    mainLayouts = l_3COL ||| l_2COL ||| l_FULL ||| l_DRAG
    gimpLayouts = l_TALL ||| l_FULL
    termLayouts = l_3COL ||| l_FULL

    gimpModify x = renamed [CutWordsLeft 3]
                 $ withIM (0.15) (Role "gimp-toolbox")
                 $ reflectHoriz
                 $ withIM (0.15) (Role "gimp-dock")
                 $ x

    l_3COL = name "3col" $ multiCol [1,1] 8 0.01 0.33
    l_2COL = name "2col" $ multiCol [1,2] 8 0.01 0.50
    l_FULL = name "full" $ Full
    l_DRAG = name "drag" $ mouseResizableTile { draggerType = BordersDragger }
    l_TALL = name "tall" $ ResizableTall 2 (1/118) (11/20) [1]

    name x = renamed [Replace x]

------------------------------------------------------------------------
-- MANAGE
------------------------------------------------------------------------
myManageHook :: XConfig l -> ManageHook
myManageHook c =  myManageHooks
              <+> manageDocks

myManageHooks :: ManageHook
myManageHooks = composeAll
    [ className =? "Gimp"             --> unfloat
    , className =? "foo"              --> unfloat
    ]
    where
       unfloat = ask >>= doF . W.sink


------------------------------------------------------------------------
-- EVENTS
------------------------------------------------------------------------
myHandleEventHook :: XConfig l -> (Event -> X All)
myHandleEventHook c =  handleEventHook c
                   <+> docksEventHook
                   <+> fullscreenEventHook


------------------------------------------------------------------------
-- BINDINGS
------------------------------------------------------------------------
myModMask   =   mod4Mask
myKeyMaps   = [ keyboardMap, screenMap, workspaceMap ]
myMouseMaps = [ mouseMap ] 


mouseMap :: XConfig l -> [((KeyMask, Button), (Window -> X ()))]
mouseMap conf = concat
  --  button         M-               M-S-             M-C-             M-S-C-
  [ k button1        shiftMaster      __               __               __
  , k button2        focusWindow      __               __               __
  , k button3        focusWindow      __               __               __
  , k button4        focusUp          nextWorkspace    __               __
  , k button5        focusDown        prevWorkspace    __               __
  ] 
  where
    __               = \_ -> return ()
    nextWorkspace    = \_ -> nextWS
    prevWorkspace    = \_ -> prevWS
    focusDown        = \_ -> windows W.focusDown
    focusUp          = \_ -> windows W.focusUp
    shiftMaster      = \w -> focus w >> windows W.shiftMaster
    focusWindow      = \w -> focus w

    k key m ms mc msc =
        [ bind m'    key m
        , bind ms'   key ms
        , bind mc'   key mc
        , bind msc'  key msc
        ]
      where
        bind mod key cmd = ((mod,key), cmd)
        m'   = myModMask
        ms'  = myModMask .|. shiftMask
        mc'  = myModMask .|.               controlMask
        msc' = myModMask .|. shiftMask .|. controlMask

keyboardMap :: XConfig Layout -> [(String, X ())]
keyboardMap conf = concat
  --  keysym         M-               M-S-             M-C-             M-S-C-
  [ k "`"            lastWorkspace    __               __               __
  , k "-"            __               __               __               __
  , k "<Backspace>"  __               __               __               __

  , k "q"            restartXmonad    resetXmonad      __              quitXmonad
  , k "w"            __               __               __               __
  , k "e"            __               __               __               __
  , k "r"            __               __               __               __
  , k "t"            sinkWindow       __               __               __
  , k "y"            __               __               __               __
  , k "u"            prevWorkspace    toPrevWorkspace  __               __
  , k "i"            dashWorkspace    __               __               __
  , k "o"            nextWorkspace    toNextWorkspace  __               __
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
  , k ","            incMaster        __               __               __
  , k "."            decMaster        __               __               __

  , k "<Space>"      nextLayout       firstLayout      __               __
  ]
  where
    __               = return ()
    restartXmonad    = restart "xmonad" True
    resetXmonad      = restart "xmonad" False
    quitXmonad       = io $ exitWith ExitSuccess
    nextWorkspace    = nextWS
    prevWorkspace    = prevWS
    toNextWorkspace  = shiftToNext
    toPrevWorkspace  = shiftToPrev
    lastWorkspace    = toggleWS
    dashWorkspace    = toggleOrView "dash"
    refresh'         = refresh
    firstLayout      = setLayout $ layoutHook conf
    nextLayout       = sendMessage NextLayout
    fullscreen       = sendMessage $ Toggle ZOOM
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
    openTerminal     = spawn $ terminal conf
    openDmenu        = spawn "dmenu_run"
    searchPrompt     = promptSearch defaultXPConfig google
    searchSelection  = selectSearch google

    k key m ms mc msc =
        [ bind "M-"      key m
        , bind "M-S-"    key ms
        , bind "M-C-"    key mc
        , bind "M-S-C-"  key msc
        ]
      where
        bind mod key cmd = (mod ++ key, cmd)

workspaceMap :: XConfig l -> [(String, X ())]
workspaceMap conf =
   [(mod ++ key, windows $ cmd tag)
       | (tag, key) <- zip myWorkspaces $ map show [1..]
       , (cmd, mod) <- [(W.greedyView,"M-"), (W.shift,"M-S-"), (swapWithCurrent,"M-C-")]]

screenMap :: XConfig l -> [(String, X ())]
screenMap conf =
   [(mod ++ key, screenWorkspace scr >>= flip whenJust (windows . cmd))
       | (key, scr) <- zip ["w", "e", "r"] [0..]
       , (cmd, mod) <- [(W.view, "M-"), (W.shift, "M-S-")]]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = mkKeymap conf $ concat $ map ($ conf) myKeyMaps

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings conf = M.fromList $ concat $ map ($ conf) myMouseMaps


------------------------------------------------------------------------
-- LOGGING
------------------------------------------------------------------------
myLogHook :: XConfig l -> Handle -> Handle -> X ()
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

myTopBar :: String
myTopBar    = "/usr/bin/xmobar " ++ home ++ "xmobar-top"

myBottomBar :: String
myBottomBar = "/usr/bin/xmobar " ++ home ++ "xmobar-bottom"


------------------------------------------------------------------------
-- STARTUP
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook =  setDefaultCursor xC_left_ptr
             <+> setWMName "LG3D"

