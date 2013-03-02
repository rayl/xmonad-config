{-# OPTIONS_HADDOCK prune #-}
{- |
Module      :  xmonad.hs
Description :  Ray's XMonad configuration
Copyright   :  (c) 2013 Ray Lehtiniemi
License     :  BSD3-style (see LICENSE)

Maintainer  :  rayl@mail.com
Stability   :  unstable
Portability :  non-portable

This XMonad configuration is optimized for:

   - desktop machine

   - dual 1080p monitors

   - Windows keyboard

   - Logitech Trackman Wheel

The core workflow is expected to revolve around:

   - bash

   - vim 

   - google-chrome

   - libreoffice

-}

-- Uncomment to run Haddock on this file. Recomment in order to make --recompile work...
-- module XMonadHS where

import Data.Char                         (toUpper)
import Data.Function                     (on)
import qualified Data.List as L          (intersperse,find)
import qualified Data.Map as M           (Map,fromList,lookup,union)
import Data.Maybe                        (fromMaybe)
import Data.Monoid                       (All,mconcat,mempty)
import System.Directory                  (getCurrentDirectory)
import System.Exit                       (exitWith,ExitCode(ExitSuccess))
import System.IO                         (hPutStrLn, Handle)
import XMonad                            -- many
import XMonad.Actions.CycleWS            (moveTo,
                                          WSType(HiddenWS),Direction1D(..),
                                          shiftTo, screenBy)
import XMonad.Actions.DynamicWorkspaces  (selectWorkspace,
                                          removeEmptyWorkspaceAfterExcept,
                                          renameWorkspace)
import XMonad.Actions.Search             (promptSearch,selectSearch,google)
import XMonad.Actions.UpdatePointer      (updatePointer,PointerPosition(Relative))
import XMonad.Actions.Warp               (warpToWindow)
import XMonad.Actions.WindowBringer      (bringMenu,gotoMenu)
import XMonad.Hooks.DynamicLog           -- many
import XMonad.Hooks.EwmhDesktops         (ewmh,fullscreenEventHook)
import XMonad.Hooks.ManageDocks          (manageDocks,docksEventHook,
                                          avoidStruts,ToggleStruts(..))
import XMonad.Hooks.SetWMName            (setWMName)
import XMonad.Hooks.UrgencyHook          (withUrgencyHook, NoUrgencyHook(..),focusUrgent)
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
import XMonad.Layout.WorkspaceDir        (workspaceDir,changeDir)
import XMonad.Prompt                     (defaultXPConfig,autoComplete)
import XMonad.Prompt.Workspace           (workspacePrompt)
import qualified XMonad.StackSet as W    -- many
import XMonad.Util.Cursor                (setDefaultCursor)
import XMonad.Util.EZConfig              (mkKeymap)
import XMonad.Util.NamedWindows          (getName)
import XMonad.Util.Run                   (spawnPipe)
import XMonad.Util.WorkspaceCompare      (mkWsSort,getWsIndex)


------------------------------------------------------------------------
-- * Workspaces
------------------------------------------------------------------------
-- $
-- Core tasks like Gmail, Calendar, Reader, music, etc live on a set of /fixed workspaces/.
-- These workspaces are defined by 'myWorkspaces'.
-- They are directly accessible via hotkeys defined in 'myWsShortcuts'.
-- They appear first in the workspace list on the bottom status bar, labeled with their hotkey.
-- 
-- All other tasks live in a variable list of dynamic workspaces.
-- These workspaces appear after the fixed workspaces in the workspace list.
-- They do not have direct hotkey access.

-- | The list of fixed workspaces. 
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["dash","todo","news","book","song"]

-- | The hotkeys used for direct access to fixed workspaces.
--   Also used for labeling them in the workspace list on the bottom bar.
myWsShortcuts = map show $ [1..9] ++ [0]



------------------------------------------------------------------------
-- * Bindings
------------------------------------------------------------------------

-- | Mod-4 is the Win key available on most PC keyboards.
myModMask = mod4Mask


-- ** Focus control
-- $
-- Focus control allows navigation between windows, screens and workspaces.
-- Full keyboard and partial trackball bindings are available.
-- With a few trackball exceptions, control is done using just Mod plus one of several command keys.
--
-- The focus policy is strictly FocusFollowsMouse and MouseFollowsFocus. 
-- It is not possible for the mouse pointer and the focused window to diverge.


-- *** Keyboard
-- $
-- Window focus is set with J, K and M.
-- Mod-J and Mod-K move the focus around the current screen.
-- Windows are visited in stack order.
-- Mod-M warps focus to the master window.
-- 
-- Screen focus is set with L.
-- Mod-L jumps to the next screen.
-- Navigation is forward-only, cycling at the end of the screen list.
-- On a dual-head system, this acts as a simple back-and-forth toggle between screens.
-- 
-- Workspace focus is set with U and I.
-- Mod-U and Mod-I cycle around the hidden workspaces.
-- Hidden workspaces are visited in the order shown on the workspace list at the bottom.
-- Visible workspaces are skipped, as they are accessible by changing screen focus.
--
-- Special workspace access is available with grave, Esc, and O.
-- Mod-<grave> bounces back and forth between the last visited workspace.
-- Mod-<Esc> jumps to a workspace with an urgent window.
-- Mod-O prompts (with autocompletion) for a workspace name.
-- When a unique, existing workspace prefix is detected, focus immediately jumps to that workspace.
-- Screen focus may change if the workspace is currently visible.


-- *** Trackball
-- $
-- Window focus is set with the wheel.
-- Hold Mod- and roll the wheel back and forth to move the focus around the current screen.
-- Windows are visited in stack order.
-- Hold Mod-C- and click the wheel to warp focus to the master window.
-- 
-- Screen focus is set with the wheel.
-- Hold Mod- and click the wheel to jump to the next screen.
-- Navigation is forward-only, cycling at the end of the screen list.
-- On a dual-head system, this acts as a simple back-and-forth toggle between screens.
-- 
-- Workspace focus is set with the wheel.
-- Hold Mod-C- and roll the wheel back and forth to cycle around the hidden workspaces.
-- Hidden workspaces are visited in the order shown on the workspace list at the bottom.
-- Visible workspaces are skipped, as they are accessible by changing screen focus.


-- ** Window motion
-- *** Dragging
-- $
-- Windows can be dragged around by holding Shift while moving the focus.
-- Place focus on the window to be moved, press Shift, and navigate to the desired location.
-- The window will be dragged along as the focus moves.
-- Any focus control (except view urgent) can be augmented with Shift to drag the focused window.

-- *** Sending
-- $
-- Windows can also be moved by holding Control and using many keyboard focus commands.
-- In this case, the window is sent away while the focus remains stationary.
-- Any keyboard focus control (except J, K, and Esc) can be augmented with Control.
-- Window sending commands are not mapped to the trackball.

-- ** Layout control
-- *** Keyboard
-- $

-- *** Trackball
-- $
-- Toggle fullscreen with Mod-button1. Toggle status bars with Mod-button3.
-- 
-- Adjust width of the master window using Mod-S- with button1 and button3.
-- 
-- Adjust number of windows in the current column using Mod-C- with button1 and button3.

-- ** Misc
-- *** Keyboard
-- $

-- *** Trackball
-- $
-- Use Mod-S-C-button1 to open a new terminal above the current window.
-- 
-- Use Mod-S-C-button3 to kill the current window.

-- | Binding table for keyboard navigation and window motion
navigationMap :: XConfig Layout -> [(String, X ())]
navigationMap conf = concat
  --  keysym         M-               M-S-             M-C-             M-S-C-
  [ k "<Esc>"        viewUrgnWSpace   __               __               __
  , k "`"            viewLastWSpace   dragLastWSpace   sendLastWSpace   __
  , k "u"            viewPrevWSpace   dragPrevWSpace   sendPrevWSpace   __
  , k "i"            viewNextWSpace   dragNextWSpace   sendNextWSpace   __
  , k "o"            viewSomeWSpace   dragSomeWSpace   sendSomeWSpace   __
  , k "j"            viewNextWindow   dragNextWindow   __               __
  , k "k"            viewPrevWindow   dragPrevWindow   __               __
  , k "l"            viewNextScreen   dragNextScreen   sendNextScreen   __
  , k "m"            viewMainWindow   dragMainWindow   __               __
  ]
  where

    viewNextWindow   = windows W.focusDown
    dragNextWindow   = windows W.swapDown

    viewPrevWindow   = windows W.focusUp
    dragPrevWindow   = windows W.swapUp

    viewMainWindow   = windows W.focusMaster
    dragMainWindow   = windows W.shiftMaster

    viewNextScreen   = relScreen 1 view
    dragNextScreen   = sendNextScreen >> viewNextScreen
    sendNextScreen   = relScreen 1 send

    viewNextWSpace   = moveTo Next HiddenWS
    dragNextWSpace   = sendNextWSpace >> viewNextWSpace
    sendNextWSpace   = shiftTo Next HiddenWS

    viewPrevWSpace   = moveTo Prev HiddenWS
    dragPrevWSpace   = sendPrevWSpace >> viewPrevWSpace
    sendPrevWSpace   = shiftTo Prev HiddenWS

    viewSomeWSpace   = withSomeWS view
    dragSomeWSpace   = withSomeWS drag
    sendSomeWSpace   = withSomeWS send

    viewLastWSpace   = withLastWS view
    dragLastWSpace   = withLastWS drag
    sendLastWSpace   = withLastWS send

    viewUrgnWSpace   = focusUrgent

    view             = windows . W.view
    send             = windows . W.shift
    drag             = \ w -> send w >> view w

    withLastWS f     = gets ((dfl W.tag "") . W.hidden . windowset) >>= f
    withSomeWS       = workspacePrompt defaultXPConfig { autoComplete = Just 1 }

    relScreen n f    = return n
                   >>= screenBy
                   >>= screenWorkspace
                   >>= flip whenJust f

    dfl f d l = case l of [] -> d; w:ws -> f w

    __ = return ()
    k = bindString ""

-- | Binding table for trackball navigation, window motion, and some layout control
mouseMap :: XConfig l -> [((KeyMask, Button), (Window -> X ()))]
mouseMap conf = concat
  --  button         M-               M-S-             M-C-             M-S-C-
  [ k button1        zoomWindow       shrinkMaster     incMaster        openTerminal
  , k button2        viewNextScreen   dragNextScreen   viewMainWindow   dragMainWindow
  , k button3        toggleStruts     expandMaster     decMaster        killWindow
  , k button4        viewPrevWindow   dragPrevWindow   viewPrevWSpace   dragPrevWSpace
  , k button5        viewNextWindow   dragNextWindow   viewNextWSpace   dragNextWSpace
  ] 
  where

    viewNextWindow   = b $ W.focusDown
    dragNextWindow   = b $ W.swapDown

    viewPrevWindow   = b $ W.focusUp
    dragPrevWindow   = b $ W.swapUp

    viewMainWindow   = b $ W.focusMaster
    dragMainWindow   = b $ W.shiftMaster

    viewNextScreen   = a $ relScreen 1 view
    dragNextScreen   = a $ relScreen 1 drag

    viewNextWSpace   = a $ viewNextWS
    dragNextWSpace   = a $ sendNextWS >> viewNextWS

    viewPrevWSpace   = a $ viewPrevWS
    dragPrevWSpace   = a $ sendPrevWS >> viewPrevWS

    zoomWindow       = c $ Toggle ZOOM
    toggleStruts     = c $ ToggleStruts

    expandMaster     = c $ Expand
    shrinkMaster     = c $ Shrink

    incMaster        = c $ IncMasterN 1
    decMaster        = c $ IncMasterN (-1)

    openTerminal     = a $ spawn $ terminal conf
    killWindow       = a $ kill

    viewNextWS       = moveTo Next HiddenWS
    viewPrevWS       = moveTo Prev HiddenWS
    sendNextWS       = shiftTo Next HiddenWS
    sendPrevWS       = shiftTo Prev HiddenWS

    view             = windows . W.view
    send             = windows . W.shift
    drag             = \ w -> send w >> view w

    relScreen n f    = return n
                   >>= screenBy
                   >>= screenWorkspace
                   >>= flip whenJust f

    a x  = \ _ -> x
    b x  = \ _ -> windows x
    c x  = \ _ -> sendMessage x
 -- __   = \ _ -> return ()
    k = bindButton 0

-- | Binding table for keyboard layout control
layoutMap :: XConfig Layout -> [(String, X ())]
layoutMap conf = concat
  --  keysym         M-               M-S-             M-C-             M-S-C-
  [ k "j"            __               __               __               __
  , k "k"            __               __               __               __
  , k "l"            __               __               __               __
  , k "z"            fullscreen       toggleStruts     __               __
  , k ","            incMaster        __               __               __
  , k "."            decMaster        __               __               __
  , k "<Space>"      nextLayout       resetLayout      __               refresh'
  , k "<Up>"         shrinkSlave      __               __               __
  , k "<Down>"       expandSlave      __               __               __
  , k "<Left>"       shrinkMaster     __               __               __
  , k "<Right>"      expandMaster     __               __               __
  ]
  where

    refresh'         = refresh
    resetLayout      = setLayout $ layoutHook conf
    nextLayout       = sendMessage NextLayout
    fullscreen       = sendMessage (Toggle ZOOM)
    toggleStruts     = sendMessage ToggleStruts
    expandMaster     = sendMessage Expand
    shrinkMaster     = sendMessage Shrink
    expandSlave      = sendMessage ExpandSlave
    shrinkSlave      = sendMessage ShrinkSlave
    incMaster        = sendMessage (IncMasterN 1)
    decMaster        = sendMessage (IncMasterN (-1))

    __ = return ()
    k = bindString "M1-"

mouseLayoutMap :: XConfig l -> [((KeyMask, Button), (Window -> X ()))]
mouseLayoutMap conf = concat
  --  button         M-               M-S-             M-C-             M-S-C-
  [ k button1        __               __               __               __
  , k button2        __               __               __               __
  , k button3        __               __               __               __
  , k button4        openTerminal     __               __               __
  , k button5        __               __               __               __
  ]
  where

    openTerminal     = a $ spawn $ terminal conf

    a x  = \ _ -> x
    __   = \ _ -> return ()
    k = bindButton mod1Mask

-- | Binding table for other keyboard commands
keyboardMap :: XConfig Layout -> [(String, X ())]
keyboardMap conf = concat
  --  keysym         M-               M-S-             M-C-             M-S-C-
  [ k "-"            __               __               __               __
  , k "="            __               __               __               __
  , k "<Backspace>"  killWindow       __               __               __

  , k "<Tab>"        __               __               __               __
  , k "q"            restartXmonad    resetXmonad      __              quitXmonad
  , k "w"            __               __               __               __
  , k "e"            __               __               __               __
  , k "r"            __               __               __               __
  , k "t"            sinkWindow       __               __               __
  , k "y"            __               __               __               __
  , k "p"            gotoMenu'        bringMenu'       __               __
  , k "["            __               __               __               __
  , k "]"            __               __               __               __
  , k "\\"           __               __               __               __

  , k "a"            openTerminal     openChrome       openDmenu        __
  , k "s"            searchSelection  searchPrompt     __               __
  , k "d"            __               __               __               __
  , k "f"            __               __               __               __
  , k "g"            __               __               __               __
  , k "h"            __               __               __               __
  , k ";"            __               __               __               __
  , k "'"            __               __               __               __
  , k "<Return>"     __               __               __               __

  , k "z"            fullscreen       toggleStruts     __               __
  , k "x"            __               __               __               __
  , k "c"            chdir            __               __               __
  , k "v"            __               __               __               __
  , k "b"            __               __               __               __
  , k "n"            newWorkspace     nameWorkspace    nukeWorkspace    __
  , k "/"            __               __               __               __

  , k "<Home>"       __               __               __               __
  , k "<End>"        __               __               __               __
  , k "<Delete>"     __               __               __               __
  , k "<Page_Up>"    __               __               __               __
  , k "<Page_Down>"  __               __               __               __

  ]
  where

    restartXmonad    = restart "xmonad" True
    resetXmonad      = restart "xmonad" False
    quitXmonad       = io $ exitWith ExitSuccess

    newWorkspace     = selectWorkspace defaultXPConfig
    nameWorkspace    = renameWorkspace defaultXPConfig
    nukeWorkspace    = removeEmptyWorkspaceAfterExcept myWorkspaces viewNextWSpace
    viewNextWSpace   = moveTo Next HiddenWS

    killWindow       = kill
    sinkWindow       = withFocused sink
    gotoMenu'        = gotoMenu
    bringMenu'       = bringMenu

    sink             = windows . W.sink

    openTerminal     = spawn $ terminal conf
    openChrome       = spawn "google-chrome"
    openDmenu        = spawn "dmenu_run"
    searchPrompt     = promptSearch defaultXPConfig google
    searchSelection  = selectSearch google

    fullscreen       = sendMessage (Toggle ZOOM)
    toggleStruts     = sendMessage ToggleStruts

    chdir            = changeDir defaultXPConfig

    __ = return ()
    k = bindString ""

-- | Binding table for hotkey access to stable workspaces
shortcutMap :: XConfig l -> [(String, X ())]
shortcutMap conf =
        [(mod ++ key, cmd $ tag)
              | (tag, key) <- zip myWorkspaces myWsShortcuts
              , (mod, cmd) <- actions]
           where
               actions = [("M-",view),("M-S-",drag),("M-C-",send)]
               view    = windows . W.view
               send    = windows . W.shift
               drag    = \ w -> send w >> view w



bindString :: String -> String -> X () -> X () -> X () -> X () -> [(String, X ())]
bindString p key m ms mc msc =
        [ bind ""      key m
        , bind "S-"    key ms
        , bind "C-"    key mc
        , bind "S-C-"  key msc
        ]
           where
               bind mod key cmd = ("M-" ++ p ++ mod ++ key, cmd)

bindButton :: KeyMask -> Button -> (Window -> X ()) -> (Window -> X ()) -> (Window -> X ()) -> (Window -> X ())
           -> [((KeyMask,Button), (Window -> X ()))]
bindButton p but m ms mc msc =
        [ bind m'    but m
        , bind ms'   but ms
        , bind mc'   but mc
        , bind msc'  but msc
        ]
           where
               bind mod but cmd = ((p .|. mod,but), cmd)
               m'   = myModMask
               ms'  = myModMask .|. shiftMask
               mc'  = myModMask .|.               controlMask
               msc' = myModMask .|. shiftMask .|. controlMask


-- | Compile all keyboard maps into a keys hook
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = mkKeymap conf $ concat $ map ($ conf)
       [ navigationMap
       , shortcutMap
       , layoutMap
       , keyboardMap
       ]

-- | Compile all mouse maps into a mouseBindings hook
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings conf = M.fromList $ concat $ map ($ conf)
       [ mouseMap
       , mouseLayoutMap
       ] 


------------------------------------------------------------------------
-- * Layouts
-- ** Core layouts
-- ** Modifiers
-- ** Per-workspace layouts
-- ** Toggles
-- ** Other features
------------------------------------------------------------------------
myLayoutHook = id
             $ avoidStruts
             $ workspaceDir "~"
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
-- * Logging
-- In progress.
------------------------------------------------------------------------
myTemplates :: Int -> Pos -> (String,String,String)
myTemplates s p | p == T = headerTmpl
                | p == B = footerTmpl
   where
      headerTmpl = 
         ( xmobarColor "black" "lightblue" . wrap " " " " $ (show s)
         , "%StdinReader%"
         , "<fc=#ee9a00>%date%</fc>"
         )
      footerTmpl =
         ( "%StdinReader%"
         , ""
         , "%multicpu% | %memory%%swap%"
         )

myCommands :: Int -> Pos -> [String]
myCommands s p | p == T = headerCmds
               | p == B = footerCmds
   where
      headerCmds = 
         [ "Run StdinReader"
         , "Run Date \"%a %Y-%m-%d %H:%M:%S\" \"date\" 10"
         ]
      footerCmds = 
         [ "Run StdinReader"
         , "Run MultiCpu [ \"-t\", \"CPU: <total>% (<user>%user, <nice>%nice, <system>%sys) [<autototal>]\",\
                         \ \"-L\", \"3\",\
                         \ \"-H\", \"50\",\
                         \ \"--normal\", \"green\",\
                         \ \"--high\",\"red\"\
                         \ ] 10"
         , "Run Memory   [\"-t\",\"MEM: <free>M free, <used>M used, \"] 10"
         , "Run Swap     [\"-t\", \"<used>M swap\"] 10"
         ]

data Pos = T | B deriving (Eq)

spawnBar :: Int -> Pos -> IO Handle
spawnBar s p = spawnPipe cmd
    where cmd = unwords [ "/usr/bin/xmobar"
                        , "-f -*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
                        , "-B black"
                        , "-F grey"
                        , pos
                        , "-a '}{'"
                        , "-s '%'"
                        , "-t", fmtt $ myTemplates s p
                        , "-c", fmtc $ myCommands s p
                        , "-x" , show s
                        ]
                    where fmtt (l,c,r) = wrap "'" "'" $ l ++ "}" ++ c ++ "{" ++ r
                          fmtc x = wrap "'[ " " ]'" $ unwords $ L.intersperse "," x
                          pos | p == T = "-o"
                              | p == B = "-b"

focusedTitleOnScreen :: ScreenId -> X (String -> String)
focusedTitleOnScreen n = do
    ws <- gets windowset
    let ss = (W.current ws) : (W.visible ws)
        s  = L.find ((n==) . W.screen) ss
        t  = maybe Nothing
                   (W.stack . W.workspace)
                   s
    m <- maybe (return "<empty>")
               (fmap show . getName . W.focus)
               t
    let x = if n == (W.screen . W.current) ws
               then xmobarColor "black" "green" . wrap "  " "  " $ m
               else xmobarColor "grey"  ""      . wrap "  " "  " $ m
    return (\ _ -> x)

workspaceOnScreen :: ScreenId -> X (String -> String)
workspaceOnScreen n = do
   w <- gets windowset
   d <- io getCurrentDirectory
   let tag = fromMaybe "<???>" $ W.lookupWorkspace n w
       foc = W.currentTag w
       fmt1 = if tag == foc then cur else vis
          where
            cur = xmobarColor "white" "blue" . wrap "  " "  " $ map toUpper tag
            vis = xmobarColor "grey" ""     . wrap "  " "  " $ tag
       fmt2 = if tag == foc then (" in " ++ cur) else ""
          where
            cur = xmobarColor "white" "" . wrap " " " " $ d
            vis = xmobarColor "grey" ""     . wrap " " " " $ d
   return $ \ _ -> fmt1 ++ fmt2

myLogHook :: XConfig l -> Handle -> Handle -> Handle -> Handle -> X ()
myLogHook c u0 d0 u1 d1 = do
    g0 <- focusedTitleOnScreen 0
    g1 <- focusedTitleOnScreen 1
    h0 <- workspaceOnScreen 0
    h1 <- workspaceOnScreen 1

    mempty
       <+> dynamicLogWithPP (topPP u0 g0 h0)
       <+> dynamicLogWithPP (topPP u1 g1 h1)
       <+> dynamicLogWithPP bottomPP
       <+> updatePointer (Relative 0.5 0.5)
       <+> logHook c

             where
                topPP u g h = defaultPP
                   { ppOutput   = hPutStrLn u
                   , ppOrder    = \ (ws:l:t:_) -> [ws,t]
                   , ppCurrent  = h
                   , ppVisible  = const ""
                   , ppHidden   = const ""
                   , ppHiddenNoWindows = const ""
                   , ppWsSep    = ""
                   , ppTitle    = g
                   }

                bottomPP = defaultPP
                   { ppOutput   = \ s -> hPutStrLn d0 s >> hPutStrLn d1 s
                   , ppOrder    = \ (ws:l:t:_) -> [l,ws]
                   , ppSep      = " "
                   , ppLayout   = xmobarColor "black"  "#ccc"   . wrap "<" ">"
                   , ppWsSep    = " "
                   , ppSort     = mkWsSort cmp
                   , ppCurrent  = xmobarColor "white"  "blue" . wrap " " " " . shortcut
                   , ppVisible  = xmobarColor "black"  "blue" . wrap " " " " . shortcut
                   , ppHidden   = xmobarColor "white"  "" . shortcut
                   , ppHiddenNoWindows =
                                  xmobarColor "#444"   "black" . shortcut
                   , ppUrgent   = xmobarColor "black"  "yellow" . wrap " " " " . shortcut
                   }

                shortcut x = case (M.lookup x labels) of
                                Just i  -> i ++ "-" ++ x
                                Nothing -> x
                    where
                        labels = M.fromList $ zip myWorkspaces myWsShortcuts

                cmp = do wsIndex <- getWsIndex
                         return $ mconcat [foo `on` wsIndex, compare]
                    where
                        foo (Just a) (Just b) = compare a b
                        foo Nothing (Just _)  = GT
                        foo (Just _) Nothing  = LT
                        foo Nothing Nothing   = EQ


------------------------------------------------------------------------
-- * Manage hook
-- In progress.
------------------------------------------------------------------------
myManageHook :: XConfig l -> ManageHook
myManageHook c = mempty
                    <+> myManageHooks
                    <+> manageDocks

myManageHooks :: ManageHook
myManageHooks = composeAll
    [ className =? "Gimp"             --> unfloat
    ]
    where
       unfloat = ask >>= doF . W.sink


------------------------------------------------------------------------
-- * Event hook
-- In progress.
------------------------------------------------------------------------
myHandleEventHook :: XConfig l -> (Event -> X All)
myHandleEventHook c = mempty
                         <+> fullscreenEventHook
                         <+> docksEventHook
                         <+> handleEventHook c


------------------------------------------------------------------------
-- * Startup hook
------------------------------------------------------------------------
-- | Set the mouse cursor to a nice pointer.
-- Set WM name to LG3D for better Java compatibility.
myStartupHook :: X ()
myStartupHook = mempty
                   <+> setDefaultCursor xC_left_ptr
                   <+> setWMName "LG3D"


------------------------------------------------------------------------
-- * Main
------------------------------------------------------------------------
-- | Spawn xmobar processes, then run XMonad using local configuration options
--   defined in the following sections.
main :: IO ()
main = do
    topBar0    <- spawnBar 0 T
    bottomBar0 <- spawnBar 0 B
    topBar1    <- spawnBar 1 T
    bottomBar1 <- spawnBar 1 B
    xmonad $ ewmh
           $ withUrgencyHook NoUrgencyHook
           $ defaultConfig
        { borderWidth        = 2
        , workspaces         = myWorkspaces
        , layoutHook         = myLayoutHook
        , terminal           = "urxvt"
        , normalBorderColor  = "#222"
        , focusedBorderColor = "#ff4"
        , modMask            = myModMask
        , keys               = myKeys
        , logHook            = myLogHook defaultConfig topBar0 bottomBar0
                                                       topBar1 bottomBar1
        , startupHook        = myStartupHook
        , mouseBindings      = myMouseBindings
        , manageHook         = myManageHook defaultConfig
        , handleEventHook    = myHandleEventHook defaultConfig
        }


