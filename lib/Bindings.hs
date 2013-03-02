
module Bindings
  ( navigationMap
  , mouseMap
  , layoutMap
  , mouseLayoutMap
  , keyboardMap
  ) where


import XMonad                            hiding ((|||))
import qualified XMonad.StackSet as W    -- many

import Graphics.X11                      -- keysyms,etc
import System.Exit                       (exitWith,ExitCode(ExitSuccess))

import XMonad.Actions.CycleWS            (moveTo,WSType(HiddenWS),
                                          Direction1D(..),shiftTo,screenBy)
import XMonad.Actions.DynamicWorkspaces  (selectWorkspace,renameWorkspace,
                                          removeEmptyWorkspaceAfterExcept)
import XMonad.Actions.Search             (promptSearch,selectSearch,google)
import XMonad.Actions.WindowBringer      (bringMenu,gotoMenu)

import XMonad.Hooks.ManageDocks          (ToggleStruts(..))
import XMonad.Hooks.UrgencyHook          (focusUrgent)

import XMonad.Layout.LayoutCombinators   ((|||),JumpToLayout(..))
import XMonad.Layout.MouseResizableTile  (MRTMessage(ExpandSlave,ShrinkSlave))
import XMonad.Layout.MultiToggle         (Toggle(..))
import XMonad.Layout.MyTransformers      (MyTransformers(ZOOM))
import XMonad.Layout.WorkspaceDir        (changeDir)

import XMonad.Prompt                     (defaultXPConfig,autoComplete)
import XMonad.Prompt.Workspace           (workspacePrompt)

import XMonad.Util.Keytable              (bindString,bindButton)


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
    withSomeWS       = workspacePrompt acXPConfig

    relScreen n f    = return n
                   >>= screenBy
                   >>= screenWorkspace
                   >>= flip whenJust f

    dfl f d l = case l of [] -> d; w:ws -> f w

    __ = return ()
    k = bindString ""

-- | Binding table for trackball navigation, window motion, and some layout control
mouseMap :: XConfig Layout -> [((KeyMask, Button), (Window -> X ()))]
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
    k = bindButton (modMask conf)

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
  , k "<Return>"     selectLayout     __               __               __
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
    selectLayout     = __ -- layoutPrompt acXPConfig (\ l -> sendMessage $ JumpToLayout l)

    __ = return ()
    k = bindString "M1-"

mouseLayoutMap :: XConfig Layout -> [((KeyMask, Button), (Window -> X ()))]
mouseLayoutMap conf = concat
  --  button         M-               M-S-             M-C-             M-S-C-
  [ k button1        fullscreen       __               __               __
  , k button2        __               __               __               resetLayout
  , k button3        toggleStruts     __               __               __
  , k button4        incMaster        expandMaster     expandSlave      __
  , k button5        decMaster        shrinkMaster     shrinkSlave      __
  ]
  where

    resetLayout      = a $ setLayout $ layoutHook conf
    nextLayout       = c $ NextLayout
    fullscreen       = c $ (Toggle ZOOM)
    toggleStruts     = c $ ToggleStruts
    expandMaster     = c $ Expand
    shrinkMaster     = c $ Shrink
    expandSlave      = c $ ExpandSlave
    shrinkSlave      = c $ ShrinkSlave
    incMaster        = c $ (IncMasterN 1)
    decMaster        = c $ (IncMasterN (-1))

    a x  = \ _ -> x
    b x  = \ _ -> windows x
    c x  = \ _ -> sendMessage x
    __   = \ _ -> return ()
    k = bindButton ((modMask conf) .|. mod1Mask)

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
    nukeWorkspace    = __ -- removeEmptyWorkspaceAfterExcept (asks workspaces) viewNextWSpace
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

acXPConfig = defaultXPConfig { autoComplete = Just 1 }

