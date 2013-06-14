{-# OPTIONS_HADDOCK ignore-exports #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Rayl.Keymaps
-- Copyright    : (c) 2013 Ray Lehtiniemi
-- License      : BSD3-style (see LICENSE)
--
-- Maintainer   : Ray Lehtiniemi <rayl@mail.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- This module provides tables implementing my preferred keybindings.
-----------------------------------------------------------------------------
module XMonad.Config.Rayl.Keymaps (
    mkShortcutMap,
    navigationMap,
    mouseMap,
    layoutMap,
    mouseLayoutMap,
    keyboardMap
    ) where


import XMonad                            hiding ((|||))
import qualified XMonad.StackSet as W    -- many

import Graphics.X11                      -- keysyms,etc
import System.Exit                       (exitWith,ExitCode(ExitSuccess))

import XMonad.Actions.CycleWS            (WSType(HiddenWS),Direction1D(..),
                                          screenBy,findWorkspace)
import XMonad.Actions.DynamicWorkspaces  (addHiddenWorkspace,renameWorkspace,
                                          removeEmptyWorkspaceAfterExcept)
import XMonad.Actions.Search             (promptSearch,selectSearch,google)
import XMonad.Actions.WindowBringer      (bringMenu,gotoMenu)

import XMonad.Config.Rayl.Layout         (MyTransformers(ZOOM))

import XMonad.Hooks.ManageDocks          (ToggleStruts(..))
import XMonad.Hooks.UrgencyHook          (focusUrgent)

import XMonad.Layout.LayoutCombinators   ((|||),JumpToLayout(..))
import XMonad.Layout.MouseResizableTile  (MRTMessage(ExpandSlave,ShrinkSlave))
import XMonad.Layout.MultiToggle         (Toggle(..))
import XMonad.Layout.WorkspaceDir        (changeDir)

import XMonad.Prompt                     (defaultXPConfig,autoComplete)
import XMonad.Prompt.Workspace           (workspacePrompt)

import XMonad.Util.Keymap                (bindString,bindButton)
import XMonad.Util.WorkspaceCompare      (getSortByIndex)


-- * Focus control
-- $
-- Focus control allows navigation between windows, screens and workspaces.
-- Full keyboard and partial trackball bindings are available.
-- With a few trackball exceptions, control is done using just Mod plus one of several command keys.
--
-- The focus policy is strictly FocusFollowsMouse and MouseFollowsFocus. 
-- It is not possible for the mouse pointer and the focused window to diverge.


-- ** Window
-- $
-- On the keyboard, window focus is set with J, K and M.
-- Mod-J and Mod-K move the focus around the current screen.
-- Windows are visited in stack order.
-- Mod-M warps focus to the master window.
--
-- On the trackball, window focus is set with the wheel.
-- Hold Mod- and roll the wheel back and forth to move the focus around the current screen.
-- Windows are visited in stack order.
-- Hold Mod-C- and click the wheel to warp focus to the master window.

viewNextWindow   = windows W.focusDown
viewPrevWindow   = windows W.focusUp
viewMainWindow   = windows W.focusMaster



-- ** Screen
-- $
-- On the keyboard, screen focus is set with L.
-- Mod-L jumps to the next screen.
-- Navigation is forward-only, cycling at the end of the screen list.
-- On a dual-head system, this acts as a simple back-and-forth toggle between screens.
-- 
-- On the trackball, screen focus is set with the wheel.
-- Hold Mod- and click the wheel to jump to the next screen.

viewNextScreen   = relScreen 1 view



-- ** Workspace
-- $
-- On the keyboard, workspace focus is set with U and I.
-- Mod-U and Mod-I cycle around the hidden workspaces.
-- Hidden workspaces are visited in the order shown on the workspace list at the bottom.
-- Visible workspaces are skipped, as they are accessible by changing screen focus.
--
-- On the trackball, workspace focus is set with the wheel.
-- Hold Mod-C- and roll the wheel back and forth to cycle around the hidden workspaces.

viewNextWSpace   = nuke $ withNextWS view
viewPrevWSpace   = nuke $ withPrevWS view

-- $
-- Special workspace access is available on the keyboard using grave, Esc, O, and N.
-- Mod-<grave> bounces back and forth between the last visited workspace.
-- Mod-<Esc> jumps to a workspace with an urgent window.
-- Mod-O prompts (with autocompletion) for a workspace name.
-- When a unique, existing workspace prefix is detected, focus immediately jumps to that workspace.
-- Screen focus may change if the workspace is currently visible.
-- Mod-N prompts for a fresh workspace name to be created.

viewLastWSpace   = nuke $ withLastWS view
viewUrgnWSpace   = nuke $ focusUrgent
viewSomeWSpace   = nuke $ withSomeWS view
viewFrshWSpace   = nuke $ withFrshWS view


-- $
-- Direct access to fixed workspaces can be arranged with 'mkShortcutMap'. This
-- allows single key jumps to 'Main.myWorkspaces' using Mod- plus 'Main.myWsShortcuts'.


-- * Window motion
-- ** Dragging
-- $
-- Windows can be dragged around by holding Shift while moving the focus.
-- Place focus on the window to be moved, press Shift, and navigate to the desired location.
-- The window will be dragged along as the focus moves.
-- Any focus control (including fixed shotcuts, except Esc) can be augmented with Shift to drag the focused window.

dragNextWindow   = windows W.swapDown
dragPrevWindow   = windows W.swapUp
dragMainWindow   = windows W.shiftMaster
dragNextScreen   = sendNextScreen >> viewNextScreen
dragNextWSpace   = nuke $ withNextWS drag
dragPrevWSpace   = nuke $ withPrevWS drag
dragLastWSpace   = nuke $ withLastWS drag
dragSomeWSpace   = nuke $ withSomeWS drag
dragFrshWSpace   = nuke $ withFrshWS drag



-- ** Sending
-- $
-- Windows can also be moved by holding Control and using many keyboard focus commands.
-- In this case, the window is sent away while the focus remains stationary.
-- Any keyboard focus control (including fixed shortcuts, except J, K, and Esc) can be augmented with Control.
-- Window sending commands are not mapped to the trackball.

sendNextScreen   = relScreen 1 send
sendNextWSpace   = withNextWS send
sendPrevWSpace   = withPrevWS send
sendSomeWSpace   = withSomeWS send
sendFrshWSpace   = withFrshWS send
sendLastWSpace   = withLastWS send



-- * Layout control
-- $
-- Toggle fullscreen with Mod-button1. Toggle status bars with Mod-button3.
fullscreen       = sendMessage $ Toggle ZOOM
toggleStruts     = sendMessage $ ToggleStruts

-- $
-- Adjust width of the master window using Mod-S- with button1 and button3.
expandMaster     = sendMessage $ Expand
shrinkMaster     = sendMessage $ Shrink

-- $
-- Adjust number of windows in the current column using Mod-C- with button1 and button3.
incMaster        = sendMessage $ IncMasterN 1
decMaster        = sendMessage $ IncMasterN (-1)

-- $
-- Other layout stuff...
expandSlave      = sendMessage $ ExpandSlave
shrinkSlave      = sendMessage $ ShrinkSlave
nextLayout       = sendMessage $ NextLayout

-- * Misc
-- ** Keyboard
-- $

-- ** Trackball
-- $
-- Use Mod-S-C-button1 to open a new terminal above the current window.
-- 
-- Use Mod-S-C-button3 to kill the current window.
restartXmonad    = restart "xmonad" True
resetXmonad      = restart "xmonad" False
quitXmonad       = io $ exitWith ExitSuccess

refresh'         = refresh

nameWorkspace    = renameWorkspace defaultXPConfig

killWindow'      = kill
sinkWindow       = withFocused (windows . W.sink)

gotoMenu'        = gotoMenu
bringMenu'       = bringMenu

openChrome       = spawn "google-chrome"
openDmenu        = spawn "dmenu_run"

searchPrompt     = promptSearch defaultXPConfig google
searchSelection  = selectSearch google

chdir            = changeDir defaultXPConfig









-- * Predefined keytables
-- $
-- Several predefined keytables are exported.
-- These tables provide access to the routines described above via my preferred keybindings.

-- | Create a binding table for hotkey access to stable workspaces. The
-- list of shortcuts must be key names acceptable to 'mkKeyMap'
mkShortcutMap :: [WorkspaceId] -> [String] -> XConfig l -> [(String, X ())]
mkShortcutMap ws sc = \ c ->
        [(mod ++ key, cmd $ tag)
              | (tag, key) <- zip ws sc
              , (mod, cmd) <- actions]
           where
               actions = [("M-", nuke . view),("M-S-", nuke . drag),("M-C-",send)]

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
  , k "n"            viewFrshWSpace   dragFrshWSpace   sendFrshWSpace   __
  ]
  where
    k = bindString ""



-- | Binding table for trackball navigation, window motion, and some layout control
mouseMap :: XConfig Layout -> [((KeyMask, Button), (Window -> X ()))]
mouseMap conf = concat
  --  button         M-               M-S-             M-C-             M-S-C-
  [ k button1        fullscreen       shrinkMaster     incMaster        openTerminal
  , k button2        viewNextScreen   dragNextScreen   viewMainWindow   dragMainWindow
  , k button3        toggleStruts     expandMaster     decMaster        killWindow'
  , k button4        viewPrevWindow   dragPrevWindow   viewPrevWSpace   dragPrevWSpace
  , k button5        viewNextWindow   dragNextWindow   viewNextWSpace   dragNextWSpace
  ] 
  where
    k = bindButton (modMask conf)
    openTerminal     = spawn $ terminal conf



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
    k = bindString "M1-"
    resetLayout      = setLayout $ layoutHook conf
    selectLayout     = __ -- layoutPrompt acXPConfig (\ l -> sendMessage $ JumpToLayout l)



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
    k = bindButton ((modMask conf) .|. mod1Mask)
    resetLayout      = setLayout $ layoutHook conf



-- | Binding table for other keyboard commands
keyboardMap :: XConfig Layout -> [(String, X ())]
keyboardMap conf = concat
  --  keysym         M-               M-S-             M-C-             M-S-C-
  [ k "-"            __               __               __               __
  , k "="            __               __               __               __
  , k "<Backspace>"  killWindow'      __               __               __

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
  , k "h"            nameWorkspace    __               __               __
  , k ";"            __               __               __               __
  , k "'"            __               __               __               __
  , k "<Return>"     __               __               __               __

  , k "z"            fullscreen       toggleStruts     __               __
  , k "c"            chdir            __               __               __
  , k "v"            __               __               __               __
  , k "b"            __               __               __               __
  , k "/"            __               __               __               __

  , k "<Home>"       __               __               __               __
  , k "<End>"        __               __               __               __
  , k "<Delete>"     __               __               __               __
  , k "<Page_Up>"    __               __               __               __
  , k "<Page_Down>"  __               __               __               __

  ]
  where
    k = bindString ""

    openTerminal     = spawn $ terminal conf






-- * Utility functions
-- $
-- A number of small helper functions used in the actions described above.


acXPConfig = defaultXPConfig { autoComplete = Just 1 }

withNextWS f = findWorkspace getSortByIndex Next HiddenWS 1 >>= f
withPrevWS f = findWorkspace getSortByIndex Prev HiddenWS 1 >>= f
withLastWS f = gets ((dfl W.tag "") . W.hidden . windowset) >>= f
withSomeWS   = workspacePrompt acXPConfig
withFrshWS f = workspacePrompt defaultXPConfig (\ w -> addHiddenWorkspace w >> f w)

dfl f d l = case l of [] -> d; w:ws -> f w

relScreen n f = return n
            >>= screenBy
            >>= screenWorkspace
            >>= flip whenJust f

nuke f = asks (workspaces . config) >>= \ s -> removeEmptyWorkspaceAfterExcept s f

view = windows . W.view
send = windows . W.shift
drag = \ w -> send w >> view w

__   = return ()
