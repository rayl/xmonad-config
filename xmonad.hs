{-# OPTIONS_HADDOCK ignore-exports #-}

-----------------------------------------------------------------------------
-- |
-- Module       : xmonad.hs
-- Description  :  Ray's XMonad configuration
-- Copyright    : (c) 2013 Ray Lehtiniemi
-- License      : BSD3-style (see LICENSE)
--
-- Maintainer   : Ray Lehtiniemi <rayl@mail.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- My XMonad configuration is optimized for:
-- 
--    - desktop machine
-- 
--    - dual 1080p monitors
-- 
--    - Windows keyboard
-- 
--    - Logitech Trackman Wheel
-- 
-- My core workflows revolve around:
-- 
--    - bash
-- 
--    - vim 
-- 
--    - google-chrome
-- 
--    - libreoffice
-----------------------------------------------------------------------------

module Main (main) where

import XMonad                            hiding ((|||))
import qualified XMonad.StackSet as W    -- many

import Data.Char                         (toUpper)
import Data.Function                     (on)
import qualified Data.List as L          (intersperse,find,isInfixOf)
import qualified Data.Map as M           (Map,fromList,lookup)
import Data.Maybe                        (fromMaybe)
import Data.Monoid                       (All,mconcat)
import Graphics.X11                      -- keysyms,etc
import System.Directory                  (getCurrentDirectory)
import System.IO                         (hPutStrLn,Handle)

import XMonad.Actions.UpdatePointer      (updatePointer,PointerPosition(Relative))

import XMonad.Config.Rayl.Layout         (MyTransformers(ZOOM))

import XMonad.Hooks.DynamicLog           -- many
import XMonad.Hooks.EwmhDesktops         (ewmh,fullscreenEventHook)
import XMonad.Hooks.ManageDocks          (manageDocks,docksEventHook, avoidStruts)
import XMonad.Hooks.SetWMName            (setWMName)
import XMonad.Hooks.UrgencyHook          (withUrgencyHook,NoUrgencyHook(..))

import XMonad.Layout.LayoutCombinators   ((|||))
import XMonad.Layout.IM                  (withIM,Property(Role))
import XMonad.Layout.MouseResizableTile  (mouseResizableTile,draggerType,
                                          DraggerType(BordersDragger))
import XMonad.Layout.MultiColumns        (multiCol)
import XMonad.Layout.MultiToggle         (mkToggle,single)
import XMonad.Layout.NoBorders           (smartBorders)
import XMonad.Layout.PerWorkspace        (onWorkspace)
import XMonad.Layout.Reflect             (reflectHoriz)
import XMonad.Layout.Renamed             (renamed,Rename(Replace,CutWordsLeft))
import XMonad.Layout.ResizableTile       (ResizableTall(..))
import XMonad.Layout.WorkspaceDir        (workspaceDir)

import XMonad.Prompt                     (XPConfig(..),XPrompt(..),mkXPrompt,mkComplFunFromList')

import XMonad.Util.Cursor                (setDefaultCursor)
import XMonad.Util.Keymap                (mkMyKeys,mkMyMouseBindings,bindString)
import XMonad.Util.NamedScratchpad       (NamedScratchpad(NS),namedScratchpadAction,
                                          namedScratchpadManageHook,customFloating)
import XMonad.Util.NamedWindows          (getName)
import XMonad.Util.Run                   (spawnPipe)
import XMonad.Util.WorkspaceCompare      (mkWsSort,getWsIndex)

import XMonad.Config.Rayl.Keymaps        (mkShortcutMap,navigationMap,mouseMap,layoutMap,
                                          mouseLayoutMap,keyboardMap)


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
myWsShortcuts :: [String]
myWsShortcuts = map show $ [1..9] ++ [0]



------------------------------------------------------------------------
-- * Bindings
------------------------------------------------------------------------

-- | Mod-4 is the Win key available on most PC keyboards.
myModMask = mod4Mask


-- | Compile all keyboard maps into a keys hook
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys = mkMyKeys
        [ navigationMap
        , mkShortcutMap myWorkspaces myWsShortcuts
        , namedScratchpadMap
        , layoutMap
        , keyboardMap
        ]

-- | Compile all mouse maps into a mouseBindings hook
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings = mkMyMouseBindings
        [ mouseMap
        , mouseLayoutMap
        ] 

namedScratchpadMap :: XConfig Layout -> [(String, X ())]
namedScratchpadMap conf = concat
  --  keysym         M-               M-S-             M-C-             M-S-C-
  [ k "x"            mixer            xprop            __               __
  ]
  where
    k = bindString ""
    mixer = namedScratchpadAction myScratchpads "mixer"
    xprop = withFocused (\ w -> spawn $ "urxvt -hold -e xprop -id " ++ show w)
    __ = return ()


------------------------------------------------------------------------
-- * Named scratchpads
------------------------------------------------------------------------
myScratchpads =
        [ NS "mixer"
             "urxvt -e alsamixer"
             (title =? "alsamixer")
             (customFloating $ W.RationalRect (1/12) (1/4) (5/6) (1/2))
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
             $ mainLayouts
  where

    mainLayouts = l_3COL ||| l_2COL ||| l_DRAG
    gimpLayouts = l_TALL

    gimpModify x = renamed [CutWordsLeft 3]
                 $ withIM (0.15) (Role "gimp-toolbox")
                 $ reflectHoriz
                 $ withIM (0.15) (Role "gimp-dock")
                 $ x

    l_3COL = name "3col" $ multiCol [1,1] 8 0.01 0.33
    l_2COL = name "2col" $ multiCol [1,2] 8 0.01 0.50
    l_DRAG = name "drag" $ mouseResizableTile { draggerType = BordersDragger }
    l_TALL = name "tall" $ ResizableTall 2 (1/118) (11/20) [1]

    name x = renamed [Replace x]

myLayouts = ["3col","2col","drag","tall"]

data Lay = Lay String

instance XPrompt Lay where
   showXPrompt (Lay x) = x

layoutPrompt :: XPConfig -> (String -> X ()) -> X ()
layoutPrompt c f = mkXPrompt (Lay "Choose layout: ") c (mkComplFunFromList' myLayouts) f


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

    idHook
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
myManageHook c = idHook
                    <+> myManageHooks
                    <+> namedScratchpadManageHook myScratchpads
                    <+> manageDocks

myManageHooks :: ManageHook
myManageHooks = composeAll
    [ className =? "Gimp"             --> unfloat
    ]
    where
       unfloat = ask >>= doF . W.sink

-- | @q =~ x@. if the result of @q@ contains @x@, return 'True'.
(=~) :: Eq a => Query [a] -> [a] -> Query Bool
q =~ x = fmap (x `L.isInfixOf`) q


------------------------------------------------------------------------
-- * Event hook
-- In progress.
------------------------------------------------------------------------
myHandleEventHook :: XConfig l -> (Event -> X All)
myHandleEventHook c = idHook
                         <+> fullscreenEventHook
                         <+> docksEventHook
                         <+> handleEventHook c


------------------------------------------------------------------------
-- * Startup hook
------------------------------------------------------------------------
-- | Set the mouse cursor to a nice pointer.
-- Set WM name to LG3D for better Java compatibility.
myStartupHook :: X ()
myStartupHook = idHook
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


