
import Data.Char                         (toUpper)
import Data.Function                     (on)
import qualified Data.List as L          (intersperse,find)
import qualified Data.Map as M           (Map,fromList,lookup,union)
import Data.Monoid                       (All,mconcat)
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
import XMonad.Prompt                     (defaultXPConfig,autoComplete)
import XMonad.Prompt.Workspace           (workspacePrompt)
import qualified XMonad.StackSet as W    -- many
import XMonad.Util.Cursor                (setDefaultCursor)
import XMonad.Util.EZConfig              (mkKeymap)
import XMonad.Util.NamedWindows          (getName)
import XMonad.Util.Run                   (spawnPipe)
import XMonad.Util.WorkspaceCompare      (mkWsSort,getWsIndex)


myTerminal :: String
myTerminal = "urxvt"

main :: IO ()
main = do
    topBar0    <- spawnBar 0 T
    bottomBar0 <- spawnBar 0 B
    topBar1    <- spawnBar 1 T
    bottomBar1 <- spawnBar 1 B
    xmonad $ ewmh
           $ withUrgencyHook NoUrgencyHook
           $ defaultConfig
        { borderWidth        = myBorderWidth
        , workspaces         = myWorkspaces
        , layoutHook         = myLayoutHook
        , terminal           = myTerminal
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , modMask            = myModMask
        , keys               = myKeys
        , logHook            = myLogHook defaultConfig topBar0 bottomBar0
                                                       topBar1 bottomBar1
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
myWorkspaces = ["dash","todo","news","book","song"]


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

myWsShortcuts = map show $ [1..9] ++ [0]

myKeymaps  = [ navigationMap, shortcutMap, keyboardMap ]
myButmaps  = [ mouseMap ] 


mouseMap :: XConfig l -> [((KeyMask, Button), (Window -> X ()))]
mouseMap conf = concat
  --  button         M-               M-S-             M-C-             M-S-C-
  [ k button1        zoomWindow       openTerminal     incMaster        shrinkMaster
  , k button2        viewNextScreen   dragNextScreen   nextLayout       dragMainWindow
  , k button3        toggleStruts     killWindow       decMaster        expandMaster
  , k button4        viewPrevWindow   dragPrevWindow   viewPrevWSpace   dragPrevWSpace
  , k button5        viewNextWindow   dragNextWindow   viewNextWSpace   dragNextWSpace
  ] 
  where

    viewNextWindow   = a $ W.focusDown
    dragNextWindow   = a $ W.swapDown

    viewPrevWindow   = a $ W.focusUp
    dragPrevWindow   = a $ W.swapUp

    dragMainWindow   = a $ W.shiftMaster
    expandMaster     = c $ Expand
    shrinkMaster     = c $ Shrink

    viewNextScreen   = b $ relScreen 1 view
    dragNextScreen   = b $ relScreen 1 drag

    viewNextWSpace   = b $ viewNextWS
    dragNextWSpace   = b $ sendNextWS >> viewNextWS

    viewPrevWSpace   = b $ viewPrevWS
    dragPrevWSpace   = b $ sendPrevWS >> viewPrevWS

    zoomWindow       = c $ Toggle ZOOM
    toggleStruts     = c $ ToggleStruts

    nextLayout       = c $ NextLayout
    incMaster        = c $ IncMasterN 1
    decMaster        = c $ IncMasterN (-1)

    openTerminal     = d $ spawn $ terminal conf
    killWindow       = d $ kill

    viewNextWS       = moveTo Next HiddenWS
    viewPrevWS       = moveTo Prev HiddenWS
    sendNextWS       = shiftTo Next HiddenWS
    sendPrevWS       = shiftTo Prev HiddenWS
    fetchMouse       = warpToWindow 0.5 0.5

    view             = windows . W.view
    send             = windows . W.shift
    drag             = \w -> send w >> view w

    relScreen n f    = return n
                   >>= screenBy
                   >>= screenWorkspace
                   >>= flip whenJust f

    a x  = \_ -> windows x >> fetchMouse
    b x  = \_ -> x >> fetchMouse
    c x  = \w -> sendMessage x
    d x  = \w -> x

    --__ = \_ -> return ()
    k = bindButton

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
    drag             = \w -> send w >> view w

    withLastWS f     = gets ((dfl W.tag "") . W.hidden . windowset) >>= f
    withSomeWS       = workspacePrompt defaultXPConfig { autoComplete = Just 1 }

    relScreen n f    = return n
                   >>= screenBy
                   >>= screenWorkspace
                   >>= flip whenJust f

    dfl f d l = case l of [] -> d; w:ws -> f w

    __ = return ()
    k = bindString

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
  , k "x"            fetchMouse       __               __               __
  , k "c"            __               __               __               __
  , k "v"            __               __               __               __
  , k "b"            __               __               __               __
  , k "n"            newWorkspace     nameWorkspace    nukeWorkspace    __
  , k ","            incMaster        __               __               __
  , k "."            decMaster        __               __               __
  , k "/"            __               __               __               __

  , k "<Space>"      nextLayout       resetLayout      __               refresh'

  , k "<Home>"       __               __               __               __
  , k "<End>"        __               __               __               __
  , k "<Delete>"     __               __               __               __
  , k "<Page_Up>"    __               __               __               __
  , k "<Page_Down>"  __               __               __               __

  , k "<Up>"         shrinkSlave      __               __               __
  , k "<Down>"       expandSlave      __               __               __
  , k "<Left>"       shrinkMaster     __               __               __
  , k "<Right>"      expandMaster     __               __               __
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

    openTerminal     = spawn $ terminal conf
    openChrome       = spawn "google-chrome"
    openDmenu        = spawn "dmenu_run"
    searchPrompt     = promptSearch defaultXPConfig google
    searchSelection  = selectSearch google
    fetchMouse       = warpToWindow 0.5 0.5

    __ = return ()
    k = bindString

shortcutMap :: XConfig l -> [(String, X ())]
shortcutMap conf =
        [(mod ++ key, cmd $ tag)
              | (tag, key) <- zip myWorkspaces myWsShortcuts
              , (mod, cmd) <- actions]
           where
               actions = [("M-",view),("M-S-",drag),("M-C-",send)]
               view    = windows . W.view
               send    = windows . W.shift
               drag    = \w -> send w >> view w

bindString :: String -> X () -> X () -> X () -> X () -> [(String, X ())]
bindString key m ms mc msc =
        [ bind "M-"      key m
        , bind "M-S-"    key ms
        , bind "M-C-"    key mc
        , bind "M-S-C-"  key msc
        ]
           where
               bind mod key cmd = (mod ++ key, cmd)

bindButton :: Button -> (Window -> X ()) -> (Window -> X ()) -> (Window -> X ()) -> (Window -> X ())
           -> [((KeyMask,Button), (Window -> X ()))]
bindButton but m ms mc msc =
        [ bind m'    but m
        , bind ms'   but ms
        , bind mc'   but mc
        , bind msc'  but msc
        ]
           where
               bind mod but cmd = ((mod,but), cmd)
               m'   = myModMask
               ms'  = myModMask .|. shiftMask
               mc'  = myModMask .|.               controlMask
               msc' = myModMask .|. shiftMask .|. controlMask

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = mkKeymap conf $ concat $ map ($ conf) myKeymaps

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings conf = M.fromList $ concat $ map ($ conf) myButmaps


------------------------------------------------------------------------
-- LOGGING
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
    return (\_ -> x)

workspaceOnScreen :: ScreenId -> X (String -> String)
workspaceOnScreen n = do
    ws <- gets windowset
    let ss = (W.current ws) : (W.visible ws)
        s  = L.find ((n==) . W.screen) ss
        t  = maybe "???"
                   (W.tag . W.workspace)
                   s
        c  = (W.tag . W.workspace . W.current) ws
        x = if t == c
              then xmobarColor "white" "blue" . wrap "  " "  " $ (map toUpper t)
              else xmobarColor "white" ""     . wrap "  " "  " $ t
    return (\_ -> x)

myLogHook :: XConfig l -> Handle -> Handle -> Handle -> Handle -> X ()
myLogHook c u0 d0 u1 d1 = do
    g0 <- focusedTitleOnScreen 0
    g1 <- focusedTitleOnScreen 1
    h0 <- workspaceOnScreen 0
    h1 <- workspaceOnScreen 1

    id $  logHook c
      <+> dynamicLogWithPP (topPP u0 g0 h0)
      <+> dynamicLogWithPP (topPP u1 g1 h1)
      <+> dynamicLogWithPP bottomPP

             where
                topPP u g h = defaultPP
                   { ppOutput   = hPutStrLn u
                   , ppOrder    = \(ws:l:t:_) -> [ws,t]
                   , ppCurrent  = h
                   , ppVisible  = const ""
                   , ppHidden   = const ""
                   , ppHiddenNoWindows = const ""
                   , ppWsSep    = ""
                   , ppTitle    = g
                   }

                bottomPP = defaultPP
                   { ppOutput   = \s -> hPutStrLn d0 s >> hPutStrLn d1 s
                   , ppOrder    = \(ws:l:t:_) -> [l,ws]
                   , ppSep      = " "
                   , ppLayout   = xmobarColor "black"  "#ccc"   . wrap "<" ">"
                   , ppWsSep    = " "
                   , ppSort     = mkWsSort cmp
                   , ppCurrent  = xmobarColor "black"  "yellow" . wrap " " " " . shortcut
                   , ppVisible  = xmobarColor "green"   "" . shortcut
                   , ppHidden   = xmobarColor "white"  "" . shortcut
                   , ppHiddenNoWindows =
                                  xmobarColor "#444"   "black" . shortcut
                   , ppUrgent   = xmobarColor "black"  "red" . wrap " " " " . shortcut
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
-- STARTUP
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook =  setDefaultCursor xC_left_ptr
             <+> setWMName "LG3D"

