
import Data.Char                         (toUpper)
import Data.Function                     (on)
import qualified Data.List as L          (intersperse,find)
import qualified Data.Map as M           (Map,fromList,lookup)
import Data.Monoid                       (All,mconcat)
import System.Exit                       (exitWith,ExitCode(ExitSuccess))
import System.IO                         (hPutStrLn, Handle)
import XMonad                            -- many
import XMonad.Actions.CycleWS            (toggleWS, toggleOrView,moveTo,
                                          WSType(HiddenWS),Direction1D(..),
                                          shiftTo, screenBy)
import XMonad.Actions.DynamicWorkspaces  (selectWorkspace,
                                          removeEmptyWorkspaceAfterExcept,
                                          renameWorkspace)
import XMonad.Actions.Search             (promptSearch,selectSearch,google)
import XMonad.Actions.SwapWorkspaces     (swapWithCurrent)
import XMonad.Actions.Warp               (warpToWindow)
import XMonad.Actions.WindowBringer      (bringMenu,gotoMenu)
import XMonad.Hooks.DynamicLog           -- many
import XMonad.Hooks.EwmhDesktops         (ewmh,fullscreenEventHook)
import XMonad.Hooks.ManageDocks          (manageDocks,docksEventHook,
                                          avoidStruts,ToggleStruts(..))
import XMonad.Hooks.SetWMName            (setWMName)
import XMonad.Hooks.UrgencyHook          (withUrgencyHook, NoUrgencyHook(..))
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
myKeyMaps   = [ keyboardMap, workspaceMap ]
myMouseMaps = [ mouseMap ] 
myWsShortcuts = map show $ [1..9] ++ [0]


mouseMap :: XConfig l -> [((KeyMask, Button), (Window -> X ()))]
mouseMap conf = concat
  --  button         M-               M-S-             M-C-             M-S-C-
  [ k button1        shiftMaster      __               __               __
  , k button2        focusWindow      __               __               __
  , k button3        focusWindow      __               __               __
  , k button4        focusUp          prevWorkspace    __               __
  , k button5        focusDown        nextWorkspace    __               __
  ] 
  where
    __               = \_ -> return ()
    nextWorkspace    = \_ -> moveTo Next HiddenWS
    prevWorkspace    = \_ -> moveTo Prev HiddenWS
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
  [ k "<Esc>"        __               __               __               __

  , k "`"            lastWorkspace    __               __               __
  -- see workspaceMap for number keys
  , k "-"            __               __               __               __
  , k "="            __               __               __               __
  , k "<Backspace>"  closeWindow      killWorkspace    __               __

  , k "<Tab>"        __               __               __               __
  , k "q"            restartXmonad    resetXmonad      __              quitXmonad
  , k "w"            __               __               __               __
  , k "e"            __               __               __               __
  , k "r"            __               __               __               __
  , k "t"            sinkWindow       __               __               __
  , k "y"            __               __               __               __
  , k "u"            prevWorkspace    toPrevWorkspace  __               __
  , k "i"            nextWorkspace    toNextWorkspace  __               __
  , k "o"            __               __               __               __
  , k "p"            __               __               __               __
  , k "["            __               __               __               __
  , k "]"            __               __               __               __
  , k "\\"           __               __               __               __

  , k "a"            openTerminal     openChrome       openDmenu        __
  , k "s"            searchSelection  searchPrompt     __               __
  , k "d"            __               __               __               __
  , k "f"            __               __               __               __
  , k "g"            __               __               __               __
  , k "h"            prevScreen       toPrevScreen     __               __
  , k "j"            focusDown        swapDown         __               __
  , k "k"            focusUp          swapUp           __               __
  , k "l"            nextScreen       toNextScreen     __               __
  , k ";"            __               __               __               __
  , k "'"            __               __               __               __
  , k "<Return>"     __               __               __               __

  , k "z"            pickWorkspace    fullscreen       __               __
  , k "x"            fetchMouse       toggleStruts     __               __
  , k "c"            newWorkspace     nameWorkspace    __               __
  , k "v"            __               __               __               __
  , k "b"            __               __               __               __
  , k "n"            gotoMenu'        bringMenu'       __               __
  , k "m"            focusMaster      shiftMaster      __               __
  , k ","            incMaster        __               __               __
  , k "."            decMaster        __               __               __
  , k "/"            __               __               __               __

  , k "<Space>"      nextLayout       firstLayout      __               refresh'

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
    __               = return ()
    restartXmonad    = restart "xmonad" True
    resetXmonad      = restart "xmonad" False
    quitXmonad       = io $ exitWith ExitSuccess
    nextScreen       = onScr 1 W.view
    prevScreen       = onScr (-1) W.view
    toNextScreen     = onScr 1 W.shift
    toPrevScreen     = onScr (-1) W.shift
    nextWorkspace    = moveTo Next HiddenWS
    prevWorkspace    = moveTo Prev HiddenWS
    toNextWorkspace  = shiftTo Next HiddenWS
    toPrevWorkspace  = shiftTo Prev HiddenWS
    lastWorkspace    = toggleWS
    pickWorkspace    = workspacePrompt defaultXPConfig { autoComplete = Just 1 } $ \w ->
                       do s <- gets windowset
                          if W.tagMember w s
                            then windows $ W.view w
                            else return ()
    newWorkspace     = selectWorkspace defaultXPConfig
    killWorkspace    = removeEmptyWorkspaceAfterExcept myWorkspaces nextWorkspace
    nameWorkspace    = renameWorkspace defaultXPConfig
    refresh'         = refresh
    firstLayout      = setLayout $ layoutHook conf
    nextLayout       = sendMessage NextLayout
    fullscreen       = sendMessage (Toggle ZOOM)
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
    shiftMaster      = windows W.shiftMaster
    focusMaster      = windows W.focusMaster
    openTerminal     = spawn $ terminal conf
    openChrome       = spawn "google-chrome"
    openDmenu        = spawn "dmenu_run"
    searchPrompt     = promptSearch defaultXPConfig google
    searchSelection  = selectSearch google
    fetchMouse       = warpToWindow 0.5 0.5

    k key m ms mc msc =
        [ bind "M-"      key m
        , bind "M-S-"    key ms
        , bind "M-C-"    key mc
        , bind "M-S-C-"  key msc
        ]
      where
        bind mod key cmd = (mod ++ key, cmd)

    onScr n f = screenBy n
            >>= screenWorkspace
            >>= flip whenJust (windows . f)

workspaceMap :: XConfig l -> [(String, X ())]
workspaceMap conf =
   [(mod ++ key, windows $ cmd tag)
       | (tag, key) <- zip myWorkspaces myWsShortcuts
       , (cmd, mod) <- [(W.view,"M-"), (W.shift,"M-S-"), (swapWithCurrent,"M-C-")]]

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

