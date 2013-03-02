
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Util.Keymap
-- Copyright    : (c) 2013 Ray Lehtiniemi
-- License      : BSD3-style (see LICENSE)
--
-- Maintainer   : Ray Lehtiniemi <rayl@mail.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- This module provides tabular specification of keybindings.
-----------------------------------------------------------------------------
module XMonad.Util.Keymap
    (
    mkMyKeys,
    mkMyMouseBindings,
    bindString,
    bindButton 
    ) where

import XMonad;
import qualified Data.Map as M (Map,fromList)
import XMonad.Util.EZConfig (mkKeymap)

-- | Given a list of String-style tables, produce a keys function for XConfig
mkMyKeys :: [XConfig Layout -> [(String, X ())]]
         -> XConfig Layout
         -> M.Map (KeyMask, KeySym) (X ())
mkMyKeys maps conf = mkKeymap conf $ concat $ map ($ conf) maps


-- | Given a list of Button-style tables, produce a mouseBindings function for XConfig
mkMyMouseBindings :: [XConfig Layout -> [((KeyMask, Button), (Window -> X ()))]]
                  -> XConfig Layout
                  -> M.Map (KeyMask, Button) (Window -> X ())
mkMyMouseBindings maps conf = M.fromList $ concat $ map ($ conf) maps


-- | Expand a row in a String-style table
bindString :: String -> String
           -> X () -> X () -> X () -> X ()
           -> [(String, X ())]
bindString p key m ms mc msc =
        [ bind ""      key m
        , bind "S-"    key ms
        , bind "C-"    key mc
        , bind "S-C-"  key msc
        ]
           where
               bind mod key cmd = ("M-" ++ p ++ mod ++ key, cmd)


-- | Expand a row in a Button-style table
bindButton :: KeyMask -> Button
           -> X () -> X () -> X () -> X ()
           -> [((KeyMask,Button), (Window -> X ()))]
bindButton p but m ms mc msc =
        [ bind m'    but m
        , bind ms'   but ms
        , bind mc'   but mc
        , bind msc'  but msc
        ]
           where
               bind mod but cmd = ((p .|. mod,but), (\ _ -> cmd))
               m'   = 0
               ms'  = shiftMask
               mc'  =               controlMask
               msc' = shiftMask .|. controlMask


