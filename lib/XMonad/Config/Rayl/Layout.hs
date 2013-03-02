{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Rayl.Layout
-- Copyright    : (c) 2013 Ray Lehtiniemi
-- License      : BSD3-style (see LICENSE)
--
-- Maintainer   : Ray Lehtiniemi <rayl@mail.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- This module provides some custom layout transformer toggles.
-----------------------------------------------------------------------------
module XMonad.Config.Rayl.Layout (
    -- * Usage
    -- $usage
    MyTransformers(ZOOM)
    ) where

import Data.Char                         (toUpper)
import XMonad                            -- many
import XMonad.Layout.MultiToggle         (Transformer,transform)
import XMonad.Layout.NoBorders           (noBorders)
import XMonad.Layout.Renamed             (renamed,Rename(Replace))

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.MultiToggle (mkToggle,single,Toggle(..))
-- > import XMonad.Config.Rayl.Layout
--
-- Then edit your @layoutHook@ by adding a transformer to layout(s):
--
-- > myLayout = mkToggle (single ZOOM) $ someLayout
-- > main = xmonad defaultConfig { layoutHook = myLayout }
--
-- Finally, create a keybinding to use the toggle.
--
-- > `additionalKeys` [ ((mod,xK_z), sendMessage $ Toggle ZOOM) ]

-- | Data type for various custom layout modifiers
--
-- * ZOOM upcases the current layout name, then applies fullscreen, no borders layout
--
-- * (more transformers to come...)
data MyTransformers = ZOOM
   deriving (Read, Show, Eq, Typeable)

instance Transformer MyTransformers Window where
  transform ZOOM x k = k (renamed [Replace uc] $ noBorders Full) (const x)
    where
      uc = map toUpper (description x)

