{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, MultiParamTypeClasses #-}

module XMonad.Layout.MyTransformers
  ( MyTransformers(..)
  ) where

import Data.Char                         (toUpper)
import XMonad                            -- many
import XMonad.Layout.MultiToggle         (Transformer,transform)
import XMonad.Layout.NoBorders           (noBorders)
import XMonad.Layout.Renamed             (renamed,Rename(Replace))

data MyTransformers = ZOOM
   deriving (Read, Show, Eq, Typeable)

instance Transformer MyTransformers Window where
  transform ZOOM x k = k (renamed [Replace uc] $ noBorders Full) (const x)
    where
      uc = map toUpper (description x)

