{-# LANGUAGE FlexibleContexts #-}
module Text.RSS.Lens.Rules where

-- {{{ Imports
import           Language.Haskell.TH.Syntax
import           Lens.Micro
import           Lens.Micro.TH
-- }}}

rules :: LensRules
rules = set lensField f lensRules where
  f _ _ name = [TopName $ mkName (nameBase name <> "L")]
