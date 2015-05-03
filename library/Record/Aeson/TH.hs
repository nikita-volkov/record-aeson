{-# LANGUAGE CPP #-}
module Record.Aeson.TH where

import BasePrelude
import Record.Types
import Record.Lens
import Language.Haskell.TH
import Data.Aeson.Types
import GHC.TypeLits


-- |
-- Compatibility with the 'ClassP' constructor,
-- which was thrown away in 2.10.
#if MIN_VERSION_template_haskell(2,10,0)
mkClassP :: Name -> [Type] -> Type
mkClassP a b =
  foldl' AppT (ConT a) b
#else
mkClassP :: Name -> [Type] -> Pred
mkClassP =
  ClassP
#endif
