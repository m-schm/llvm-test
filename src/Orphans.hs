{-# OPTIONS_GHC -Wno-orphans #-}
module Orphans where

import Relude (Hashable)
import LLVM.AST (Name)

instance Hashable Name
