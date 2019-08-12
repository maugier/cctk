module CCTK.Group.Modulo where

import CCTK.Group
import Math.NumberTheory.Moduli


instance KnownNat m => Group (MultMod m) where
    inv = invertGroup    
