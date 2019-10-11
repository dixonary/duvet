{-| An implementation of the hcover CovChecker as an example of integration
    with duvet.
-}
module Checker.Hcover (hcover) where

import Data.VASS.Coverability

{-| TODO: This is currently a stub. It should probably be moved to the 
    hcover package, so that duvet does not explicitly rely on hcover.
-}
hcover :: CovChecker
hcover = const $ return Safe