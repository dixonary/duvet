module Checker.Hcover (hcover) where

import Data.VASS.Coverability

hcover :: CovChecker
hcover = const $ return Safe