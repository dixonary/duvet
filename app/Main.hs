module Main where

import Duvet (runDuvet)

-- Duvet itself does not provide any checkers!
-- To use Duvet, add it as a dependency and use runDuvet with your own checkers.
main = runDuvet []