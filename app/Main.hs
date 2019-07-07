module Main where
    
import Options.Applicative
import Data.Char (toLower)

import Checker.Hcover
import Data.VASS.Coverability.KarpMiller
import Text.Pretty.Simple

-- | From vass
import Data.VASS.Coverability
import Data.VASS.Read (readAny)


--------------------------------------------------------------------------------
-- ** Primary Code

main :: IO ()
main = do
    (checker, filename) <- execParser optionParser -- Command line options
    problem             <- readAny filename        -- Read file
    result              <- checker problem         -- Run checker
    print result




--------------------------------------------------------------------------------
-- ** Options Parser

optionParser :: ParserInfo (CovChecker, FilePath)
optionParser = info parser config
    where
        parser   = helper <*> liftA2 (,) checkerP filenameP

        config :: InfoMod (CovChecker, FilePath)
        config
            =  progDesc "Run a coverability checker"  
            <> header "Duvet: Check coverability on Petri Net / VASS problems"

        filenameP :: Parser FilePath
        filenameP = argument str (metavar "FILENAME")

        checkerP :: Parser CovChecker
        checkerP 
            =   flag' karpMiller 
                ( long "karp-miller"
                <> short 'k' 
                <> help "Use the standard Karp-Miller algorithm"
                )
            <|> flag' hcover 
                ( long "hcover"
                <> short 'i' 
                <> help "Use the hcover checker (based on icover)"
                )