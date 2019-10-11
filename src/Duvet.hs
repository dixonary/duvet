{-| Duvet is an application which allows you to test arbitrary Petri Net 
    coverability checkers against each other.

    The requirements for integrating a checker are simple. As the CovChecker 
    type admits IO actions, you can also use this to interface directly with 
    other tools such as MIST or BFC, by marshalling the internal data type into 
    an accepted format. We do not expose the file format directly as not all
    tools can take all formats (most take exactly one format).

    1. Create an instance of the 'CheckerInfo' type. One example is the 
    'Data.VASS.Coverability.Checker.hcover' implementation which implements
    the hcover tool. 

    2. In the main function of your code, call runDuvet with the set of checkers 
    which you wish to compile against.

    3. Run your compiled program and use the built-in command-line flags 
    to control input and output files, output format, and so on.
-}
module Duvet where

import Options.Applicative
import Data.Char (toLower)

import Checker.Hcover
import Text.Pretty.Simple

-- | From vass
import Data.VASS.Coverability
import Data.VASS.Read (readAny)

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Vector as Vector
import Data.Vector (Vector)

import Data.Functor ((<&>))
import Data.Traversable (for)

import Text.Pretty.Simple
import qualified Data.Text.Lazy as Text (unpack)
import qualified Data.Text.Lazy.IO as Text

import qualified Data.Csv as Cassava
import Data.Csv
  ( ToRecord (toRecord)
  , ToNamedRecord (toNamedRecord)
  , Header
  , ToField (toField)
  , (.:)
  , (.=)
  )

import System.TimeIt (timeItT)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as BS
import System.FilePath
import System.IO.Silently
import Data.Bool (bool)

--------------------------------------------------------------------------------
-- ** Primary Code

runDuvet :: [CheckerInfo] -> IO ()
runDuvet checkers = do

    -- Command line options
    (checkers, filenames, saveOpt, tOpt, quiet) <- execParser $ optionParser checkers 

    problems <- mapM (\s -> (s,) <$> readAny s) filenames

    -- TODO timeouts
    let
        results :: Map String [IO (String, Double, CovResult)]
        results = Map.fromList [
            ( takeBaseName pName, 
                [ (\(d,r) -> (longName,d,r)) <$> timeItT (checker pSpec)
                | CheckerInfo{..} <- checkers
                ]
            ) 
            | (pName, pSpec) <- problems
            ]

        csvHeader = Vector.fromList $ "problem" : (pack . longName <$> checkers)

    resultsEval <- sequence $ sequence . fmap (bool id silence quiet) <$> results
    pPrint resultsEval

    case saveOpt of
        NoSave -> return ()
        Save outputFile -> do
            let csvData = Cassava.encodeByName csvHeader $ Map.toList resultsEval
            BS.writeFile outputFile csvData
            putStrLn $ "Saved results to " ++ outputFile


--------------------------------------------------------------------------------
-- ** Specifying a checker's command-line representation

data CheckerInfo = CheckerInfo
    { checker :: CovChecker
    , longName :: String
    , shortName :: Char
    , description :: String
    }

data SaveOpt = Save String | NoSave

instance {-# OVERLAPS #-} ToNamedRecord (String, [(String, Double, CovResult)]) where 
    toNamedRecord (problemName, results) =
        Cassava.namedRecord $ ("problem" .= pack problemName) : (toNamedField <$> results)

toNamedField (checker, time, res) = pack checker .= pack (show res ++ " (" ++ show time ++ ")")


--------------------------------------------------------------------------------
-- ** Options Parser

optionParser :: [CheckerInfo] -> ParserInfo ([CheckerInfo], [FilePath], SaveOpt, Maybe Integer, Bool)
optionParser checkers = info parser config
    where
        parser   = (helper <*>) 
            $ (,,,,) 
            <$> (allCheckersP <|> checkerP) 
            <*> filenameP 
            <*> saveP 
            <*> timeoutP
            <*> quietP

        config
            =  progDesc "Run a coverability checker"  
            <> header "Duvet: Check coverability on Petri Net / VASS problems"

        filenameP :: Parser [FilePath]
        filenameP = some $ argument str (metavar "FILENAME")

        allCheckersP :: Parser [CheckerInfo]
        allCheckersP = flag' checkers (long "all")

        checkerP :: Parser [CheckerInfo]
        checkerP = choices $ checkers <&> (\c@CheckerInfo{..} ->
            flag' c (long longName <> short shortName <> help description))
            
        choices :: Alternative f => [f a] -> f [a]
        choices = some . foldl1 (<|>)

        saveP = option 
            (Save <$> str) 
            (short 'o' <> long "output" <> value NoSave <> metavar "OUTPUT_FILE")

        timeoutP = option
            (Just <$> auto)
            (long "timeout" <> value Nothing <> metavar "TIMEOUT")

        quietP = switch (long "quiet")