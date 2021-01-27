module CommonMain where

import           Control.Monad
import           Data.Char
import           Data.List           (intercalate, intersperse)
import           Data.Semigroup
import           Dictionary
import           Frontend
import           GeneralIO
import           Options.Applicative
import           Print
import           System.Environment  (getArgs, getEnv)
import           System.IO
import           System.IO.Error     (catchIOError)
import           System.IO.Strict    as Strict
import           Trie


data ProgramMode
    = SynthMode
    | InflMode
    | InflBatchMode
    | LexMode
    | WriteLexicon
    | WriteTables
    | WriteGF
    | WriteGFR
    | WriteLatex
  deriving (Show, Eq)
data Flags
  = SimpleFlags
      { mode :: ProgramMode
      }
  | FileFlags
      { mode :: ProgramMode
      , file :: Maybe String
      }



parseCoreFlags :: Parser ProgramMode
parseCoreFlags =
    flag' SynthMode (long "synthesize" <> short 's' <> help "Interactive synthesizer mode") <|>
    flag' InflMode (long "inflection" <> short 'i' <> help "Interactive inflection mode") <|>
    flag' InflBatchMode (long "batch" <> short 'b' <> help "Batch inflection mode")

parseFileFlags :: Parser ProgramMode
parseFileFlags =
    flag' WriteLexicon (long "lexicon" <> short 'l' <> help "Write internal lexicon to output") <|>
    flag' WriteTables (long "table" <> short 't' <> help "Write internal tables to output") <|>
    flag' WriteGF (long "gf" <> help "Write GF to output") <|>
    flag' WriteGFR (long "gfr" <> help "Write GFR to output") <|>
    flag' WriteLatex (long "latex" <> help "Write LaTeX output to output")

parseProgramOptions :: Parser Flags
parseProgramOptions =
    SimpleFlags <$> parseCoreFlags <|>
    FileFlags <$> parseFileFlags <*> parseOutputFile
  where
    parseOutputFile :: Parser (Maybe String)
    parseOutputFile = Just <$> strOption (long "file" <> short 'f' <> metavar "FILE" <> value "stdout" <> help "Write output to FILE")



commonMain :: Language a => a -> IO ()
commonMain a = do
    opts <- execParser optsParser
    putStrLn $
        "Hello, " ++ (show . mode $ opts)
  where
    optsParser :: ParserInfo Flags
    optsParser =
        info
            (helper <*> versionOption <*> parseProgramOptions)
            (fullDesc <> progDesc "optparse example" <>
             header
                 "optparse-example - a small example program for optparse-applicative")
    versionOption :: Parser (a -> a)
    versionOption = infoOption "0.0" (long "version" <> help "Show version")

run :: (String -> [[String]]) -> IO ()
run f = Strict.interact $ unlines . analyze f . nWords

gfTypes :: Language a => a -> String
gfTypes l = "types." ++ name l ++ ".gf"

readDict :: Language a => a -> FilePath -> IO Dictionary
readDict l f = do database <- parseDict l f
                  return $ database <> internDict l

readTrie :: Language a => a -> FilePath -> IO SATrie
readTrie l f = do d <- readDict l f
                  prInfo d
                  return $ trieDict d

uName :: Language a => a -> String
uName l = case name l of
           []     -> []
           (x:xs) -> toUpper x : xs

analyze :: (String -> [[String]]) -> [String] -> [String]
analyze _  []  = []
analyze f (s:ss)
 = case f s of
    [] -> ("[ <" ++ s ++ "> NONE]") : analyze f ss
    xs -> ("[ <" ++ s ++ ">") : (prA xs ++  "]") : analyze f ss
 where
       prA xs = unlines [show n ++ ". " ++ s | (n,s) <- zip [1..] (map pr xs)]
       pr []  = []
       pr [x] = x
       pr xs  = "Composite: " ++ intercalate " | " xs

welcome :: Language a => a -> String
welcome l = unlines
            [
             "********************************************",
             "* " ++ uName l ++ " Morphology" ++ padding (uName l) 30 ++ "*",
             "********************************************",
             "* Functional Morphology v1.10              *",
             "* (c) Markus Forsberg & Aarne Ranta 2004   *",
             "* under GNU General Public License.        *",
             "********************************************",
             ""
            ]
  where padding s n = replicate (max (n - length s) 0) ' '


prInfo :: Dictionary -> IO()
prInfo dict = prErr $ "Dictionary loaded: DF = " ++ show (size dict) ++ " and WF = " ++ show (sizeW dict) ++ ".\n"


helpText :: IO()
helpText = prErr . unlines $
                    ["",
                     " |---------------------------------------|",
                     " |        Program parameters             |",
                     " |---------------------------------------|",
                     " | -h             | Display this message |",
                     " |---------------------------------------|",
                     " | <None>         | Enter tagger mode    |",
                     " |---------------------------------------|",
                     " | -s             | Enter interactive    |",
                     " |                | synthesiser mode     |",
                     " |---------------------------------------|",
                     " | -i             | Enter inflection     |",
                     " |                | mode                 |",
                     " |---------------------------------------|",
                     " | -ib            | Inflection batch     |",
                     " |                | mode                 |",
                     " |---------------------------------------|",
                     " | -lex    [file] | Full form lexicon    |",
                     " | -tables [file] | Tables               |",
                     " | -gf     [file] | GF top-level code    |",
                     " | -gfr    [file] | GF resource code     |",
                     " | -latex  [file] | LaTeX source code    |",
                     " | -xml    [file] | XML source code      |",
                     " | -lexc   [file] | LexC source code     |",
                     " | -xfst   [file] | XFST source code     |",
                     " | -sql    [file] | SQL source code      |",
                     " |---------------------------------------|",
                     ""
                    ]

