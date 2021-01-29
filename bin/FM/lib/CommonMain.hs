module CommonMain where

import           Control.Monad
import           Data.Char
import           Data.List           (intercalate, intersperse)
import           Data.Semigroup
import           Data.Tuple
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


data SimpleMode
  = SynthMode
  | InflMode
  | TaggerMode
  | InflBatchMode
  deriving (Show, Eq, Ord)
data OutputMode
  = LexMode
  | WriteLexicons
  | WriteTables
  | WriteGF
  | WriteGFR
  | WriteLatex
  | WriteLEXC
  | WriteXFST
  | WriteSQL
  deriving (Show, Eq, Ord)
data ProgramMode
  = SimpleProgramMode SimpleMode
  | OutputProgramMode OutputMode
  deriving (Show, Eq, Ord)
data Flags
  = Flags
      { file :: Maybe String
      , mode :: ProgramMode
      }
  deriving (Show, Eq, Ord)


parseBaseFlags :: Parser Flags
parseBaseFlags = Flags Nothing <$> (SimpleProgramMode <$> parseProgramMode)
  where
    parseProgramMode :: Parser SimpleMode
    parseProgramMode =
        flag' SynthMode (long "synthesize" <> short 's' <> help "Interactive synthesizer mode") <|>
        flag' InflMode (long "inflection" <> short 'i' <> help "Interactive inflection mode") <|>
        flag' InflBatchMode (long "batch" <> short 'b' <> help "Batch inflection mode") <|>
        flag' TaggerMode (long "tag" <> short 't' <> help "Tagger mode")


parseOutputFlags :: Parser Flags
parseOutputFlags = Flags <$> parseFileTarget <*> (OutputProgramMode <$> parseProgramMode)
  where
    parseProgramMode :: Parser OutputMode
    parseProgramMode =
        flag' WriteLexicons (long "lex" <> help "Write internal lexicon to output") <|>
        flag' WriteTables (long "tables" <> help "Write internal tables to output") <|>
        flag' WriteGF (long "gf" <> help "Write GF to output") <|>
        flag' WriteGFR (long "gfr" <> help "Write GFR to output") <|>
        flag' WriteLatex (long "latex" <> help "Write LaTeX to output") <|>
        flag' WriteLEXC (long "lexc" <> help "Write LEXC to output") <|>
        flag' WriteXFST (long "xfst" <> help "Write XFST to output") <|>
        flag' WriteSQL (long "sql" <> help "Write SQL to output")
    parseFileTarget :: Parser (Maybe String)
    parseFileTarget =
        flag' () (short 'o' <> long "output") *>
        optional (strOption (long "file" <> short 'f' <> metavar "FILE" <> help "test"))


parseProgramFlags :: Language a => a -> ParserInfo Flags
parseProgramFlags l = info coreParser programDescription
  where
    coreParser :: Parser Flags
    coreParser =
        helper <*> parseBaseFlags <|> parseOutputFlags
    programDescription = fullDesc <> progDesc "TODO" --(welcome l)


commonMain :: Language a => a -> IO ()
commonMain l = do
    flags <- execParser $ parseProgramFlags l
    lex <- catchIOError (getEnv (env l))
           (\_ -> do prErr $ "\n[" ++ env l ++ " is undefined, using \"./" ++ dbaseName l ++ "\".]\n"
                     return $ "./" ++ dbaseName l)
    let pm = mode flags
    case pm of
      SimpleProgramMode m ->
        case m of
          TaggerMode    ->
            do prErr $ welcome l
               t <- readTrie l lex
               run (analysis t (composition l))
          SynthMode     ->
            do prErr $ welcome l
               putStrLn "\n[Synthesiser mode]\n"
               putStrLn $ "Enter a " ++ uName l ++ " word in any form.\n"
               putStrLn "If the word is not found, a [command] with [arguments].\n"
               putStrLn "Type 'c' to list commands.\n"
               putStrLn "Type 'q' to quit.\n"
               theDictionary <- readDict l lex
               trieDictL     <- readTrie l lex
               synthesiser l theDictionary trieDictL
          InflMode      ->
            do prErr $ welcome l
               putStrLn "\n[Inflection mode]\n"
               putStrLn "Enter [command] [dictionary form].\n"
               putStrLn "Type 'c' to list commands.\n"
               putStrLn "Type 'q' to quit.\n"
               infMode l
          InflBatchMode ->
            do prErr $ welcome l
               imode l
      OutputProgramMode m ->
        do let fi = file flags
           theDictionary <- readDict l lex
           let funcs = case m of
                         WriteLexicons -> (outputLex,
                                           (writeLex, "Wrote full form lexicon: "))
                         WriteTables   -> (outputTables,
                                           (writeTables, "Wrote tables: "))
                         WriteGF       -> (outputGF (gfTypes l),
                                           (writeGF (gfTypes l), "Wrote GF source code: "))
                         WriteGFR      -> (outputGFRes (gfTypes l),
                                           (writeGFRes (gfTypes l), "Wrote GF resource: "))
                         WriteLatex    -> (outputLatex,
                                           (writeLatex, "Wrote LaTeX document: "))
                         WriteLEXC     -> (outputXML,
                                           (writeXML, "Wrote XML source code: "))
                         WriteXFST     -> (outputLEXC,
                                           (writeLEXC, "Wrote LEXC source code: "))
                         WriteSQL      -> (outputSQL,
                                           (writeSQL, "Wrote SQL source code: "))
           case fi of
             Nothing -> fst funcs theDictionary
             Just f  -> fst tup f theDictionary >>
                        prErr (snd tup ++ f)
                          where
                            tup = snd funcs

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
