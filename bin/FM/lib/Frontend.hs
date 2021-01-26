{-# LANGUAGE BlockArguments #-}

module Frontend where

import           Control.Monad
import           Data.Char
import           Data.Maybe       (isJust)
import           Dictionary
import           ErrM
import           General
import           Map
import           System.IO
import           System.IO.Error  (catchIOError)
import           System.IO.Strict as Strict

-- A class defined to be able to construct a language independent frontend

-- Note that all Functions have default definitions, but
-- in the common case, you give, at least, definitions for "paradigms"
-- "interDict" and "composition"

class Show a => Language a where
  name        :: a -> String                  -- The name of the language
  dbaseName   :: a -> String                  -- The name of dictionary Database
  composition :: a -> ([Attr] -> Bool)      -- Definition of legal compositions
  env         :: a -> String                  -- Environment variable
  paradigms   :: a -> Commands
  internDict  :: a -> Dictionary                   -- The internal dictionary

  name        l = map toLower (show l)
  dbaseName   l = name l ++ ".lexicon"
  composition _ = noComp
    where noComp [_] = True
          noComp  _  = False
  env         l = "FM_" ++ map toUpper (show l)
  paradigms   _ = empty
  internDict  _ = emptyDict

type Commands = Map String ([String], [String] -> Entry) -- a map of paradigms

emptyC :: Commands
emptyC = Map.empty

insertCommand :: (String,[String],[String] -> Entry) -> Commands -> Commands
insertCommand (n,args,f) cs = (n,(args,f)) |-> cs

parseCommand :: Language a => a -> String -> Err Entry
parseCommand l s =
   case words s of
    (x:xs) -> case paradigms l ! x of
               Nothing -> Left $ "Error: Command not found [" ++ s ++ "]"
               Just (ys,f) -> if length xs == length ys then
                           Right $ f xs
                               else
                                 Left $ "Error: wrong number of arguments [" ++ s ++ "]"
    _ -> Left $ "Error: Invalid command [" ++ s ++ "]"

paradigmNames :: Language a => a -> [String]
paradigmNames l = [ c ++ " " ++ unwords args | (c,(args,_)) <- flatten (paradigms l)]

parseDict :: Language a => a -> FilePath -> IO Dictionary
parseDict l f =
    do es <- catchIOError (readdict l f) (\_ -> do writeFile f [] ; prErr ("Created new external dictionary: \"" ++ f ++ "\".\n"); return [])
       return $ dictionary es



isParadigm :: Language a => a -> String -> Bool
isParadigm l s = isJust $ paradigms l ! s


readdict :: Language a => a -> FilePath -> IO [Entry]
readdict l f = do h <- openFile f ReadMode
                  c <- Strict.hGetContents h
                  rv <- process l (lines c) []
                  hClose h
                  return rv


process :: Language a => a -> [String] -> [Entry] -> IO [Entry]
process _ [] xs = return xs
process l (c:cs) xs = do res <- collect c xs
                         process l cs res
 where
  collect []     pre = return xs
  collect xs@(c:s) pre
   | isComment xs = return pre
   | otherwise   = case parseCommand l xs of
                    Right e -> return (e:pre)
                    Left s  -> do prErr s
                                  return pre
  isComment []           = False
  isComment (' ':xs)     = isComment xs
  isComment ('-':'-':xs) = True
  isComment _            = False

-- Application of lists to functions

app1 :: (String -> Entry) -> [String] -> Entry
app1 f [x] = f x
app1 _ _   = error "app1: wrong number of arguments"

app2 :: (String -> String -> Entry) -> [String] -> Entry
app2 f [x,y] = f x y
app2 _ _     = error "app2: wrong number of arguments"

app3 :: (String -> String -> String -> Entry) -> [String] -> Entry
app3 f [x,y,z] = f x y z
app3 _ _       = error "app3: wrong number of arguments"

app4 :: (String -> String -> String -> String -> Entry) -> [String] -> Entry
app4 f [x,y,z,w] = f x y z w
app4 _ _         = error "app4: wrong number of arguments"

app5 :: (String -> String -> String -> String -> String -> Entry) -> [String] -> Entry
app5 f [x,y,z,w,a] = f x y z w a
app5 _ _           = error "app5: wrong number of arguments"

app6 :: (String -> String -> String -> String -> String -> String -> Entry) -> [String] -> Entry
app6 f [x,y,z,w,a,b] = f x y z w a b
app6 _ _             = error "app6: wrong number of arguments"

app7 :: (String -> String -> String -> String -> String -> String -> String -> Entry) -> [String] -> Entry
app7 f [x,y,z,w,a,b,c] = f x y z w a b c
app7 _ _               = error "app7: wrong number of arguments"

prErr :: String -> IO()
prErr s =  hPutStr stderr (s ++ "\n")
