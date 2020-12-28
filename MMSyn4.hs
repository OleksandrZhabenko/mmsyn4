{-# OPtIONS_HADDOCK show-extensions #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

{-|
Module      : MMSyn4
Description : The "glue" between electronic tables and GraphViz
Copyright   : (c) OleksandrZhabenko, 2017-2020
License     : MIT
Maintainer  : olexandr543@yahoo.com
Stability   : Experimental

A program @mmsyn4@ converts a specially formated @.csv@ file with a colon as a field separator obtained from the electronic table
into a visualized by GraphViz graph in the one of the supported by GraphViz graphics format. The proper GraphViz installation is required.
-}

module MMSyn4 (getFormat,process2) where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Data.List (nub)
import System.Info (os)
import System.CPUTime (getCPUTime)
import System.Process (callCommand)
import GHC.Arr
import EndOfExe (showE)
import Data.Maybe (isJust,fromJust)
import Data.Foldable (foldr)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

isSep :: Char -> Bool
isSep = (== ':')

-- | Returns @True@ if OS is Windows.
isWindows :: Bool
isWindows = take 5 os == "mingw"
{-# INLINE isWindows #-}

divideString :: (Char -> Bool) -> String -> [String]
divideString p xs
 | null xs = []
 | otherwise = let (zs,ys) = break p xs in zs:(if null ys then [""] else divideString p (drop 1 ys))

isEscapeChar :: Char -> Bool
isEscapeChar x = x == '\n' || x == '\r'

dropEmptyLines :: [String] -> [String]
dropEmptyLines [] = []
dropEmptyLines (ys:yss)
 | let ts = dropWhile isSep ys in all isEscapeChar ts || null ts = dropEmptyLines yss
 | otherwise = ys:dropEmptyLines yss

cells :: String -> Array Int [String]
cells xs = amap (divideString isSep) . listArray (0,l) . dropEmptyLines . map (\rs -> if drop (length rs - 1) rs == "\r" then init rs else rs) $ yss
  where (yss,l) = linesL1 xs
{-# INLINE cells #-}

-- | Inspired by: <https://hackage.haskell.org/package/base-4.14.0.0/docs/src/Data.OldList.html#lines>
linesL :: ([String],Int) -> String -> ([String],Int)
linesL (xs,y) "" = (xs,y)
linesL (xs,y) s  = linesL (l:xs,y + 1) (case s' of { [] -> [] ; _:s'' -> s'' })
  where (l, s') = break (== '\n') s

-- | Inspired by: <https://hackage.haskell.org/package/base-4.14.0.0/docs/src/Data.OldList.html#lines>
linesL1 :: String -> ([String],Int)
linesL1 = linesL ([],-1)

processCells :: String -> Array Int [String] -> String
processCells xs arr = makeRecordGv xs . convertElemsToStringGv . filterNeeded . changeNeededCells $ arr
{-# INLINE processCells #-}

processCellsG :: String -> String -> String
processCellsG xs = processCells xs . cells
{-# INLINE processCellsG #-}

-- | Do not change the lengths of element lists
changeNeededCells :: Array Int [String] -> Array Int [String]
changeNeededCells arr = listArray (bounds arr) . map (\(i, e) -> changeLine i e arr) . assocs $ arr
{-# INLINE changeNeededCells #-}

-- | Changes every line by changing (if needed) one empty String to the needed one non-empty. It is necessary for this to find the parent cell for the
-- line in the previous elements of the 'Array'. The contents of the cell (if exist) are substituted instead of the empty 'String' in the line being
-- processed. Afterwards, drops all the preceding empty strings in the line. The length of the line now is not constant.
changeLine :: Int -> [String] -> Array Int [String] -> [String]
changeLine i yss arr =
  let !n = length . takeWhile null $ yss
      !xs = parentCellContents n i arr in if null xs then drop n yss else xs:(drop n yss)
{-# NOINLINE changeLine #-}

parentCellContents :: Int -> Int -> Array Int [String] -> String
parentCellContents n i arr
 | n == 0 = []
 | ll == 0 = []
 | otherwise = (\(x, _, _) -> x) . foldr f ([], 0, ll) . amap (!! (n - 1)) $ arr
     where ll = numElements arr - i - 1
           f e (e0, m, k)
             | m < k && not (null e) = (e, m + 1, k)
             | otherwise = (e0, m + 1, k)

-- | Change the lengths of element lists by dropping the last empty strings in every element.
filterNeeded :: Array Int [String] -> Array Int [String]
filterNeeded = amap (takeWhile (not . null))
{-# INLINE filterNeeded #-}

-- | Makes conversion for every line
convertElemsToStringGv :: Array Int [String] -> (Array Int String, String)
convertElemsToStringGv arr = (amap convertLineToStrGv arr, findAndMakeFilledWithClr arr)

convertLineToStrGv :: [String] -> String
convertLineToStrGv xss = "\"" ++ (let ys = concatMap (++"\"->\"") xss in take (length ys - 3) ys) ++ endOfLineGv
{-# INLINE convertLineToStrGv #-}

endOfLineGv :: String
endOfLineGv | isWindows = "\r\n"
            | otherwise = "\n"
{-# INLINE endOfLineGv #-}

findAndMakeFilledWithClr :: Array Int [String] -> String
findAndMakeFilledWithClr = concatMap (('\"':) .
  (++ "\" [style=filled, fillcolor=\"#ffffba\"];" ++ endOfLineGv)) . nub . mconcat . elems . amap lineWithAtSign
{-# INLINE findAndMakeFilledWithClr #-}

-- | In every list (representing a line) returns only those strings that begin with at-sign.
lineWithAtSign :: [String] -> [String]
lineWithAtSign = filter beginsWithAtSign
{-# INLINE lineWithAtSign #-}

beginsWithAtSign :: String -> Bool
beginsWithAtSign xs = if take 1 xs == "@" then True else take 2 xs == "\"@"
{-# INLINE beginsWithAtSign #-}

-- | Makes all needed additions and synthesizes into a single 'String' ready to be recorded to the .gv file.
makeRecordGv :: String -> (Array Int String, String) -> String
makeRecordGv xs (arr1,str2) = mconcat ["strict digraph 1 {", endOfLineGv, "overlap=false", endOfLineGv, "splines=",
  case xs of { "0" -> "false" ; "1" -> "true" ; "2" -> "ortho" ; "3" -> "polyline" ; ~vvv -> "true" }, endOfLineGv,
    mconcat (elems arr1 `mappend` [str2]), "}", endOfLineGv]
{-# INLINE makeRecordGv #-}

-- | Processes the given text (the first 'String' argument). The second one is used to get a name of the command to be
-- executed to obtain a visualization file. The third argument is used for the 'getFormat'. The fourth argument is the
-- basic name for the created files (without prefixes and extensions), the fifth one is an option for GraphVize splines
-- functionality. The sixth argument is used to specify whether to remove at-signs from the created files.
process2 :: String -> String -> String -> String -> String -> String -> IO ()
process2 text xxs yys bnames splines remAts
  | length text > 0 = do
      ts <- getCPUTime
      [bnames1,splines1] <- proc2Params2 bnames splines
      if remAts == "y"
        then do
          let ys = filter (/='@') . processCellsG splines1 $ text in writeFile (show ts ++ "." ++ bnames1 ++ ".gv") ys
          putStrLn "The visualization will be created without the at-sign."
          processFile 'n' ts bnames1 xxs yys
        else do
          let ys = processCellsG splines1 text in writeFile ("at." ++ show ts ++ "." ++ bnames1 ++ ".gv") ys
          putStrLn "The visualization will be created with the at-sign preserved."
          processFile 'a' ts bnames1 xxs yys
  | otherwise = error "Empty text to be processed! "

procCtrl :: Int -> IO String
procCtrl 1 = putStrLn "Please, input the basic name of the visualization file!" >> getLine
procCtrl 2 = do
  putStrLn "Please, specify the splines mode for GraphViz (see the documentation for GraphViz)"
  putStrLn "0 -- for \"splines=false\""
  putStrLn "1 -- for \"splines=true\""
  putStrLn "2 -- for \"splines=ortho\""
  putStrLn "3 -- for \"splines=polyline\""
  putStrLn "The default one is \"splines=true\""
  getLine
procCtrl _ = putStrLn "Would you like to remove all \'@\' signs from the visualization file?" >> getLine

processFile :: Char -> Integer -> String -> String -> String -> IO ()
processFile w t zs xxs yys = do
  if all (isJust . showE) ["fdp","twopi","circo","neato","sfdp","dot","patchwork","osage"]
    then processFile1 w t zs xxs yys
    else error "MMSyn4.processFile: Please, install the GraphViz so that its executables are in the directories mentioned in the variable PATH!"
{-# INLINE processFile #-}

processFile1 :: Char -> Integer -> String -> String -> String -> IO ()
processFile1 w t zs xxs yys = do
  [vs,spec] <- proc2Params xxs yys
  let u = take 1 vs
  if null u || u == "\n" || u == "\x0000"
    then error "MMSyn4.processFile1: Please, specify the needed character."
    else do
      let temp = fromJust . showE . (\x -> case x of { "c" -> "circo" ; "d" -> "dot" ; "f" -> "fdp" ; "n" -> "neato" ;
           "o" ->"osage" ; "p" -> "patchwork" ; "s" -> "sfdp" ; "t" -> "twopi" ; ~vv -> "sfdp" })
          q = getFormat spec
      callCommand $ temp u ++ (if w == 'n' then " -T" ++ q ++ " " else " -T" ++ q ++ " at.") ++ show t ++ "." ++ zs ++ ".gv -O "

proc2Params :: String -> String -> IO [String]
proc2Params xxs yys
 | null xxs = if null yys then mapM getFormat1 [1,2] else do { vs <- getFormat1 1 ; return [vs,yys] }
 | null yys = do { spec <- getFormat1 2 ; return [xxs,spec] }
 | otherwise = return [xxs,yys]
{-# INLINE proc2Params #-}

specFormatFile :: IO String
specFormatFile = do
  putStrLn "Please, specify the GraphViz output format for the file: "
  mapM_ printFormF ["do", "xd", "ps", "pd", "sv", "sz", "fi", "pn", "gi", "jp", "je", "js", "im", "cm"]
  putStrLn "otherwise there will be used the default -Tsvg"
  getLine
{-# INLINE specFormatFile #-}

proc2Params2 :: String -> String -> IO [String]
proc2Params2 bnames splines
 | null bnames = if null splines then mapM procCtrl [1,2] else do { bnames1 <- procCtrl 1 ; return [bnames1,splines] }
 | null splines = do { splines1 <- procCtrl 2 ; return [bnames,splines1] }
 | otherwise = return [bnames,splines]
{-# INLINE proc2Params2 #-}

getFormat1 :: Int -> IO String
getFormat1 1 = do
  putStrLn "Please, specify the GraphViz command: "
  mapM_ printGraphFilter ["d","f","t","c","n","s","p","o"]
  putStrLn "otherwise there will be used the default sfdp"
  getLine
getFormat1 _ = specFormatFile
{-# INLINE getFormat1 #-}

-- | For the given argument (usually of two characters) return the full form of the file format to be generated by GraphViz and @mmsyn4@. The default one
-- is \"svg\".
getFormat :: String -> String
getFormat xs = case xs of { "cm" -> "cmapx" ; "do" -> "dot" ; "fi" -> "fig" ; "gi" -> "gif" ; "im" -> "imap" ;
  "je" -> "jpeg" ; "jp" -> "jpg" ; "js" -> "json" ; "pd" -> "pdf" ; "pn" -> "png" ; "ps" -> "ps" ; "sv" -> "svg" ; "sz" -> "svgz" ; "xd" -> "xdot" ; ~vvv -> "svg" }
{-# INLINE getFormat #-}

printFormF :: String -> IO ()
printFormF xs = putStrLn $ show xs ++ " -- for -T" ++ case xs of { "cm" -> "cmapx" ; "do" -> "dot" ; "fi" -> "fig" ;
   "gi" -> "gif" ; "im" -> "imap" ; "je" -> "jpeg" ; "jp" -> "jpg" ; "js" -> "json" ; "pd" -> "pdf" ; "pn" -> "png" ;
      "ps" -> "ps" ; "sv" -> "svg" ; "sz" -> "svgz" ; "xd" -> "xdot" ; ~vvv -> "svg" } ++ "\""
{-# INLINE printFormF #-}

printGraphFilter :: String -> IO ()
printGraphFilter xs = putStrLn $ show (take 1 xs) ++ " -- for " ++ case take 1 xs of { "c" -> "circo" ; "d" -> "dot" ;
  "f" -> "fdp" ; "n" -> "neato" ; "o" -> "osage" ; "p" -> "patchwork" ; "s" -> "sfdp" ; "t" -> "twopi" ;
    ~vvv ->  "sfdp" }
{-# INLINE printGraphFilter #-}
