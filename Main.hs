{-|
Module      : Main
Description : The "glue" between electronic tables and GraphViz
Copyright   : (c) OleksandrZhabenko, 2017-2020
License     : MIT
Maintainer  : olexandr543@yahoo.com
Stability   : Experimental

A program @mmsyn4@ converts a specially formated @.csv@ file with a colon as a field separator obtained from the electronic table 
into a visualized by GraphViz graph in the one of the supported by GraphViz format. The proper GraphViz installation is required.
-}

module Main where

import MMSyn4 (process2)
import System.Environment (getArgs)
import System.Directory
import Data.List (isPrefixOf)

{-| Usage with only the first command line

1. After installation the executable mmsyn4 is created. Afterwards, it is used to process files. So, open an office spreadsheet program, 
e. g. LibreOffice Calc.
2. Begin to enter the text in the cells. You can use Unicode characters. No quotation marks should be used, instead use some 
special delimiter except '@' sign.
3. Do not use colons, instead when needed switch to the nearest cell to the right. 
4. To make a text visually highlighted (yellowish), start the cell with an ’@’ sign.
5. Lines in the table create different chains in the resulting graph. To produce an arrow to the text in the cell, enter it in the next cell 
in the row to the right.
6. To make several arrows from the cell, switch to the next cell to the right for this parent one (the cell that will be a parent 
for several other cells), enter needed new texts there and in the located below cells.
7. Usually, you can search the needed text with Ctrl+F if needed.
8. Empty lines in the table do not influence the resulting visualization. Above each line, except the first one, there must be at least one filled cell. 
It must be located above the text on the new line or even further to the right above. Otherwise, the program will produce no reasonably useful output.
9. After entering all the text, export the sheet as a \"\*.csv\" file using colons (\':\') as separator in the working directory. Otherwise, 
the program won’t work.
10. Run the appropriate executable mmsyn4 in the terminal or from the command line while being in the directory with the created .csv file. 
Specify as a command line argument its name. While executing a program enter a word name of the .csv file to be saved. DO use alphanumeric symbols 
and dashes if needed. Then specify the needed visualization scheme by specifying the appropriate character in the terminal and the format of the 
resulting visualization file (refer to GraphViz documentation for the default list of formats).
11. Your first visualization is then created. 
12. Save the spreadsheet document as a spreadsheet file (if you worked with spreadsheets, otherwise this step can be omitted). 
13. Repeat the steps from 2 to 12 as needed to produce more visualizations. 
14. Afterwards, you have a list of graphics files, a list of .gv files -- source files for Graphviz -- and a saved spreadsheet file. 
Then you can use the produced visualizations for some other documents.

Usage of the next command line arguments after the first one

Since the version 0.3.0.0 mmsyn4 supports the following further command lines arguments (given after the first one -- see above):

-c... -- dots are instead of one letter to specify the first character of the GraphViz command (e. g. \'n\' -- for \'neato\')

-f... -- dots are instead of two letters to specify the format (according to the 'getFormat') of the GraphViz command (e. g. \'jp\' -- for \'jpg\')

They can be given in any combinations (if needed) or omitted. In the latter one case the program will prompt you the needed information.
-}
main :: IO ()
main = do 
  args <- getArgs
  let arg0 = concat . take 1 $ args
      arggs = drop 1 args
      xxs = concatMap (take 1 . drop 2) . filter ("-c" `isPrefixOf`) $ arggs
      yys = concatMap (take 2 . drop 2) . filter ("-f" `isPrefixOf`) $ arggs
      bnames = concatMap (drop 2) . filter ("-b" `isPrefixOf`) $ arggs
      splines = concatMap (take 1 . drop 2) . filter ("-s" `isPrefixOf`) $ arggs
      remAts = concatMap (take 1 . drop 1) . filter ("-y" `isPrefixOf`) $ arggs
  exI <- doesFileExist arg0
  if exI 
    then do 
      text2 <- readFile arg0
      process2 text2 xxs yys bnames splines remAts
    else putStrLn "No file specified exists in the current directory! "  
