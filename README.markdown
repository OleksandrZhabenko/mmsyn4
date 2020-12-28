             
             ***** Usage *****
             -----------------

1. After installation the executable mmsyn4 is created.
 Afterwards, it is used to process files. So, open an
  office spreadsheet program, e. g.
   [LibreOffice Calc](https://libreoffice.org).
  
2. Begin to enter the text in the cells. You can use
 Unicode characters. No quotation marks should be used,
  instead use some special delimiter except '@' sign.
  
3. Do not use colons, instead when needed switch to the
 nearest cell to the right.
 
4. To make a text visually highlighted (yellowish), start
 the cell with an ’@’ sign.
 
5. Lines in the table create different chains in the
 resulting graph. To produce an arrow to the text in the
 cell, enter it in the next cell in the row to the right.
 
6. To make several arrows from the cell, switch to the
 next cell to the right for this parent one (the cell that
  will be a parent for several other cells), enter needed
   new texts there and in the located below cells.
   
7. Usually, you can search the needed text with Ctrl+F if
 needed.
 
8. Empty lines in the table do not influence the resulting
 visualization. Above each line, except the first one,
  there must be at least one filled cell. It must be
   located above the text on the new line or even further
    to the right above. Otherwise, the program will
     produce no reasonably useful output.
     
9. After entering all the text, export the sheet as a 
  "*.csv" file using colons (':') as separator 
    in the working directory. Otherwise, the program 
      won’t work.
      
10. Run the appropriate executable mmsyn4 in the terminal 
  or from the command line while being in the directory 
    with the created .csv file. Specify as a command line 
      argument its name. While executing a program enter 
        a basic name of the file to be saved. DO use 
          alphanumeric symbols and dashes if needed. 
            Then specify the needed visualization scheme 
              by specifying the appropriate character 
                in the terminal and the format of the 
                  resulting visualization file (refer to 
                    GraphViz documentation for the default 
                      list of formats). For more information, 
                        see the 
  [GraphViz documentation](https://www.graphviz.org/documentation/).
                
11. Your first visualization is then created.

12. Save the spreadsheet document as a spreadsheet file (if you 
  worked with spreadsheets, otherwise this step can be omitted).

13. Repeat the steps from 2 to 12 as needed to produce
 more visualizations.
 
14. Afterwards, you have a list of graphics files, a list of .gv 
 files as source files for Graphviz, and a saved spreadsheet file. 
   Then you can use the produced visualizations for some other 
     documents.

    ***** Usage of the Next Command Line Arguments after the First One *****
    ------------------------------------------------------------------------

Since the version 0.3.0.0 mmsyn4 supports the following further 
command lines arguments (given after the first one -- see above):

-c... -- dots are instead of one letter to specify the first character 
  of the GraphViz command (e. g. \'n\' -- for \'neato\')

-f... -- dots are instead of two letters to specify the format (according to 
  the 'getFormat') of the GraphViz command (e. g. \'jp\' -- for \'jpg\')

Since the version 0.4.0.0 mmsyn4 supports the following further 
command line arguments (additionally to the previous ones):

-b... -- dots are instead of the basic name for the created files (the 
name without prefixes and extensions)

-s... -- dots are instead of one digit to specify the GraphViz splines 
functionality. 0 -- for "splines=false"; 1 -- for "splines=true"; 
2 -- for "splines=ortho"; 3 -- for "splines=polyline". The default 
one is "splines=true".

-y -- (if present) means that the '@' signs will be removed from the created 
files.

They can be given in any combinations (if needed) or omitted. In the latter 
one case the program will prompt you the needed information (but this is 
not the case for a separator, which must be specified in such a way to be 
used instead).
