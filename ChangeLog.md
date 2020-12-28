# Revision history for mmsyn4

## 0.1.0.0 -- 2019-10-17

* First version. Released on an unsuspecting world.

## 0.1.1.0 -- 2019-10-18

* First version revised A. Some documentation and .cabal file improvements.

## 0.1.1.1 -- 2019-10-18

* First version revised B. Some minor documentation and .cabal file improvements.

## 0.1.2.0 -- 2019-10-22

* First version revised C. Changed the output files scheme. Avoided a pipe redirection in the terminal.
 Some minor documentation and .cabal file improvements.

## 0.1.3.0 -- 2019-12-16

* First version revised D. Added the possibility to avoid using the at-sign in the resulting visualization file. Added the possibility to choose
different splines schemes according to the GraphViz documentation.

## 0.1.4.0 -- 2019-12-17

* First version revised E. Added filtering for not to duplicate records in the
.gv file. Some minor documentation improvement.

## 0.1.5.0 -- 2019-12-24

* First version revised F. Changed dependency bounds so that it can now be compiled for GHC 8.8.1.

## 0.1.6.0 -- 2020-01-31

* First version revised G. Changed README to README.markdown

## 0.2.0.0 -- 2020-05-14

* Second version. Changed the bounds for dependencies so that now also GHC 8.10* series is supported. Changed a module structur so that now it has
additional module MMSyn4 with almost all functions in it (they mostly are not exported because of their specific usage). Added possibility
to specify another basic file to work with except 1.csv. Added a possibility to specify an output graphics format. Some code and
documentation improvements.

## 0.3.0.0 -- 2020-05-16

* Third version. The code is almost fully rewritten. Fixed issues with wrong processing and formatting of the file. Fixed issue with double
printed prompt message in the MMSyn4 module. Changed the exported and imported functions and dependencies for modules. Added the possibility
to specify three additional command line arguments to specify the command to be executed to create the visualization file and its format.
So, now the program behavior can be basically controlled with command line arguments.

## 0.3.1.0 -- 2020-05-16

* Third version revised A. Added CPP pre-processor to support compilation also for GHC-7.8* series.

## 0.3.1.1 -- 2020-05-16

* Third version revised B. Trying the same as above with more specific syntaxis and LANGUAGE CPP pragma.

## 0.4.0.0 -- 2020-05-19

* Fourth version. Added the new command line arguments "-y", "-s...", "-b..." to specify whether to remove the '@' markers, which option is used to splines
GraphViz functionality and what is the basic name for the created files respectively. For this changed the functions. Some documentation additions
respectively.

## 0.5.0.0 -- 2020-10-29

* Fifth version. Changed the inlining policies. Some minor code improvements. Boundaries changes for dependencies.

## 0.6.0.0 -- 2020-12-15

* Sixth version. Switched to the GHC.Arr module from the base package. Removed vector and mmsyn2 as dependencies.
Some code improvements.

## 0.6.1.0 -- 2020-12-15

* Sixth version revised A. Removed also mmsyn2-array as a dependency.

## 0.6.2.0 -- 2020-12-16

* Sixth version revised B. Fixed issue with incorrectly defined fold that has led to incorrect some results.

## 0.6.3.0 -- 2020-12-16

* Sixth version revised C. Fixed another issue with incorrectly defined fold that has led to incorrect some results.

