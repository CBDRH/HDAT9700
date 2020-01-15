# HDAT9700 Statistical Modelling II

This repo contains interactive learnr tutorials for the Masters of Science in Health Data Science courses **Statistical Modelling II** delivered by the Centre for Big Data Resaerch in Health, UNSW Sydney.

## Notes
* Individual tutorials can be found in the folder inst/intutorials.
* Data, images and .css code associated with a particular tutorial can be found in the relevant folder for each tutorial. 
* The folder `inst/extdata` contains a second copy of all data files used in any tutorial, which allows the data to be accessed within R/RStudio without launching the learnr tutorial. 
* The functions `chapter1()`, `chapter2()` etc launch the learnr tutorials in an interactive session.
* The file `R/functions.R` maps the tutorials to a chapter number e.g. the first block of code maps the tutorial research-design-and-methods to the function `chapter1()`. 
* The file `R/zzz.R` contains the welcome messages that a displayed when the package is launched. 
