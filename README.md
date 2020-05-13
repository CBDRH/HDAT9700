# HDAT9700 Statistical Modelling II

This repo contains interactive learnr tutorials for the Masters of Science in Health Data Science courses **Statistical Modelling II** delivered by the Centre for Big Data Resaerch in Health, UNSW Sydney.

## Tutorial files

The learnr tutorial files can be found in the subfolder `inst/tutorials`. Long file names caused errors so the tutorials are organised under shorter pseudonyms, detailed below.

| Tutorial                              | Alias   | Author    |
| :---                                  | :---    | :---      |
| Research Design and Methods           | rdx     | Sanja     |
| DAGS                                  | dags    | Mark      |
| Propensity Score Matching             | psm     | Mark      |
| Time Series Analysis                  | tsa     | Andrea    |
| Interrupted Time Series Analysis      | its     | Andrea    |
| Multilevel Modelling I                | mlm1    | Mark      |
| Multilevel Modelling II               | mlm2    | Mark      |
| Growth Curve Modelling                | mlm3    | Kylie-Ann |
| Missing Data and Multiple Imputation  | mi      | Mark      |


## Notes on the tutorial folder structure
* Each tutorial has it's own `data`, `images` and `css` subfolders containing any data/images/css code relevant to that tutorial. 
* The `rsconnect` subfolder contains files related to the deployment of tutorials online via shinyapps.io. Generally, we won't have to edit any files in these folders.

## Updating tutorials

Below is a general workflow for updating tutorials.

0. If not done already, clone the entire package repo to your local computer. 
1. If neccessary, use the Git tab to `pull` any remote updates to your local computer.
2. Make changes in the tutorial .rmd file.
3. Run the .rmd file to make sure you are happy with your changes, and to update the corresponding .html file.
4. Use Git to `add` and `commit` your changes. 
5. When you are ready, use Git to `push` your changes to the remote repo.

## Notes on the package
* The package name is `hdat9700tutorials`
* The functions `chapter1()`, `chapter2()` etc launch the learnr tutorials in an interactive session.
* The file `R/functions.R` contains code to map the learnr tutorials to the correct chapter number function. For example, the first block of code maps the tutorial research-design-and-methods (i.e. "rdx") to the function `chapter1()`. This makes it simple to re-order the chapters as required.
* The file `R/zzz.R` contains a list of quotes, one of which is displayed at random on the screen when the package is loaded. 

## A note on data shipped with the package 
* The folder `inst/extdata` contains a second copy of the data files used across all tutorials. This allows students to access the data within R/RStudio without launching the learnr tutorial (provided they have the package installed). 
* For example, the following code could be used within R Studio to read the file `yrs2015.csv`: `data <- read.csv(system.file("extdata", "yrs2015.csv", package="hdat9700tutorials"))`
