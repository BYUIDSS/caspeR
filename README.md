
# caspeR

The goal of caspeR is to be used by healthcare facilities that need to scrape the CASPER reports from 2019-Feb 2020. This was built by BYUI students in Winter 2020 to help health care providers clean their monthly pdfs from each of their facilities. This package takes the information from the CASPER reports to analyze and put it in a table which is then written to a csv for further analytic use. 

## Installation

This is a private package, so it takes a little more work to download than just doing the normal, 

``` r
devtools::install_github(byuidss/caspeR)
library(caspeR)
```
but there are directions on how to install it in the R files (see package_installation_instructions.R). 

## Example

Make sure you have all these libraries installed and loaded. You can use this code to load them all. 

``` r
install.packages("pacman")
pacman::p_load(tidyverse, devtools, caspeR, pdftools, roxygen2,zoo, stringr, glue)
```
If you have any issues with reading in the pdf, make sure you have the right file path, check the working directory ``` getwd()``` 

There is an example pdf in the R file that you can download and use to test the functions. If you are having problems with the file path, you can save the file you're currently working on in the same place you saved the downloaded pdf and go to the Session tab, Set Working Directory, To Source File Location and your pdf should read in. 

