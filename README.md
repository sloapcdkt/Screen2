# Data Screening Tools for San Luis Obispo County APCD
This repository contains R functions and RMarkdown templates used by the [San Luis Obispo County Air Pollution Control District](http://www.slocleanair.org/) for data screening during validation.

These tools are highly customized to the SLO County APCD situation; they are not likely to work in other situations unless modified. They are presented here for informational/inspirational purposes only.

The `manual.html` file contains detailed instructions for how to use the screening tools along with examples. The easiest way view the manual is to click on the "Clone or download" button, above, and download a zip file of this repository. Unzip the file, and click on `manual.html` to view the manual in your browser.

## File Descriptions
### The Manual and related files
* `manual.html` HTML manual describing how to use the data screening tools
* `manual.Rmd` R markdown file with code for generating `manual.html`
* `/Examples`  Contains the example html output of the screens discussed in the manual
* `/images` Images used in the manual

### Screening Tool
* `functions.R` R code for the functions used to generate the screens. "Source" this file to import the screening functions into your R session.
* `screen.Rmd` Template for the general screen
* `metscreen.Rmd` Template for the meteorological screen
* `pollscreen.Rmd` Template for the pollutant screen
* `calibrations.Rmd` Template for the calibrations report
* `exceedences.Rmd` Template for the exceedence report

### Data Archive
The meteorological and pollutant screens compare data from the current month & year to validated data collected in previous years. To do this, there needs to be an archive of data from earlier years.

* `create-archive.R` Script for creating the needed archive files from an AQS download
* `arch-atas.csv` Data archive for Atascadero site (used for examples in the manual)
* `arch-....` Archives for other sites are need for running meteorological and pollutant screens but are **not included** in this repository.

### Other
* `README.md` This description/information file
* `Screen2.Rproj`
* `.gitignore`

## Putting It All Together
This repository should contain all of the code and data needed to reproduce the examples in the manual. As described in the manual, you will need to have R (3.4.0 or higher), RStudio, and a few additional packages installed to run the screens. R can be downloaded from https://www.r-project.org/ and RStudio from  https://www.rstudio.com/. 

This project depends on the follows packages: `shiny`, `rmarkdown`, and `openair` and `dplyr`. You can easily install them from within RStudio, or they will be installed automatically the first time you try to use the screening tools (see below).

Once the necessary software is installed, download this repository by clicking the "Clone or download" button, above. Unzip the file and double click on `Screen2.Rproj`. This should launch RStudio. Source the `functions.R` script by either 1) clicking on it in RStudio "Files" pane and then clicking the "Source" button, or 2) entering `source('functions.R')` into the R console. 

The script will first check if all of the required packages are installed. Any missing packages will be installed automatically, which may take a few minutes.

The tools should now be all set up, but to fully reproduce the examples in the manual, you will need to create a sub folder called `data` and **copy** the following files from the `Examples` folder into the new `data` folder: 

* `AT-2017-01-BAM10.csv`
* `AT-2017-01-BAM25.csv`
* `AT-2017-01.csv`
* `example-RH-062015.csv`
* `NRP-2016-03.csv`
* `AQSReport.txt`



