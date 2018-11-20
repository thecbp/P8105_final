Report Rough Draft
================
11/19/2018

Introduction
============

...

Project Motivation
------------------

...

Data Characterization
=====================

Before we start our analyses, it's important for us to understand our data in its original form.

Libraries and settings
----------------------

``` r
# Our data analysis toolkit
library(tidyverse)
```

    ## ── Attaching packages ──── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.8
    ## ✔ tidyr   0.8.2     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ─────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
# Formatting plot output
knitr::opts_chunk$set(
  out.width = "90%"
)

# Set the plot design
theme_set(theme_classic() + 
            theme(legend.position = "bottom", 
                  plot.title = element_text(hjust = 0.5)))
```

Our raw data is contained in `procedure10.csv`, so we'll bring it in and have a look at its size.

``` r
colectomies = read.csv(file = './procedure10.csv') 

# TODO: Figure out how to grab data from specific URL (ie OneDrive)
```

In its raw form, `colectomies` has 3084 rows and 993 columns. Each row in this dataset corresponds to a single colectomy with an incredible amount of information associated with it. The data has already been purged of identifiable personal information, leaving us with laboratory data, disease data and patient characteristics.

That being said, many of the columns are useless to us. Many columns are contain mostly or only missing data, denoted by blank cells or `NA`. Before we can start the variable selection for our model, we need to tidy up the dataset.

Data Cleaning
=============
