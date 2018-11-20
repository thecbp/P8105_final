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

In its raw form, `colectomies` has 3084 rows and 993 columns. Each row in this dataset corresponds to a single colectomy with an incredible amount of information associated with it. The data has already been purged of identifiable personal information, leaving us with a bevy of laboratory, disease and patient data.

That being said, many of the columns are useless for regression analysis. Many columns are contain mostly or only missing data, denoted by blank cells or `NA`. Before we can start the variable selection for our model, we need to tidy up the dataset.

Data Cleaning
=============

To start the cleaning, we want to remove all columns that contain more than 50% missing values (either blank cells or `NA`). Furthermore, some of the columns actually contain duplicate information from others. These duplicate columns start with `flg` or `e`.

``` r
tidy_colectomies = colectomies %>% 
  select(-contains("flg_"), -contains("e_")) %>%
  select_if(colSums(is.na(.)) < nrow(.) * 0.5)
```

After the removal of duplicate columns and columns with more than 50% missing data, `tidy_colectomies` comes to be 3084 rows and 196 columns.
