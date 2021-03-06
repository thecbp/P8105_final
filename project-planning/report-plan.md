Final Report Components
================
11/16/2018

Introduction
------------

-   context about colectomy
-   where does our data come from
-   motivation for project

Data Characterization
---------------------

-   how many rows and columns originally
-   what are some key variables in the dataset?
-   hospital data, patient data, laboratory data, disease data
-   key outcome: SSI, surgical site infection
-   brief summary of the above variables

Data Cleaning
-------------

-   remove columns with &gt;50% missing values
-   remove duplicated columns
-   defining variables to keep from literature review
-   renaming columns to be more informative
-   recoding columns (0/1 to logical, categories to factors)

Subanalyses
-----------

-   how many different hospitals are represented in the data?
-   how many of their surgeries result in SSI?
-   how are successes distributed by patients with different diseases
-   how are successes distributed by insurance status?
-   how are successes associated with ASA level?
-   does the number of surgeries & successes change with time?

Modeling
--------

-   make model based on variables of interest from subanalyses
-   cross-validation
-   comparison to some other model from literture

Conclusion
----------

-   What factors are associated with SSI?
-   positively? negatively?
-   What factors ended up being non-significant?
