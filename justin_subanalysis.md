Justin Sub-analysis
================
Justin Hsie
11/30/2018

How are successes distributed by insurance status?
--------------------------------------------------

Measure success with SSI. Variables are postop\_ssi\_super, postop\_ssi\_deep, and postop\_ssi\_organspace

#### Setup

``` r
library(tidyverse)
```

#### Clean data

Removing duplicate columns containing "flg\_" and "e\_", and &gt;50% missing value.

``` r
insurance = read_csv("./procedure10.csv") %>% 
  select(insurance_payment_type)
data = read_csv("./procedure10.csv") %>% 
  select(-contains("flg_"), -contains("e_")) %>%
  select_if(colSums(is.na(.)) < nrow(.) * 0.5)
colectomy = select(data, postop_ssi_super, postop_ssi_deep, postop_ssi_organspace)
```
