Test-clean
================
Christian Pascual

``` r
library(tidyverse)
```

    ## ── Attaching packages ────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ───────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
data = read_csv("./procedure10.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   X1 = col_integer(),
    ##   cohort = col_integer(),
    ##   admission_source = col_integer(),
    ##   death = col_integer(),
    ##   discharge_destination = col_integer(),
    ##   dx_icd_typeid = col_integer(),
    ##   e_oversampled = col_integer(),
    ##   e_sampling_status = col_integer(),
    ##   still_in_hospital = col_integer(),
    ##   surgical_priority = col_integer(),
    ##   complete = col_integer(),
    ##   cpt_code = col_integer(),
    ##   cycleid = col_integer(),
    ##   dx_icd9_cd = col_integer(),
    ##   flg_letter = col_integer(),
    ##   id = col_integer(),
    ##   v2_ed_in_30 = col_integer(),
    ##   v2_readmit_in_30 = col_integer(),
    ##   v2_reop_in_30 = col_integer(),
    ##   insurance_blue_cross = col_integer()
    ##   # ... with 355 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Warning in rbind(names(probs), probs_f): number of columns of result is not
    ## a multiple of vector length (arg 1)

    ## Warning: 5 parsing failures.
    ## row # A tibble: 5 x 5 col     row col                 expected              actual file              expected   <int> <chr>               <chr>                 <chr>  <chr>             actual 1  1554 bloodglucose        no trailing characte… .3     './procedure10.c… file 2  1554 val_bloodglucose    no trailing characte… .3     './procedure10.c… row 3  2874 concurrentprocscpt… an integer            S2900  './procedure10.c… col 4  3082 concurrentprocscpt… an integer            S2900  './procedure10.c… expected 5  3083 concurrentprocscpt… an integer            S2900  './procedure10.c…

The original data set has 3084 rows and 993 columns. Many of the columns are all NA or all blank.

``` r
is_all_NA = function(col) {
  is_not_NAs = !is.na(col)
  return(sum(is_not_NAs) == 0)
}
 
test = tibble(
  data = map(data, is_all_NA)
) 
test = test %>% 
  mutate(all_NA = ifelse(data == TRUE, "Yes", "No")) %>% 
  group_by(all_NA) %>% 
  summarize(n = n())
knitr::kable(test)
```

| all\_NA |    n|
|:--------|----:|
| No      |  491|
| Yes     |  502|
