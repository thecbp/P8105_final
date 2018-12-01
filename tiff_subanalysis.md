tiff\_subanalysis
================
Tiffany Tu
11/25/2018

## How are successes distributed by patients with different diseases and prior conditions?

  - Success can be measured by Surgical Site Infection (SSI) levels
      - postop\_ssi\_super  
      - postop\_ssi\_deep
      - postop\_ssi\_organspace
  - Pre-operative health condition descriptions
      - Smoker: Tobacco use within 1 year
      - Etoh: \>2 drinks/day two weeks prior to surgery
      - Body Weight Loss: 10% of body weight loss 6 months prior to
        surgery
      - Chf: Congestive Heart Failure within 30 days prior to surgery
      - Scd : Specific Carbohydrate Diet
      - Copd: Chronic Obstructive Pulmonary Disease
      - Dvt: Deep Vein Thrombosis
      - Chronic Condition: steriods, immunosuppresive meds
      - Preop-transfusion: RBCs within 72 hours of surgery
      - Ventilator: ventilator dependent

There are three levels of severity for variables sleep apnea and
diabetes. All variables are converted to binary for this analysis and NA
entries are set to 0, indicating that the patient does not have this
condition.

``` r
dist_healthdisease = healthdisease %>% 
  select(-postop_ssi_super, -postop_ssi_deep, -postop_ssi_organspace, 
         -any_ssi, -death, -surgical_approach) %>% 
  mutate(ascites = ifelse(ascites == 2, 1, 0),
         preop_sepsis = ifelse(preop_sepsis == 1, 0, 1), 
         sleep_apnea = ifelse(sleep_apnea == 0, 0, 1)) %>%
  replace_na(list(dvt = 0, disseminated_cancer = 0, beta_blocker = 0, scd = 0,
                  heparinbid = 0, heparintid = 0)) %>% 
  rownames_to_column %>% 
  gather(condition, value, -rowname) %>% 
  spread(rowname, value) %>% 
  mutate(cases = rowSums(.[2:10868])) %>% 
  transform(condition = reorder(condition, -cases))

ggplot(dist_healthdisease, aes(x = condition, y = cases, fill = condition)) +
  geom_bar(stat = "identity") + ggtitle("Pre-operative health conditions") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "none")
```

![](tiff_subanalysis_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
