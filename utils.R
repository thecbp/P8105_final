# Used for filtering for unneeded columns
is_mostly_intact = function(col) {
  # Account for literal NAs and blanks
  missings = is.na(col) | col == "" | col == "NA"
  return(sum(missings) <= (3084 / 2))
}

# Used for making column names more readable
prettify_names = function(df) {
  # Rename columns specific to colectomy data
  new_df = df %>% 
    rename(
      surgeon_id	= surgeon_cid_160801, 
      death_status = death,
      surg_priority_status = surgical_priority,
      is_complete = complete,	
      v2_ed_in_30 = v2_ed_in_30, #CHECK
      v2_readmit_in_30 = v2_readmit_in_30, #CHECK
      v2_reop_in_30	= v2_reop_in_30, #CHECK
      is_hispanic = ethnicity_hispanic,	
      had_ascites = ascites,	
      had_bleeding_disorder = bleeding_disorder,
      had_body_weight_loss = body_weight_loss,
      had_chf = chf,
      had_chronic_cond = chronic_condition,
      had_copd = copd,
      had_c_artery_prob = coronary_artery,
      had_diabetes = diabetes,
      is_on_dialysis = dialysis,
      had_dis_cancer = disseminated_cancer,
      had_dvt = dvt,
      had_etoh = etoh,	
      had_family_dvt = family_dvt,
      height_unit	= height_unit, #CHECK
      had_hypertension = hypertension,
      had_open_wound = open_wound,
      had_pvd = peripheral_vascular_disease,
      had_pneumonia = pneumonia,
      had_preop_sepsis = preop_sepsis,
      had_preop_transfusion = preop_transfusion,
      had_sleep_apnea = sleep_apnea,
      is_smoker = smoker,
      is_on_ventilator = ventilator,
      is_on_beta_blocker = beta_blocker,
      heparinbid = heparinbid,
      heparintid = heparintid, 
      has_30d_fu = followed_for_30_days,	
      times_readmitted = readmission_count,
      times_reoperated = reoperation_count,
      skin_antisepsis_status = skin_antisepsis,
      had_epidural = epidural_use,	
      admit_to_icu_postsurg = icu_admission_after_surgery,
      had_cathether = indwelling_catheter,
      lmwh	= lmwh, #CHECK
      on_anticoag = other_anticoag,
      had_scd = scd,
      age = val_age,
      height = val_height,
      weight = val_weight, 
      bmi = val_bmi,
      length_of_stay = val_los,
      intraop_temp_celsius = intraop_temp_c,	
      operation = op_year
    )
    
  return(new_df)
}