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
      length_of_stay = val_los,	
      operation = op_year
    )
    
  return(new_df)
}

# Function to help assure columns are of the correct type
catfactory = function(df) {
  new_df = df %>% 
    mutate(
      cohort = as.factor(cohort),
      admission_source = as.factor(admission_source),
      discharge_destination = as.factor(discharge_destination),
      still_in_hospital = as.logical(still_in_hospital),
      surg_priority_status = as.factor(surg_priority_status),
      insurance_payment_type = as.factor(insurance_payment_type),
      race = as.factor(race),
      sex = as.factor(sex),
      is_hispanic = as.factor(is_hispanic),
      had_ascites = as.logical(had_ascites),
      had_bleeding_disorder = as.logical(had_bleeding_disorder),
      had_body_weight_loss = as.logical(had_body_weight_loss),
      had_chf = as.logical(had_chf),
      had_chronic_cond = as.logical(had_chronic_cond),
      had_copd = as.logical(had_copd),
      had_c_artery_prob = as.logical(had_c_artery_prob),
      had_diabetes = as.logical(had_diabetes),
      is_on_dialysis = as.logical(is_on_dialysis),
      had_dis_cancer = as.logical(had_dis_cancer),
      had_dvt = as.logical(had_dvt),
      had_etoh = as.logical(had_etoh),
      functional_status = as.factor(functional_status),
      had_hypertension = as.logical(had_hypertension),
      had_open_wound = as.logical(had_open_wound),
      had_pvd = as.logical(had_pvd),
      had_pneumonia = as.logical(had_pneumonia),
      had_preop_sepsis = as.logical(had_preop_sepsis),
      had_preop_transfusion = as.logical(had_preop_transfusion),
      had_sleep_apnea = as.logical(had_sleep_apnea),
      is_smoker = as.logical(is_smoker),
      is_on_ventilator = as.logical(is_on_ventilator),
      is_on_beta_blocker = as.logical(is_on_beta_blocker),
      presence_drains = as.factor(presence_drains),
      surgical_approach = as.factor(surgical_approach),
      asa_class_id = as.factor(presence_drains),
      had_epidural = as.logical(had_epidural),
      had_cathether = as.logical(had_cathether),
      nerve_block = as.logical(nerve_block),
      on_anticoag = as.factor(on_anticoag),
      had_scd = as.logical(had_scd)
    )
  return(new_df)
}

# The _states variables are for use in the Shiny application
patient_states_util = c(
  "Age" = "age",
  "BMI" = "bmi",
  "Functional Status" = "functional_status",
  "Height (in)" = "height_in",
  "Length of Stay" = "length_of_stay",
  "On Ventilator" = "is_on_ventilator",
  "On Beta Blockers" = "is_on_beta_blocker",
  "On LMWH" = "lmwh",
  "On Nerve Blockers" = "nerve_block",
  "On Anticoagulants" = "on_anticoag",
  "Race" = "race",
  "Severity" = "asa_class_id",
  "Sex" = "sex",
  "Smoker" = "is_smoker",
  "Still In Hospital" = "still_in_hospital",
  "Weight (lb)" = "weight_lb"
)

disease_states_util = c(
  "Ascites" = "had_ascites",
  "Bleeding Disorder" = "had_bleeding_disorder",
  "CHF" = "had_chf",
  "Chronic Condition" = "had_chronic_cond",
  "COPD" = "had_copd",
  "Coronary Artery Problem" = "had_c_artery_prob",
  "Diabetes" = "had_diabetes",
  "DVT" = "had_dvt",
  "ETOH" = "had_etoh",
  "Hypertension" = "had_hypertension",
  "Open Wound" = "had_open_wound",
  "PVD" = "had_pvd",
  "Pneumonia" = "had_pneumonia",
  "Sleep Apnea" = "had_sleep_apnea",
  "SCD" = "had_scd"
)

surgery_states_util = c(
  "Drain Presence" = "presence_drains",
  "Had Cathether" = "had_cathether",
  "Had Epidural" = "had_epidural",
  "Placed in ICU Postop" = "admit_to_icu_postsurg",
  "Preop Sepsis" = "had_preop_sepsis",
  "Preop Transfusion" = "had_preop_transfusion",
  "Surgery Year" = "operation",
  "Surgical Approach" = "surgical_approach",
  "Surgery Priority" = "surg_priority_status"
)

lab_states_util = c(
  "Albumin" = "albumin",
  "Bilirubin" = "bilirubin",
  "Blood Glucose" = "bloodglucose",
  "Creatinine" = "creatinine",
  "Hematocrit" = "hct",
  "Hemoglobin" = "hemoglobin",
  "INR" = "inr",
  "Lactate" = "lactate",
  "Plate Count" = "platecount",
  "WBC" = "wbc"
)
