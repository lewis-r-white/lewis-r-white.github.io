## Creating Clean Stress Surveys

# Load necessary libraries
library(readxl)     # reading Excel files
library(tidyverse)      # data manipulation
library(labelled)   # labeling columns 
library(here) # file paths
library(haven)


# Importing Birth Time Data 

delivery_date <- read_csv(here("data", "GRAPHS_datdeliv_vname.csv"))



# Importing data from Excel ----
stress_NLE <- read_dta(here("data", "Stress_NLE.dta"))

# acquire length of gestational period
NLE_covariates <- stress_NLE %>%
  select(mstudyid, GESTAGE_DAYS, married, wealthindex, age, medlev, fan, pregchn) %>%
  rename(gestage_days = GESTAGE_DAYS)

crisis_data <- read_excel(here("data", "CAL_stress.xlsx"), sheet = "Crisis")
stress_data <- read_excel(here("data", "CAL_stress.xlsx"), sheet = "Pss")

pssfup_with_pss <- read_excel(here("data", "CAL_stress.xlsx"), sheet = "Pssfup with Pss")
pssfup_no_pss <- read_excel(here("data", "CAL_stress.xlsx"), sheet = "Pssfup no Pss")
# pss_no_pssfup <- read_excel(here("data", "CAL_stress.xlsx"), sheet = "Pss No Pssfup") #just a duplicate



pss_clean <- bind_rows(stress_data, pssfup_no_pss, pssfup_with_pss) %>%
  mutate(survey_date = case_when(is.na(quessetd) & !is.na(interviewdate) ~ interviewdate,
                                 !is.na(quessetd) & is.na(interviewdate) ~ quessetd,
                                 TRUE ~ NA)) %>%
  mutate(survey_time = case_when(is.na(quessetdt) & !is.na(time) ~ time,
                                 !is.na(quessetdt) & is.na(time) ~ quessetdt,
                                 TRUE ~ NA)) %>%
  select(-c(quessetd, quessetdt, interviewdate, time)) %>%
  mutate(
    survey_time = sprintf("%04d", survey_time),  # Ensure survey_time has 4 digits
    survey_time = sub("(\\d{2})(\\d{2})", "\\1:\\2", survey_time),  # Insert colon
    survey_datetime = as.POSIXct(paste(survey_date, survey_time), format = "%Y-%m-%d %H:%M")  # Combine date and time
  )



# Recording postnatal PSS scores
pss_recode <- pss_clean %>%
  mutate(
    recode_psa = psa,
    recode_psb = case_when(psb == 0 ~ 4, psb == 1 ~ 3, psb == 2 ~ 2, psb == 3 ~ 1, psb == 4 ~ 0),
    recode_psc = case_when(psc == 0 ~ 4, psc == 1 ~ 3, psc == 2 ~ 2, psc == 3 ~ 1, psc == 4 ~ 0),
    recode_psd = psd
  ) %>%
  mutate(
    PSS4 = ifelse(rowSums(is.na(select(., recode_psa, recode_psb, recode_psc, recode_psd))) > 1, NA, rowSums(select(., recode_psa, recode_psb, recode_psc, recode_psd), na.rm = TRUE))
  ) %>%
  left_join(delivery_date) %>% 
  mutate(diffdays = difftime(datdeliv, survey_date, units = "days")) %>%
  mutate(
    term_at_survey = case_when(
      diffdays > 42 ~ "far_postnatal",
      diffdays >= 0 & diffdays <= 42 ~ "post_partem",
      diffdays < 0 & diffdays >= -84 ~ "third_trimester",
      diffdays < -84 & diffdays >= -189 ~ "second_trimester",
      diffdays < -189 & diffdays >= -280 ~ "first_trimester",
      diffdays < -280 ~ "pre_conception"
    )
  ) %>%
  
  # ## ASSUMPTION: If questionnaire date and delivery date are the same, assuming participant responded to survey before going into labor
  mutate(survey_pre_post_birth = ifelse(diffdays > 0, "post_delivery", "pre_delivery")) %>%
  
  # Add in gestation period
  left_join(NLE_covariates) 



## Recoding crisis data so b question is NA when a indicates event did not occur / NA response ----

### LET 3 contiunue to represent NA in the data 

crisis_recode <- crisis_data %>%
  mutate(
    # Replace `9` with 3 for all columns starting with "a"
    across(matches("^a\\d+$"), ~ ifelse(. == 9, 3, .)),
    
    # Step 2: Iterate over the "a" and "b" pairs to apply the second rule
    across(matches("^b\\d+$"), ~ {
      # Extract the column number from the "b" column name
      col_num <- gsub("^b", "", cur_column())
      # Get the corresponding "a" column
      a_col <- paste0("a", col_num)
      # Make "b" value NA if "a" is 0 or NA
      ifelse(get(a_col) %in% c(0, 3), 3, .)
    })
  )
  

# Developing NLE score ----

crisis_recode <- crisis_recode %>%
  
  # replace 3 with NA to reflect what skip logic would have done
  # mutate(across(matches("^b\\d+$"), ~ replace(., . == 3, NA))) %>%

  # Calculate negative perception counts based on "b" columns
  
  # CHANGES FROM SAS CODE: REMOVED A1, A4, A72 as positive events
  mutate(
    fin_events = rowSums(select(., a2, a3, a5, a6, a7, a8, a9, a10, a11, a12, a14) == 1, na.rm = TRUE),
    crifinneg = rowSums(select(., b2, b3, b5, b6, b7, b8, b9, b10, b11, b12, b14) == 1, na.rm = TRUE),
    fin_nds = ifelse(crifinneg > 0, 1, 0),
    
    leg_events = rowSums(select(., a15, a16, a17, a18, a19) == 1, na.rm = TRUE),
    crilegalneg = rowSums(select(., b15, b16, b17, b18, b19) == 1, na.rm = TRUE),
    leg_nds = ifelse(crilegalneg > 0, 1, 0),
    
    car_events = rowSums(select(., a27, a73, a75) == 1, na.rm = TRUE),
    cricareerneg = rowSums(select(., b27, b73, b75) == 1, na.rm = TRUE),
    car_nds = ifelse(cricareerneg > 0, 1, 0),
    
    rel_events = rowSums(select(., a20, a21, a22, a24, a29, a30, a31, a32, a33, a34, a35, a36, a37) == 1, na.rm = TRUE),
    crirelneg = rowSums(select(., b20, b21, b22, b24, b29, b30, b31, b32, b33, b34, b35, b36, b37) == 1, na.rm = TRUE),
    rel_nds = ifelse(crirelneg > 0, 1, 0),
    
    homesf_events = rowSums(select(., a39, a40, a41) == 1, na.rm = TRUE),
    crihomesafeneg = rowSums(select(., b39, b40, b41) == 1, na.rm = TRUE),
    homesf_nds = ifelse(crihomesafeneg > 0, 1, 0),
    
    neighsf_events = rowSums(select(., a42, a43, a44, a45, a46, a47, a48, a38) == 1, na.rm = TRUE),
    crineighsafeneg = rowSums(select(., b42, b43, b44, b45, b46, b47, b48, b38) == 1, na.rm = TRUE),
    neighsf_nds = ifelse(crineighsafeneg > 0, 1, 0),
    
    medself_events = rowSums(select(., a52, a54, a55) == 1, na.rm = TRUE),
    crimedselfneg = rowSums(select(., b52, b54, b55) == 1, na.rm = TRUE),
    medself_nds = ifelse(crimedselfneg > 0, 1, 0),
    
    medoth_events = rowSums(select(., a53, a56, a57, a58) == 1, na.rm = TRUE),
    crimedothneg = rowSums(select(., b53, b56, b57, b58) == 1, na.rm = TRUE),
    medoth_nds = ifelse(crimedothneg > 0, 1, 0),
    
    home_events = rowSums(select(., a59, a60, a61, a62, a63, a64, a13) == 1, na.rm = TRUE),
    crihomeneg = rowSums(select(., b59, b60, b61, b62, b63, b64, b13) == 1, na.rm = TRUE),
    home_nds = ifelse(crihomeneg > 0, 1, 0),
    
    prej_events = rowSums(select(., a66, a67, a68, a69, a70) == 1, na.rm = TRUE),
    criprejneg = rowSums(select(., b66, b67, b68, b69, b70) == 1, na.rm = TRUE),
    prej_nds = ifelse(criprejneg > 0, 1, 0),
    
    auth_events = rowSums(select(., a74, a28, a65) == 1, na.rm = TRUE),
    criauthneg = rowSums(select(., b74, b28, b65) == 1, na.rm = TRUE),
    auth_nds = ifelse(criauthneg > 0, 1, 0),
    
    # Other specific negative events
    oth_events = rowSums(select(., a25, a26, a50, a51, a23) == 1, na.rm = TRUE),
    crireadneg = ifelse(b25 == 1, 1, 0),
    cricomneg = ifelse(b26 == 1, 1, 0),
    cridrugneg = ifelse(b50 == 1, 1, 0),
    cripdrugneg = ifelse(b51 == 1, 1, 0),
    crikidneg = ifelse(b23 == 1, 1, 0),
    criothneg = rowSums(select(., crireadneg, cricomneg, cridrugneg, cripdrugneg, crikidneg) == 1, na.rm = TRUE),
    oth_nds = ifelse(criothneg > 0, 1, 0)
  ) %>%
  
  # Summing across all domain scores
  mutate(
    total_events = rowSums(select(., fin_events, leg_events, car_events, rel_events, homesf_events, neighsf_events, 
                                  medself_events, medoth_events, home_events, prej_events, auth_events, oth_events), 
                           na.rm = TRUE),
    
    total_negative_responses = rowSums(select(., crifinneg, crilegalneg, cricareerneg, crirelneg, crihomesafeneg, crineighsafeneg, 
                                              crimedselfneg, crimedothneg, crihomeneg, criprejneg, criauthneg, criothneg), 
                                       na.rm = TRUE),
    
    sum_nds = rowSums(select(., fin_nds, leg_nds, car_nds, rel_nds, homesf_nds, neighsf_nds, 
                                              medself_nds, medoth_nds, home_nds, prej_nds, auth_nds, oth_nds), 
                                       na.rm = TRUE)
  ) %>%
  
  # whether event was experienced 
  mutate(
    experienced_auth = ifelse(auth_events > 0, 1, 0),
    experienced_fin = ifelse(fin_events > 0, 1, 0),
    experienced_leg = ifelse(leg_events > 0, 1, 0),
    experienced_car = ifelse(car_events > 0, 1, 0),
    experienced_rel = ifelse(rel_events > 0, 1, 0),
    experienced_homesf = ifelse(homesf_events > 0, 1, 0),
    experienced_neighsf = ifelse(neighsf_events > 0, 1, 0),
    experienced_medself = ifelse(medself_events > 0, 1, 0),
    experienced_medoth = ifelse(medoth_events > 0, 1, 0),
    experienced_home = ifelse(home_events > 0, 1, 0),
    experienced_prej = ifelse(prej_events > 0, 1, 0),
    experienced_oth = ifelse(oth_events > 0, 1, 0)
  ) %>%
  
  # Change NDS to no event if no event in the domain was experienced (essentially add NA for negative event if event didn't occur)
  mutate(
    auth_nds = ifelse(experienced_auth == 1, auth_nds, "No Event"),
    fin_nds = ifelse(experienced_fin == 1, fin_nds, "No Event"),
    leg_nds = ifelse(experienced_leg == 1, leg_nds, "No Event"),
    car_nds = ifelse(experienced_car == 1, car_nds, "No Event"),
    rel_nds = ifelse(experienced_rel == 1, rel_nds, "No Event"),
    homesf_nds = ifelse(experienced_homesf == 1, homesf_nds, "No Event"),
    neighsf_nds = ifelse(experienced_neighsf == 1, neighsf_nds, "No Event"),
    medself_nds = ifelse(experienced_medself == 1, medself_nds, "No Event"),
    medoth_nds = ifelse(experienced_medoth == 1, medoth_nds, "No Event"),
    home_nds = ifelse(experienced_home == 1, home_nds, "No Event"),
    prej_nds = ifelse(experienced_prej == 1, prej_nds, "No Event"),
    oth_nds = ifelse(experienced_oth == 1, oth_nds, "No Event")
  ) %>%
  
  # change #negative events expeirenced to "no event" if no event in the domain was experienced 
  mutate(
    criauthneg = ifelse(experienced_auth == 1, criauthneg, "No Event"),
    crifinneg = ifelse(experienced_fin == 1, crifinneg, "No Event"),
    crilegalneg = ifelse(experienced_leg == 1, crilegalneg, "No Event"),
    cricareerneg = ifelse(experienced_car == 1, cricareerneg, "No Event"),
    crirelneg = ifelse(experienced_rel == 1, crirelneg, "No Event"),
    crihomesafeneg = ifelse(experienced_homesf == 1, crihomesafeneg, "No Event"),
    crineighsafeneg = ifelse(experienced_neighsf == 1, crineighsafeneg, "No Event"),
    crimedselfneg = ifelse(experienced_medself == 1, crimedselfneg, "No Event"),
    crimedothneg = ifelse(experienced_medoth == 1, crimedothneg, "No Event"),
    crihomeneg = ifelse(experienced_home == 1, crihomeneg, "No Event"),
    criprejneg = ifelse(experienced_prej == 1, criprejneg, "No Event"),
    criothneg = ifelse(experienced_oth == 1, criothneg, "No Event")
  ) %>%

  mutate(
    # Resilience score based on total events and total negative responses
    resilience_score = ifelse(total_events > 0, 1 - (total_negative_responses / total_events), NA)
  ) %>%
  
  #recateogrize NDS to low, moderate, high
  mutate(sum_nds_category = factor(
    case_when(sum_nds <= 2 ~ "low",
              sum_nds >= 3 & sum_nds <= 5 ~ "moderate",
              sum_nds >= 6 ~ "high"),
    levels = c("low", "moderate", "high"),
    ordered = TRUE
  )) %>%
  
  # add delivery date
  left_join(delivery_date) %>% 
  
  # calculate difference in time between delivery and survey date
  mutate(diffdays = difftime(datdeliv, quessetd, units = "days")) %>%
  
  #classify survey date in relation to pregnancy period
  mutate(
    term_at_survey = case_when(
      diffdays > 42 ~ "far_postnatal",
      diffdays >= 0 & diffdays <= 42 ~ "post_partem",
      diffdays < 0 & diffdays >= -84 ~ "third_trimester",
      diffdays < -84 & diffdays >= -189 ~ "second_trimester",
      diffdays < -189 & diffdays >= -280 ~ "first_trimester",
      diffdays < -280 ~ "pre_conception"
    )
  ) %>%
  
  # ## ASSUMPTION: If questionnaire date and delivery date are the same, assuming participant responded to survey before going into labor
  mutate(survey_pre_post_birth = ifelse(diffdays > 0, "post_delivery", "pre_delivery"))  %>%
  
  # Add in gestation period
  left_join(NLE_covariates)
  
  

# Setting up labels for clarity
var_label(crisis_recode$criauthneg) <- "Sum of prehome authority negative events"
var_label(crisis_recode$cricareerneg) <- "Sum of prehome career negative events"
var_label(crisis_recode$crifinneg) <- "Sum of prehome financial negative events"
var_label(crisis_recode$crihomeneg) <- "Sum of prehome home negative events"
var_label(crisis_recode$crihomesafeneg) <- "Sum of prehome home safety negative events"
var_label(crisis_recode$crilegalneg) <- "Sum of prehome legal negative events"
var_label(crisis_recode$crimedselfneg) <- "Sum of prehome self medical issues negative events"
var_label(crisis_recode$crimedothneg) <- "Sum of prehome other medical issues negative events"
var_label(crisis_recode$crineighsafeneg) <- "Sum of prehome neighborhood safety negative events"
var_label(crisis_recode$crirelneg) <- "Sum of prehome relationship negative events"
var_label(crisis_recode$criprejneg) <- "Sum of prehome prejudice negative events"

var_label(crisis_recode$auth_nds) <- "Prehome authority negative domain score if ANY neg response"
var_label(crisis_recode$car_nds) <- "Prehome career negative domain score if ANY neg response"
var_label(crisis_recode$fin_nds) <- "Prehome financial negative domain score if ANY neg response"
var_label(crisis_recode$home_nds) <- "Prehome home negative domain score if ANY neg response"
var_label(crisis_recode$homesf_nds) <- "Prehome home safety negative domain score if ANY neg response"
var_label(crisis_recode$leg_nds) <- "Prehome legal negative domain score if ANY neg response"
var_label(crisis_recode$medself_nds) <- "Prehome self medical issues negative domain score if ANY neg response"
var_label(crisis_recode$medoth_nds) <- "Prehome other medical issues negative domain score if ANY neg response"
var_label(crisis_recode$neighsf_nds) <- "Prehome neighborhood safety negative domain score if ANY neg response"
var_label(crisis_recode$rel_nds) <- "Prehome relationship negative domain score if ANY neg response"
var_label(crisis_recode$prej_nds) <- "Prehome prejudice negative domain score if ANY neg response"
