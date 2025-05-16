# Falls PAR cleaning
# 03/24/2025

# Setup---
library(tidyverse)

# Read data
data_2010 <- haven::read_sav(here::here("data", "hrs", "data_2010.sav")) %>% janitor::clean_names(.)
data_2012 <- haven::read_sav(here::here("data", "hrs", "data_2012.sav")) %>% janitor::clean_names(.)
data_cog <- haven::read_sav(here::here("data", "hrs", "cog.sav")) %>% janitor::clean_names(.) %>% 
  select(hhid, pn, r10tr20, r10mstot, r10cogtot, r10cog27) %>% 
  mutate(hhidpn = as.numeric(paste0(hhid, pn)))
data_track <- haven::read_sav(here::here("data", "hrs", "track.sav")) %>% janitor::clean_names(.) %>% 
  select(hhid, pn, # id
         gender, schlyrs, race, 
         mage, mnurshm, # 2010 age, nursing home status
         nalive, nnurshm, # 2012 status, nursing home status
         mwgtr, # survey weights
         stratum # strata
  ) %>% 
  mutate(hhidpn = as.numeric(paste0(hhid, pn)))
data_harm <- haven::read_sav(here::here("data", "hrs", "harmonized_data.sav")) %>% janitor::clean_names(.) 

# get a few variables from harmonized dataset
data_harm_clean <- data_harm %>% 
  select(hhidpn, 
         r10gripsum, #grip strength
         r10rxpain, # meds: pain
         r10balance, #balance test summary score
         
  ) %>% 
  rename(
    id = hhidpn,
    grip_strength = r10gripsum, 
    pain_meds = r10rxpain,
    balance_sum = r10balance, 
  ) %>% 
  mutate(
    poor_balance = case_when(
      balance_sum < 3 ~ 1,
      balance_sum >= 3 ~ 0,
      TRUE ~ balance_sum
    )
  )

# follow-up data
data_2012_clean <- data_2012 %>% 
  select(hhidpn,
         nc272, nc273, # dementia and AD
         nc079, nc080, nc081, # fall past 2 years, fall number, fall injury
  ) %>% 
  #rename columns
  rename(
    id = hhidpn,
    fall_past_two_1 = nc079,
    fall_number_1 = nc080,
    fall_injury_1 = nc081,
    alz_dis_fu = nc272,
    dementia_fu = nc273,
  ) %>% 
  mutate(
    incident_fall = case_when(
      fall_past_two_1 == 1 ~ 1,
      fall_past_two_1 == 5 ~ 0,
      fall_past_two_1 == 8 ~ NA,
      fall_past_two_1 == 9 ~ NA,
      TRUE ~ fall_past_two_1
    ),
    # only those who have fallen are coded 1 or 0, no falls = NA
    incident_fall_number = case_when(
      fall_number_1 == 98 ~ NA,
      fall_number_1 == 99 ~ NA,
      TRUE ~ fall_number_1
    ),
    incident_fall_injury = case_when(
      fall_injury_1 == 1 ~ 1,
      fall_injury_1 == 5 ~ 0,
      fall_injury_1 == 8 ~ NA,
      fall_injury_1 == 9 ~ NA,
      TRUE ~ fall_injury_1
    ),
    dementia_2012 = case_when(
      alz_dis_fu == 1 ~ 1,
      alz_dis_fu == 3 ~ 1,
      dementia_fu == 1 ~ 1,
      dementia_fu == 3 ~ 1,
      dementia_fu == 4 ~ 0,
      dementia_fu == 5 ~ 0,
      alz_dis_fu == 4 ~ 0,
      alz_dis_fu == 5 ~ 0,
      alz_dis_fu == 7 & dementia_fu == 1 ~ 1,
      alz_dis_fu == 7 & dementia_fu == 0 ~ 0,
      alz_dis_fu > 7 & dementia_fu > 7 ~ NA,
      TRUE ~ NA
    ),
  ) %>% 
  select(-fall_past_two_1, -fall_number_1, -fall_injury_1) 

data_combo <- data_2010 %>%
  left_join(., data_cog) %>% 
  left_join(., data_track) %>% 
  #select relevant baseline variables
  select(
    hhidpn, ma028, mc272, mc273, # id, nursing home, AD, dementia
    mi800, mpmelig, # interview type, eligibility for physical assessments
    gender, schlyrs, race, 
    mage, mnurshm, # 2010 age, nursing home status
    nalive, nnurshm, # 2012 status, nursing home status
    mwgtr, stratum, # survey weights, stratum
    ma019, mx060_r, mz216,  # age, sex, educ
    mc079, mc080, mc081, # fall past 2, fall number, fall injury
    mc095, mc103, # self-rated vision, self-rated hearing
    mc005, mc010, mc036, mc053, mc070, # high BP, diabetes, heart condition, stroke, arthritis, 
    mc104, #troubled with pain
    mi823, mi824, # walking test time in seconds
    mi820, mi821m1, # walking test complete, reason why not
    mi883, contains("mi884"), mi886, mi887, mi888, #side by side stand
    mi876, contains("mi877"), mi879, mi880, mi881, # semitandem stand
    mi893, contains("mi894"), mi896, mi897, mi898, # full tandem stand
    md110, md111, md112, md113, md114, md115, md116, md117, # ces-d questions
    mg210, # subjective balance
    mc223, mc224, # vigorous, moderate activity
    r10cogtot, # total cog
  ) %>%
  # score CES-D scale
  mutate(
    md110 = case_when(
      md110 == 1 ~ 1,
      md110 == 5 ~ 0,
      TRUE ~ NA
    ),
    md111 = case_when(
      md111 == 1 ~ 1,
      md111 == 5 ~ 0,
      TRUE ~ NA
    ),
    md112 = case_when(
      md112 == 1 ~ 1,
      md112 == 5 ~ 0,
      TRUE ~ NA
    ),
    # reverse-scored
    md113 = case_when(
      md113 == 1 ~ 0,
      md113 == 5 ~ 1,
      TRUE ~ NA
    ),
    md114 = case_when(
      md114 == 1 ~ 1,
      md114 == 5 ~ 0,
      TRUE ~ NA
    ),
    # reverse-scored
    md115 = case_when(
      md115 == 1 ~ 0,
      md115 == 5 ~ 1,
      TRUE ~ NA
    ),
    md116 = case_when(
      md116 == 1 ~ 1,
      md116 == 5 ~ 0,
      TRUE ~ NA
    ),
    md117 = case_when(
      md117 == 1 ~ 1,
      md117 == 5 ~ 0,
      TRUE ~ NA
    ),
    depress_sum = rowSums(across(c(md110, md111, md112, md113, md114, md115, md116, md117)))
  ) %>% 
  #rename columns
  rename(
    id = hhidpn,
    eligible = mpmelig,
    sex_1 = mx060_r,
    age = mage, 
    nurshm_2010 = mnurshm, # nursing home status
    alive_2012 = nalive, #2012 status,
    nurshm_2012 = nnurshm, #  nursing home status
    interview_type = mi800, #ftf or phone
    fall_past_two_1 = mc079,
    fall_number_1 = mc080,
    fall_injury_1 = mc081,
    alz_dis = mc272,
    dementia = mc273,
    vision = mc095,
    hearing = mc103,
    highbp = mc005, 
    diabetes = mc010, 
    heart = mc036, 
    stroke = mc053, 
    arthritis = mc070, 
    pain = mc104,
    gait_1 = mi823, 
    gait_2 = mi824,
    subj_balance = mg210,
    mod_act = mc224,
    vig_act = mc223,
    tot_cog = r10cogtot,
    sbs_complete = mi883,
    semi_complete = mi876,
    full_complete = mi893,
    sbs_no_why1 = mi884m1,
    sbs_no_why2 = mi884m2,
    sbs_no_why3 = mi884m3,
    sbs_no_why4 = mi884m4,
    sbs_no_why5 = mi884m5,
    semi_no_why1 = mi877m1,
    semi_no_why2 = mi877m2, 
    semi_no_why3 = mi877m3, 
    semi_no_why4 = mi877m4, 
    semi_no_why5 = mi877m5, 
    full_no_why1 = mi894m1,
    full_no_why2 = mi894m2,
    full_no_why3 = mi894m3,
    full_no_why4 = mi894m4,
    full_no_why5 = mi894m5,
    sbs_time_full = mi886,
    sbs_time = mi887,
    semi_time_full = mi879,
    semi_time = mi880,
    full_time_full = mi896,
    full_time = mi897
  ) %>%
  mutate(
    sex = case_when(
      sex_1 == 1 ~ "male",
      sex_1 == 2 ~ "female"
    ),
    fall_past_two = case_when(
      fall_past_two_1 == 1 ~ 1,
      fall_past_two_1 == 5 ~ 0,
      fall_past_two_1 == 8 ~ NA,
      fall_past_two_1 == 9 ~ NA,
      TRUE ~ fall_past_two_1
    ),
    # only those who have fallen are coded 1 or 0, no falls = NA
    fall_number = case_when(
      fall_number_1 == 98 ~ NA,
      fall_number_1 == 99 ~ NA,
      TRUE ~ fall_number_1
    ),
    fall_injury = case_when(
      fall_injury_1 == 1 ~ 1,
      fall_injury_1 == 5 ~ 0,
      fall_injury_1 == 8 ~ NA,
      fall_injury_1 == 9 ~ NA,
      TRUE ~ fall_injury_1
    ),
    vision = case_when(
      vision == 8 ~ NA,
      vision == 9 ~ NA,
      TRUE ~ vision
    ),
    hearing = case_when(
      hearing == 8 ~ NA,
      hearing == 9 ~ NA,
      TRUE ~ hearing,
    ),
    highbp = case_when(
      highbp == 1 ~ 1,
      highbp == 3 ~ 1,
      highbp == 4 ~ 0,
      highbp == 5 ~ 0,
      highbp == 8 ~ NA,
      highbp == 9 ~ NA,
      TRUE ~ highbp,
    ), 
    diabetes = case_when(
      diabetes == 1 ~ 1,
      diabetes == 3 ~ 1,
      diabetes == 4 ~ 0,
      diabetes == 5 ~ 0,
      diabetes == 8 ~ NA,
      diabetes == 9 ~ NA,
      TRUE ~ diabetes,
    ), 
    heart = case_when(
      heart == 1 ~ 1,
      heart == 3 ~ 1,
      heart == 4 ~ 0,
      heart == 5 ~ 0,
      heart == 8 ~ NA,
      heart == 9 ~ NA,
      TRUE ~ heart,
    ), 
    stroke = case_when(
      stroke == 1 ~ 1,
      stroke == 2 ~ 1,
      stroke == 3 ~ 1,
      stroke == 4 ~ 0,
      stroke == 5 ~ 0,
      stroke == 8 ~ NA,
      stroke == 9 ~ NA,
      TRUE ~ stroke,
    ), 
    arthritis = case_when(
      arthritis == 1 ~ 1,
      arthritis == 3 ~ 1,
      arthritis == 4 ~ 0,
      arthritis == 5 ~ 0,
      arthritis == 8 ~ NA,
      arthritis == 9 ~ NA,
      TRUE ~ arthritis,
    ), 
    depression = case_when(
      depress_sum >= 4 ~ 1,
      depress_sum < 4 ~ 0,
      TRUE ~ depress_sum
    ),
    pain = case_when(
      pain == 1 ~ 1,
      pain == 5 ~ 0,
      pain == 8 ~ NA,
      pain == 9 ~ NA,
      TRUE ~ pain,
    ),
    subj_balance = case_when(
      subj_balance == 1 ~ 1, #1 = often, 2 = sometimes, 3 = rarely, 4 = never
      subj_balance == 2 ~ 1,
      subj_balance == 3 ~ 0,
      subj_balance == 4 ~ 0,
      subj_balance == 8 ~ NA,
      subj_balance == 9 ~ NA,
      TRUE ~ subj_balance,
    ),
    inactive_vig = case_when(
      vig_act == 1 ~ 0,
      vig_act == 2 ~ 0,
      vig_act == 7 ~ 0,
      vig_act == 3 ~ 1,
      vig_act == 4 ~ 1,
      vig_act == 8 ~ NA,
      vig_act == 9 ~ NA,
      TRUE ~ vig_act,
    ),
    inactive_mod = case_when(
      mod_act == 1 ~ 0,
      mod_act == 2 ~ 0,
      mod_act == 7 ~ 0,
      mod_act == 3 ~ 1,
      mod_act == 4 ~ 1,
      mod_act == 8 ~ NA,
      mod_act == 9 ~ NA,
      TRUE ~ mod_act,
    ),
    gait_1 = case_when(
      gait_1 > 990 ~ NA,
      TRUE ~ gait_1,
    ), 
    gait_2 = case_when(
      gait_2 > 990 ~ NA,
      TRUE ~ gait_2,
    ),
    age_group = case_when(
      age < 70 ~ "<70",
      age >= 80 ~ "80+",
      age >= 70 & age < 80 ~ "70-79",
      is.na(age) ~ NA
    ),
  ) %>% 
  mutate(
    gait_time_mean = (gait_1+gait_2)/2,
    gait_speed_conv = 2.5019/gait_time_mean
  ) %>% 
  left_join(., data_2012_clean) %>% 
  left_join(., data_harm_clean) %>% 
  mutate(
    poor_balance = case_when(
      balance_sum < 3 ~ 1,
      balance_sum >= 3 ~ 0,
      sbs_complete == 5 & sbs_no_why1 %in% c(1, 2, 4) ~ 1,
      semi_complete == 5 & semi_no_why1 %in% c(1, 2, 4) ~ 1,
      full_complete == 5 & full_no_why1 %in% c(1, 2, 4) ~ 1,
      TRUE ~ balance_sum
    )
  ) 

data_cat <- data_combo %>% 
  select(id, alz_dis, dementia, age, gender, schlyrs, race, age_group,
         nurshm_2010, interview_type, eligible,
         nurshm_2012, alive_2012, dementia_2012, dementia_fu, alz_dis_fu,
         mwgtr, stratum,
         fall_past_two, fall_number, fall_injury, 
         vision, hearing, 
         highbp, diabetes, heart, stroke, arthritis, 
         pain,
         depression,
         tot_cog, 
         gait_speed_conv, gait_time_mean, 
         incident_fall, incident_fall_number, incident_fall_injury,
         subj_balance,
         balance_sum,
         inactive_vig, inactive_mod,
         grip_strength, 
         poor_balance
  ) %>% 
  mutate(
    dementia = case_when(
      alz_dis == 1 ~ 1,
      dementia == 1 ~ 1,
      dementia == 5 ~ 0,
      alz_dis == 5 ~ 0,
      alz_dis == 7 & dementia == 1 ~ 1,
      alz_dis == 7 & dementia == 0 ~ 0,
      alz_dis > 7 & dementia > 7 ~ NA,
      TRUE ~ NA
    ),
    low_vision = case_when(
      vision < 4 ~ 0,
      vision >= 4 ~ 1,
      TRUE ~ vision
    ),
    poor_hearing = case_when(
      hearing < 4 ~ 0,
      hearing >= 4 ~ 1,
      TRUE ~ hearing 
    ),
  ) 


# write_csv(data_combo, here::here("data", "output_data", "falls_hrs_full.csv"))
write_csv(data_cat, here::here("data", "output_data", "falls_hrs_cat.csv"))

