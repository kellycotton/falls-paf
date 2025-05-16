# Falls PAR analysis
# 03/24/2025

# Setup ----
library(tidyverse)
library(gtsummary)
library(survey)
library(graphPAF)
library(detectseparation)
library(glm2)

# Read data
data_cat <- read_csv(here::here("data", "output_data", "falls_hrs_cat.csv")) %>% 
  select(-schlyrs, -subj_balance, 
         -inactive_mod, -balance_sum,
         -interview_type, -dementia_fu, -nurshm_2010)

# Look at data
skimr::skim(data_cat %>% 
              select(-id, -mwgtr))

data_cat %>% 
  filter(gait_speed_conv < 5) %>% 
  ggplot(aes(x=gait_speed_conv)) +
  geom_histogram()

# Clean data ----

# Numbers for exclusion sections
older <- data_cat %>% 
  filter(age >= 65) %>% # only people 65+
  # filter(interview_type == 2) %>% # only ftf interviews
  filter(eligible == 1) %>% # eligible for physical measures assessments, could be ineligible b/c nursing home or proxy interview,
                            # or because they were assigned to enhanced ftf interview in 2012
  filter(!is.na(fall_past_two)) %>% # answered falls question baseline
  filter(dementia == 0) # without dementia
length(older$id)

# number died at fu
older %>% 
  filter(alive_2012 == 5) %>% 
  summarize(n())

# number in nursing home at fu
older %>% 
  filter(nurshm_2012 == 1 | nurshm_2012 == 3) %>% 
  summarize(n())

# number dementia at fu
older %>% 
  filter(nurshm_2012 == 5) %>% # people not in nursing home
  filter(dementia_2012 == 1) %>% # but with dementia
  summarize(n())

# missing fu data
older %>% 
  filter(
    nurshm_2012 == 5 & # nursing home at fu 
      dementia_2012 == 0 & # dementia at fu
      alive_2012 == 1  # dead at fu
  ) %>% 
  summarize(n())

older %>% 
  filter(
    nurshm_2012 == 5 & # nursing home at fu 
      dementia_2012 == 0 & # dementia at fu
      alive_2012 == 1 &  # dead at fu
      is.na(incident_fall)  # missing fu fall info
  ) %>% 
  summarize(n())

# inaccurate walk times
older %>% 
  filter(
    nurshm_2012 == 5 & # nursing home at fu 
      dementia_2012 == 0 & # dementia at fu
      alive_2012 == 1 &  # dead at fu
      !is.na(incident_fall) & # missing fu fall info
      (gait_speed_conv >= 1.5 | # gait speeds > 150 cm/s 
      gait_speed_conv <=.1)
  ) %>% 
  summarize(n()) 

# n before dropping those with missing data
included_pts1 <- older %>% 
  filter(
    nurshm_2012 == 5 & # nursing home at fu 
      dementia_2012 == 0 & # dementia at fu
      alive_2012 == 1 &  # dead at fu
      !is.na(incident_fall) &  # missing fu fall info
      eligible == 1  # eligible for physical measures
  ) 
skimr::skim(included_pts1)
  
included_pts1 <- included_pts1 %>% 
  pull(id) 

# check to see which variables are missing data 
missing <- older %>% 
  filter(
    nurshm_2012 == 5 & # nursing home at fu 
      dementia_2012 == 0 & # dementia at fu
      alive_2012 == 1 & # dead at fu
      !is.na(incident_fall) &  # missing fu fall info
      (gait_speed_conv < 1.5 &
      gait_speed_conv > .1 |
      is.na(gait_speed_conv))
  ) %>% 
  select(-incident_fall_number, -incident_fall_injury, -fall_number, -fall_injury)

skimr::skim(missing)

# inaccurate/missing gait times
included_pts2 <- older %>% 
  filter(
      nurshm_2012 == 5 & # nursing home at fu 
      dementia_2012 == 0 & # dementia at fu
      alive_2012 == 1 & # dead at fu
      !is.na(incident_fall) &  # missing fu fall info
      !is.na(gait_time_mean) &
      gait_speed_conv < 1.5 &
      gait_speed_conv > .1
  ) %>% 
  pull(id)

length(included_pts1) - length(included_pts2)

# missing balance data
included_pts3 <- older %>% 
  filter(
    nurshm_2012 == 5 & # nursing home at fu 
      dementia_2012 == 0 & # dementia at fu
      alive_2012 == 1 & # dead at fu
      !is.na(incident_fall) &  # missing fu fall info
      !is.na(gait_time_mean) &
      gait_speed_conv < 1.5 &
      gait_speed_conv > .1 &
      !is.na(poor_balance)
  ) %>% 
  pull(id)

length(included_pts2) - length(included_pts3)

# remove those with missing data
included_pts <- older %>% 
  filter(
    nurshm_2012 == 5 & # nursing home at fu 
      dementia_2012 == 0 & # dementia at fu
      alive_2012 == 1 & # dead at fu
      !is.na(incident_fall) &  # missing fu fall info
      eligible == 1 & # eligible for physical measures
      !is.na(gait_time_mean) &
      gait_speed_conv < 1.5 &
      gait_speed_conv > .1 &
      !is.na(poor_balance)
  ) %>% 
  select(-fall_number, -fall_injury, -incident_fall_number, -incident_fall_injury) %>% 
  drop_na() %>% 
  pull(id)

length(included_pts3) - length(included_pts)

length(included_pts)

# Create cuts for cognition, gait, and grip strength based on pts that will be included
data_inc <- data_cat %>% 
  mutate(
    # new variable if pt should be excluded 
    included = case_when(
      id %in% included_pts ~ 1,
      !id %in% included_pts ~ 0
    )
  )

# Create survey design
design_cuts <- svydesign(id =~ id, strata =~ stratum, 
                         weights =~ mwgtr, data = data_inc)

# Subset based on exclusion criteria
data_sub_cuts <- subset(design_cuts, included == 1)
  
cog_cuts_mean <- svymean(~ tot_cog, data_sub_cuts)[1]
cog_cuts_sd <- sqrt(svyvar(~ tot_cog, data_sub_cuts)[1])
cog_cuts <- cog_cuts_mean - cog_cuts_sd

gait_cuts_mean <- svyby(~gait_speed_conv, ~age_group*gender, data_sub_cuts, svymean) %>% 
  rename(mean_gait = gait_speed_conv)
gait_cuts_sd <- svyby(~gait_speed_conv, ~age_group*gender, data_sub_cuts, svyvar) %>% 
  mutate(sd_gait= sqrt(gait_speed_conv))

gait_cuts <- gait_cuts_mean %>% 
  left_join(., gait_cuts_sd, by = c("age_group", "gender")) %>% 
  select(age_group, gender, mean_gait, sd_gait) %>% 
  mutate(slow_gait_cut = mean_gait - sd_gait)

grip_cuts_mean <- svyby(~grip_strength, ~age_group*gender, data_sub_cuts, svymean) %>% 
  rename(mean_grip = grip_strength)
grip_cuts_sd <- svyby(~grip_strength, ~age_group*gender, data_sub_cuts, svyvar) %>% 
  mutate(sd_grip = sqrt(grip_strength))

grip_cuts <- grip_cuts_mean %>% 
  left_join(., grip_cuts_sd, by = c("age_group", "gender")) %>% 
  select(age_group, gender, mean_grip, sd_grip) %>% 
  mutate(weak_grip_cut = mean_grip - sd_grip)

# Clean up data
data_clean <- data_cat %>%
  # only relevant columns
  select(id, mwgtr, stratum, age, age_group, gender, #schlyrs, 
         incident_fall,
         incident_fall_number, 
         incident_fall_injury,
         fall_past_two,
         low_vision,
         poor_hearing,
         diabetes,
         highbp,
         heart,
         stroke,
         arthritis,
         depression,
         pain,
         # subj_balance,
         poor_balance,
         # inactive_mod, 
         inactive_vig,
         tot_cog, gait_speed_conv, grip_strength, 
  ) %>% 
  # add cognition, gait, and grip cuts to main df
  left_join(., gait_cuts, by = c("age_group", "gender")) %>% 
  left_join(., grip_cuts, by = c("age_group", "gender")) %>% 
  mutate(cog_imp = case_when(
    tot_cog <= cog_cuts ~ 1,
    tot_cog > cog_cuts ~ 0,
    TRUE ~ tot_cog
  ),
  slow_gait = case_when(
    gait_speed_conv <= slow_gait_cut ~ 1,
    gait_speed_conv > slow_gait_cut ~ 0,
    TRUE ~ gait_speed_conv
  ),
  weak_strength = case_when(
    grip_strength < weak_grip_cut ~ 1,
    grip_strength >= weak_grip_cut ~ 0,
    TRUE ~ grip_strength
  ),
  # new variable if pt should be excluded 
  included = case_when(
    id %in% included_pts ~ 1,
    !id %in% included_pts ~ 0
  ))

# Make relevant variables factors
data_clean_factor <- data_clean %>% 
  mutate(across(
    c(
      fall_past_two,
      low_vision,
      poor_hearing,
      cog_imp,
      slow_gait,
      diabetes,
      highbp,
      heart,
      stroke,
      arthritis,
      depression,
      pain,
      weak_strength,
      # subj_balance,
      # inactive_mod, 
      inactive_vig,
      poor_balance
    ), 
    factor) 
  ) %>% 
  mutate(
    incident_fall_injury = case_when(
      incident_fall_injury == 1 ~ 1,
      incident_fall_injury == 0 ~ 0,
      incident_fall == 0 ~ 0,
      TRUE ~ incident_fall_injury
    ),
    multi_falls = case_when(
      incident_fall_number > 1 ~ 1,
      incident_fall_number == 1 ~ 0,
      incident_fall == 0 ~ 0,
      TRUE ~ NA
    ),
  ) %>% 
  mutate(gender = as.factor(case_when(
    gender == 1 ~ "Male", 
    gender == 2 ~ "Female"
  )))

# Create survey design
design <- svydesign(id =~ id, weights =~ mwgtr, strata =~ stratum,
                    data = data_clean_factor)

# Subset based on exclusion criteria
data_sub <- subset(design, included == 1)

# Demographics ----

# Unweighted demographics
data_no_excl <- data_clean_factor %>% 
  filter(included == 1) 

tbl_unwt <- tbl_summary(
  data = data_no_excl %>% mutate(across(where(is.factor), ~ recode(.x, "0" = "No", "1" = "Yes"))),
  by = incident_fall, 
  include = c(-id, -mwgtr, -stratum, -age_group, -slow_gait_cut, -mean_gait, 
              -incident_fall_number,
              -sd_gait, -mean_grip, -sd_grip, -weak_grip_cut, -included),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{n} ({p}%)"
                   ),
  digits = all_continuous() ~ 1,
  label = list(
    age = "Age, years, Mean (SD)",
    gender = "Women, %", 
    incident_fall_injury = "Injurious incident fall, %",
    multi_falls = "Mulitiple incident falls, %",
    cog_tot = "A-TICS, range 0-35, Mean (SD)",
    gait_speed_conv = "Gait velocity, m/s, Mean (SD)",
    grip_strength = "Grip strength, kg, Mean (SD)",
    fall_past_two = "Previous fall history",
    highbp = "Hypertension",
    diabetes = "Diabetes",
    heart = "Heart condition",
    stroke = "Stroke",
    arthritis = "Arthritis",
    depression = "Depression",
    cog_imp = "Cognitive impairment",
    slow_gait = "Slow gait",
    weak_strength = "Muscle weakness",
    low_vision = "Poor vision",
    poor_hearing = "Poor hearing",
    pain = "Pain",
    # subj_balance = "Subjective poor balance",
    inactive_mod = "Physical inactivity"
  )
  ) %>% 
  add_overall() %>% 
  add_p(test = list(all_continuous() ~ "t.test")) %>% 
  add_significance_stars() 

tbl_unwt

# export to Excel
# tbl_unwt %>%
  # as_hux_xlsx( here::here("data", "output_data", "demo_table_unweighted.xlsx"))

data_no_excl_nohx <- data_no_excl %>% 
  filter(fall_past_two == 0) 

tbl_unwt_nohx <- tbl_summary(
  data = data_no_excl_nohx %>% mutate(across(where(is.factor), ~ recode(.x, "0" = "No", "1" = "Yes"))),
  by = incident_fall, 
  include = c(-id, -mwgtr, -stratum, -age_group, -slow_gait_cut, -mean_gait, 
              -sd_gait, -mean_grip, -sd_grip, -weak_grip_cut, -included),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 1
) %>% 
  add_overall() %>% 
  add_p(test = list(all_continuous() ~ "t.test")) %>% 
  add_significance_stars() 

tbl_unwt_nohx

# Weighted demographics
tbl_wt <- tbl_svysummary(
  data_sub, 
  by = incident_fall, 
  include = c(-id, -mwgtr, -stratum, -age_group, -slow_gait_cut, -mean_gait, 
              -sd_gait, -mean_grip, -sd_grip, -weak_grip_cut, -included),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{p}%"),
  digits = list(
                all_continuous() ~ 1,
                gait_speed_conv ~ 3
  ),
  label = list(
    age = "Age, years, Mean (SD)",
    gender = "Women, %", 
    incident_fall_injury = "Injurious incident fall, %",
    multi_falls = "Mulitiple incident falls, %",
    cog_tot = "A-TICS, range 0-35, Mean (SD)",
    gait_speed_conv = "Gait velocity, m/s, Mean (SD)",
    grip_strength = "Grip strength, kg, Mean (SD)",
    fall_past_two = "Previous fall history",
    highbp = "Hypertension",
    diabetes = "Diabetes",
    heart = "Heart condition",
    stroke = "Stroke",
    arthritis = "Arthritis",
    depression = "Depression",
    cog_imp = "Cognitive impairment",
    slow_gait = "Slow gait",
    weak_strength = "Muscle weakness",
    low_vision = "Poor vision",
    poor_hearing = "Poor hearing",
    pain = "Pain",
    # subj_balance = "Subjective poor balance",
    inactive_mod = "Physical inactivity"
  )
) %>% 
  add_overall() %>% 
  add_p(test = list(all_continuous() ~ "svy.t.test"),
        pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
  add_significance_stars() 

tbl_wt

# export to Excel
# tbl_wt %>%
# as_hux_xlsx( here::here("data", "output_data", "demo_table_weighted.xlsx"))


# Weighted demographics by gender
tbl_wt_gender <- tbl_svysummary(
  data_sub, 
  by = gender, 
  include = c(-id, -mwgtr, -stratum, -age_group, -slow_gait_cut, -mean_gait, 
              -sd_gait, -mean_grip, -sd_grip, -weak_grip_cut, -included),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{p}%"),
  digits = list(
    all_continuous() ~ 1,
    gait_speed_conv ~ 3
  ),
  label = list(
    age = "Age, years, Mean (SD)",
    # gender = "Women, %", 
    incident_fall_injury = "Injurious incident fall, %",
    multi_falls = "Mulitiple incident falls, %",
    cog_tot = "A-TICS, range 0-35, Mean (SD)",
    gait_speed_conv = "Gait velocity, m/s, Mean (SD)",
    grip_strength = "Grip strength, kg, Mean (SD)",
    fall_past_two = "Previous fall history",
    highbp = "Hypertension",
    diabetes = "Diabetes",
    heart = "Heart condition",
    stroke = "Stroke",
    arthritis = "Arthritis",
    depression = "Depression",
    cog_imp = "Cognitive impairment",
    slow_gait = "Slow gait",
    weak_strength = "Muscle weakness",
    low_vision = "Poor vision",
    poor_hearing = "Poor hearing",
    pain = "Pain",
    # subj_balance = "Subjective poor balance",
    inactive_mod = "Physical inactivity"
  )
) %>% 
  add_overall() %>% 
  add_p(test = list(all_continuous() ~ "svy.t.test"),
        pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
  add_significance_stars() 

tbl_wt_gender

# export to Excel
# tbl_wt_gender %>%
  # as_hux_xlsx( here::here("data", "output_data", "demo_table_weighted_gender.xlsx"))

# Correlations ----
# Correlations between risk factors
design2 <- svydesign(id =~ id, weights =~ mwgtr, strata =~ stratum,
                     data = data_clean %>% 
                       select(id, mwgtr, stratum, included, 
                              age, gender, 
                              fall_past_two, low_vision, poor_hearing, diabetes, highbp, 
                              heart, stroke, arthritis, depression, pain, 
                              # subj_balance, 
                              # inactive_mod, 
                              inactive_vig, 
                              poor_balance, 
                              cog_imp, slow_gait, weak_strength) %>% 
                       mutate_if(is.factor, as.numeric)
                     )

# Subset based on exclusion criteria
data_sub2 <- subset(design2, included == 1)

data_cor <- as.matrix(data_clean %>% 
                        filter(included == 1) %>% 
                        select(
                               age, gender,
                               fall_past_two, 
                               low_vision, poor_hearing, diabetes, highbp, 
                               heart, stroke, arthritis, depression, pain, 
                               # subj_balance, 
                               # inactive_mod, 
                               inactive_vig,
                               poor_balance, 
                               cog_imp, slow_gait, weak_strength) %>% 
                        mutate_if(is.factor, as.numeric)
) 
v <- svyvar(data_cor, data_sub2)

df_cor <- as.data.frame(cov2cor(as.matrix(v)))

corrplot::corrplot(cov2cor(as.matrix(v)),
                   method = "color",
                   type = "lower",
                   diag = F,
                   addCoef.col = "black",
                   # number.cex=0.75
                   number.cex= 10/ncol(df_cor)
)

# Modeling ----
## Model 1 ----

### Interactions ----
# Full sample (of those who did gait assessment)
m_full_incident_interaction <- svyglm(incident_fall ~ 
                            gender*fall_past_two +
                            gender*low_vision + 
                            gender*poor_hearing +
                            gender*diabetes + 
                            gender*highbp + 
                            gender*heart +
                            gender*stroke +
                            gender*arthritis +
                            gender*depression + 
                            gender*pain +
                            gender*poor_balance +
                            gender*inactive_vig +
                            gender*cog_imp +
                            gender*slow_gait +
                            gender*weak_strength +
                            age*fall_past_two +
                            age*low_vision + 
                            age*poor_hearing +
                            age*diabetes + 
                            age*highbp + 
                            age*heart +
                            age*stroke +
                            age*arthritis +
                            age*depression + 
                            age*pain +
                            age*poor_balance +
                            age*inactive_vig +
                            age*cog_imp +
                            age*slow_gait +
                            age*weak_strength +
                             + age + gender 
                          # + schlyrs
                          , 
                          # family = quasipoisson(),
                          family = quasibinomial(),
                          design = data_sub)

tbl_results_1_interaction <- tbl_regression(
  m_full_incident_interaction,
  exponentiate = T,
  conf.int = T,
  estimate_fun = function(x) style_number(x, digits = 2),
  show_single_row = everything() # can comment out to see full model, easier to read with only 1 row
) %>% 
  add_significance_stars() %>% 
  modify_column_hide(column = std.error) %>%
  modify_column_unhide(column = conf.low) 

tbl_results_1_interaction

### Model ----
m_full_incident <- svyglm(incident_fall ~ 
                                        gender*fall_past_two +
                                        low_vision + 
                                        gender*poor_hearing +
                                        diabetes + 
                                        highbp + 
                                        heart +
                                        stroke +
                                        arthritis +
                                        depression + 
                                        pain +
                                        poor_balance +
                                        gender*inactive_vig +
                                        cog_imp +
                                        gender*slow_gait +
                                        weak_strength +
                                        + age + gender 
                                      , 
                                      # family = quasipoisson(),
                                      family = quasibinomial(),
                                      design = data_sub)

tbl_results_1 <- tbl_regression(
  m_full_incident,
  exponentiate = T,
  conf.int = T,
  estimate_fun = function(x) style_number(x, digits = 2),
  show_single_row = everything() # can comment out to see full model, easier to read with only 1 row
) %>% 
  add_significance_stars() %>% 
  modify_column_hide(column = std.error) %>%
  modify_column_unhide(column = conf.low) 

tbl_results_1

summary(m_full_incident)
# export to Excel
# tbl_results_1 %>%
  # as_hux_xlsx( here::here("data", "output_data", "tbl_results_1.xlsx"))

### Gender-stratified ----
data_sub_male <- subset(design, included == 1 & gender == "Male")
m_full_male <- svyglm(incident_fall ~ 
                            fall_past_two +
                            low_vision + 
                            poor_hearing +
                            diabetes + 
                            highbp + 
                            heart +
                            stroke +
                            arthritis +
                            depression + 
                            pain +
                            poor_balance +
                            inactive_vig +
                            cog_imp +
                            slow_gait +
                            weak_strength +
                            + age 
                          , 
                          # family = quasipoisson(),
                          family = quasibinomial(),
                          design = data_sub_male)

tbl_results_male <- tbl_regression(
  m_full_male,
  exponentiate = T,
  conf.int = T,
  estimate_fun = function(x) style_number(x, digits = 2),
  show_single_row = everything() # can comment out to see full model, easier to read with only 1 row
) %>% 
  add_significance_stars() %>% 
  modify_column_hide(column = std.error) %>%
  modify_column_unhide(column = conf.low) 

tbl_results_male

summary(m_full_male)

data_sub_female <- subset(design, included == 1 & gender == "Female")
m_full_female <- svyglm(incident_fall ~ 
                        fall_past_two +
                        low_vision + 
                        poor_hearing +
                        diabetes + 
                        highbp + 
                        heart +
                        stroke +
                        arthritis +
                        depression + 
                        pain +
                        poor_balance +
                        inactive_vig +
                        cog_imp +
                        slow_gait +
                        weak_strength +
                        + age 
                      , 
                      # family = quasipoisson(),
                      family = quasibinomial(),
                      design = data_sub_female)

tbl_results_female <- tbl_regression(
  m_full_female,
  exponentiate = T,
  conf.int = T,
  estimate_fun = function(x) style_number(x, digits = 2),
  show_single_row = everything() # can comment out to see full model, easier to read with only 1 row
) %>% 
  add_significance_stars() %>% 
  modify_column_hide(column = std.error) %>%
  modify_column_unhide(column = conf.low) 

tbl_results_female

summary(m_full_female)

## Model 2 ----
# data_sub_no_hx <- subset(design, included == 1 & fall_past_two == 0) # correct?

# Only those without prior fall history
# m_nofallhx_incident <- svyglm(incident_fall ~ 
#                                 low_vision + 
#                                 gender*poor_hearing +
#                                 diabetes + 
#                                 highbp + 
#                                 heart +
#                                 stroke +
#                                 arthritis +
#                                 depression + 
#                                 pain +
#                                 # subj_balance +
#                                 poor_balance +
#                                 inactive_vig +
#                                 cog_imp +
#                                 slow_gait +
#                                 weak_strength +
#                                 age + gender 
#                               , 
#                               family = quasipoisson(),
#                               design = data_sub_no_hx)
# 
# tbl_results_2 <- tbl_regression(
#   m_nofallhx_incident,
#   exponentiate = T,
#   conf.int = T,
#   estimate_fun = function(x) style_number(x, digits = 2),
#   show_single_row = everything()
# ) %>% 
#   add_significance_stars() %>% 
#   modify_column_hide(column = std.error) %>%
#   modify_column_unhide(column = ci)
# 
# tbl_results_2

# export to Excel
# tbl_results_2 %>%
  # as_hux_xlsx( here::here("data", "output_data", "tbl_results_2.xlsx"))

## Model 3 ----
# data_sub_inj <- subset(design, included == 1 & 
                         # incident_fall == 1) # correct?

# Only those without prior fall history
# m_fallinj <- svyglm(incident_fall_injury ~ 
#                                 fall_past_two +
#                                 low_vision + 
#                                 poor_hearing +
#                                 diabetes + 
#                                 highbp + 
#                                 heart +
#                                 stroke +
#                                 arthritis +
#                                 depression + 
#                                 pain +
#                                 poor_balance +
#                                 inactive_vig +
#                                 # pain_meds +
#                                 cog_imp +
#                                 slow_gait +
#                                 weak_strength +
#                                 age + gender 
#                     # + schlyrs
#                               , 
#                               family = quasipoisson(),
#                               design = data_sub_inj)
# 
# tbl_results_3 <- tbl_regression(
#   m_fallinj,
#   exponentiate = T,
#   conf.int = T,
#   estimate_fun = function(x) style_number(x, digits = 2),
#   show_single_row = everything()
# ) %>% 
#   add_significance_stars() %>% 
#   modify_column_hide(column = std.error) %>%
#   modify_column_unhide(column = ci)
# 
# tbl_results_3

# export to Excel
# tbl_results_2 %>%
# as_hux_xlsx( here::here("data", "output_data", "tbl_results_3.xlsx"))

# PAF calculation ----
##### Miettinen's formula ----
#function to calculate PAF
get_miettinen_paf <- function(prev,arr){
  return(round(prev*(arr-1)/arr,4))
}

get_paf_ci = function(prev_ci_lower,
                      prev_ci_upper,
                      arr_ci_lower,
                      arr_ci_upper){
  paf_ci_lower = round(prev_ci_lower*(arr_ci_lower - 1)/arr_ci_lower,4)
  paf_ci_upper = round(prev_ci_upper*(arr_ci_upper - 1)/arr_ci_upper,4)
  
  return(paste("(", as.character(paf_ci_lower),",",as.character(paf_ci_upper),")"))
}


#get adjusted relative risk

## m1 ----
arr_tb = broom::tidy(m_full_incident,exponentiate = TRUE) %>% 
  dplyr::select(term,estimate) %>% 
  filter(!(term %in% c("age","genderMale","(Intercept)", 
                       "genderMale:fall_past_two1", "genderMale:poor_hearing1", "genderMale:slow_gait1"))) %>% 
  mutate(term = str_replace_all(term,"[01]",""))

#97.5 percent CIs for RR
arr_ci_tb = broom::tidy(m_full_incident) %>% 
  dplyr::select(term,estimate,std.error) %>% 
  filter(!(term %in% c("age","genderMale","(Intercept)"))) %>% 
  mutate(term = str_replace_all(term,"[01]","")) %>% 
  mutate(arr_ci_lower = exp(estimate - 2.24*std.error),
         arr_ci_upper = exp(estimate + 2.24*std.error)) %>% 
  dplyr::select(term,arr_ci_lower,arr_ci_upper)

arr_tb = arr_tb %>% 
  left_join(arr_ci_tb,
            by = "term")

fct_lst = arr_tb$term

#get prevalence of the modifiable risk factor among those who fell


#new design: only contain eligible subjects who fell
data_sub_fall <- subset(design, included == 1&incident_fall==1)


prev_tb = tibble()
po = list()

for (i in seq(length(fct_lst))){
  if (fct_lst[i] %in% c("diabetes","cog_imp","slow_gait","weak_strength")){
    po[i] = "0"
  }
  else{
    po[i] = "1"
  }
}

for (i in seq(length(fct_lst))){
  prev = as_tibble(svymean( ~eval(as.name(fct_lst[i])), data_sub_fall),
                   rownames = "level") %>% 
    filter(str_detect(level,as.character(po[i]))) %>% 
    pull(mean)
  
  prev_se = as_tibble(svymean( ~eval(as.name(fct_lst[i])), data_sub_fall),
                      rownames = "level") %>% 
    filter(str_detect(level,as.character(po[i]))) %>% 
    pull(SE)
  
  prev_tb = prev_tb %>% 
    bind_rows(tibble(term=fct_lst[i],
                     prev=prev,
                     prev_se = prev_se,
                     prev_ci_lower = prev - 2.24*prev_se,
                     prev_ci_upper = prev + 2.24*prev_se))
}


prev_arr_tb = prev_tb %>% 
  left_join(arr_tb, by = "term") %>% 
  rename(arr = estimate) %>% 
  mutate(miettinen_paf = get_miettinen_paf(prev,arr),
         miettinen_paf_ci = get_paf_ci(prev_ci_lower,
                                       prev_ci_upper,
                                       arr_ci_lower,
                                       arr_ci_upper))


write_csv(prev_arr_tb,here::here("data", "output_data", "miettinen_paf_results_m1.csv"))

## m2 ----
arr_tb = broom::tidy(m_nofallhx_incident,exponentiate = TRUE) %>% 
  dplyr::select(term,estimate) %>% 
  filter(!(term %in% c("age","genderMale","(Intercept)", 
                       "genderMale:poor_hearing1"))) %>% 
  mutate(term = str_replace_all(term,"[01]",""))

#97.5 percent CIs for RR
arr_ci_tb = broom::tidy(m_nofallhx_incident) %>% 
  dplyr::select(term,estimate,std.error) %>% 
  filter(!(term %in% c("age","genderMale","(Intercept)"))) %>% 
  mutate(term = str_replace_all(term,"[01]","")) %>% 
  mutate(arr_ci_lower = exp(estimate - 2.24*std.error),
         arr_ci_upper = exp(estimate + 2.24*std.error)) %>% 
  dplyr::select(term,arr_ci_lower,arr_ci_upper)

arr_tb = arr_tb %>% 
  left_join(arr_ci_tb,
            by = "term")

fct_lst = arr_tb$term

#get prevalence of the modifiable risk factor among those who fell


#new design: only contain eligible subjects who fell
data_sub_fall <- subset(design, included == 1&incident_fall==1)


prev_tb = tibble()
po = list()

for (i in seq(length(fct_lst))){
  if (fct_lst[i] %in% c("inactive_mod","cog_imp","slow_gait","weak_strength")){
    po[i] = "0"
  }
  else{
    po[i] = "1"
  }
}

for (i in seq(length(fct_lst))){
  prev = as_tibble(svymean( ~eval(as.name(fct_lst[i])), data_sub_fall),
                   rownames = "level") %>% 
    filter(str_detect(level,as.character(po[i]))) %>% 
    pull(mean)
  
  prev_se = as_tibble(svymean( ~eval(as.name(fct_lst[i])), data_sub_fall),
                      rownames = "level") %>% 
    filter(str_detect(level,as.character(po[i]))) %>% 
    pull(SE)
  
  prev_tb = prev_tb %>% 
    bind_rows(tibble(term=fct_lst[i],
                     prev=prev,
                     prev_se = prev_se,
                     prev_ci_lower = prev - 2.24*prev_se,
                     prev_ci_upper = prev + 2.24*prev_se))
}


prev_arr_tb = prev_tb %>% 
  left_join(arr_tb, by = "term") %>% 
  rename(arr = estimate) %>% 
  mutate(miettinen_paf = get_miettinen_paf(prev,arr),
         miettinen_paf_ci = get_paf_ci(prev_ci_lower,
                                       prev_ci_upper,
                                       arr_ci_lower,
                                       arr_ci_upper))


write_csv(prev_arr_tb,here::here("data", "output_data", "miettinen_paf_results_m2.csv"))


# Alternative PAF calc ----
# function to run paf calculations

run_paf <- function(model, riskfactor, refval, data){
  PAF_calc_discrete(model=model, riskfactor=riskfactor,
                    refval = refval, data = data,
                    ci = T,
                    calculation_method = "D", weight_vec=data$mwgtr,
                    boot_rep = 200
  )
}

pafs <-  data.frame(matrix(ncol = 5, nrow = 0))
colnames(pafs) <- c("model", "riskfactor", "estimate", "ci")

# Recode data
paf_data = data_no_excl %>% 
  mutate(across(where(is.factor), ~ recode(.x, "0" = "No", "1" = "Yes")))

## Full, any fall----
m2_full_incident <- glm2(formula = incident_fall ~ 
                          fall_past_two +
                          gender*fall_past_two +
                          low_vision + 
                          gender*poor_hearing +
                          diabetes + 
                          highbp + 
                          heart +
                          stroke +
                          arthritis +
                          depression + 
                          pain +
                          poor_balance + 
                          gender*inactive_vig +
                          cog_imp +
                          gender*slow_gait +
                          weak_strength 
                        + age + gender
                        , 
                        # famil = quasipoisson(),
                        family = quasibinomial(),
                        weights = mwgtr,
                        data = paf_data
)

m2_full_incident_results <- broom::tidy(m2_full_incident) %>% 
  mutate(
    refval = ifelse(estimate < 0, "Yes", "No")
  ) %>% 
  select(term, refval) %>% 
  filter(!term %in% c("(Intercept)", "age", "genderMale", 
                      "fall_past_twoYes:genderMale",
                      "genderMale:poor_hearingYes",
                      "genderMale:inactive_vigYes",
                      "genderMale:slow_gaitYes"
                      )) %>% 
  mutate(
    term = gsub('.{3}$', '', term)
  )

m_sep <- glm2(formula = incident_fall ~ 
               fall_past_two +
               gender*fall_past_two +
               low_vision + 
               gender*poor_hearing +
               diabetes + 
               highbp + 
               heart +
               stroke +
               arthritis +
               depression + 
               pain +
               poor_balance + 
               gender*inactive_vig +
               cog_imp +
               gender*slow_gait +
               weak_strength 
             + age + gender
             , 
             family = "quasibinomial",
             method = "detect_separation",
             weights = mwgtr,
             data = paf_data
)
m_sep

for (i in 1:length(m2_full_incident_results$term)){
  p <- run_paf(m2_full_incident, m2_full_incident_results$term[i], m2_full_incident_results$refval[i], paf_data)
  result = data.frame(model ="m2_full_incident", riskfactor = m2_full_incident_results$term[i], 
                      estimate = p$estimate, ci = p$confidence_interval,
                      estimate_perc = p$estimate*100)
  pafs <- rbind(pafs, result)
  
}

## No fall hx, any fall----
# paf_data_nohx = data_no_excl %>% 
#   filter(fall_past_two == 0) %>% 
#   mutate(across(where(is.factor), ~ recode(.x, "0" = "No", "1" = "Yes")))
# 
# m2_nofallhx_incident  <- glm(formula = incident_fall ~ 
#                                low_vision + 
#                                poor_hearing +
#                                diabetes + 
#                                highbp + 
#                                heart +
#                                stroke +
#                                arthritis +
#                                depression + 
#                                pain +
#                                subj_balance +
#                                inactive_mod +
#                                # pain_meds +
#                                cog_imp +
#                                slow_gait +
#                                weak_strength +
#                                age + gender 
#                              # + schlyrs
#                              , 
#                              family = "quasipoisson",
#                              weights = mwgtr,
#                              data = paf_data_nohx
# )
# 
# m2_nofallhx_incident_results <- broom::tidy(m2_nofallhx_incident) %>% 
#   mutate(
#     refval = ifelse(estimate < 0, "Yes", "No")
#   ) %>% 
#   select(term, refval) %>% 
#   filter(!term %in% c("(Intercept)", "age", "genderMale", "schlyrs")) %>% 
#   mutate(
#     term = gsub('.{3}$', '', term)
#   )
# 
# for (i in 1:length(m2_nofallhx_incident_results$term)){
#   p <- run_paf(m2_nofallhx_incident, m2_nofallhx_incident_results$term[i], 
#                m2_nofallhx_incident_results$refval[i], paf_data_nohx)
#   result = data.frame(model ="m2_nofallhx_incident", riskfactor = m2_nofallhx_incident_results$term[i], 
#                       estimate = p$estimate, ci = p$confidence_interval,
#                       estimate_perc = p$estimate*100)
#   pafs <- rbind(pafs, result)
#   
# }
# 
 ## Injurious falls----
# paf_data_injfall = data_no_excl %>% 
#   filter(incident_fall == 1) %>% 
#   mutate(across(where(is.factor), ~ recode(.x, "0" = "No", "1" = "Yes")))
# 
# m3_fallinj  <- glm(formula = incident_fall_injury ~
#                               fall_past_two + 
#                                low_vision + 
#                                poor_hearing +
#                                diabetes + 
#                                highbp + 
#                                heart +
#                                stroke +
#                                arthritis +
#                                depression + 
#                                pain +
#                                subj_balance +
#                                inactive_mod +
#                                # pain_meds +
#                                cog_imp +
#                                slow_gait +
#                                weak_strength +
#                                age + gender 
#                    # + schlyrs
#                              , 
#                              family = "quasipoisson",
#                              weights = mwgtr,
#                              data = paf_data_injfall
# )
# 
# m3_fallinj_results <- broom::tidy(m3_fallinj) %>% 
#   mutate(
#     refval = ifelse(estimate < 0, "Yes", "No")
#   ) %>% 
#   select(term, refval) %>% 
#   filter(!term %in% c("(Intercept)", "age", "genderMale", "schlyrs")) %>% 
#   mutate(
#     term = gsub('.{3}$', '', term)
#   )
# 
# for (i in 1:length(m3_fallinj_results$term)){
#   p <- run_paf(m3_fallinj, m3_fallinj_results$term[i], 
#                m3_fallinj_results$refval[i], paf_data_injfall)
#   result = data.frame(model ="m3_fallinj", riskfactor = m3_fallinj_results$term[i], 
#                       estimate = p$estimate, ci = p$confidence_interval,
#                       estimate_perc = p$estimate*100)
#   pafs <- rbind(pafs, result)
#   
# }

pafs 
# write_csv(pafs, here::here("data", "output_data", "paf_results.csv"))

## Overall PAF----

# Full data
# only main effect significant
data_full_any <- paf_data %>% 
  mutate(overall_risk = as.factor(case_when(
    fall_past_two == "Yes" | 
      heart == "Yes" | 
      arthritis == "Yes" |
      pain == "Yes" | 
      poor_balance == "Yes"
    ~ 1,
    TRUE ~ 0
  ))
  )

m3_full_incident <- glm(formula = incident_fall ~ 
                          overall_risk +
                          age + gender 
                        , 
                        family = "quasipoisson",
                        weights = mwgtr,
                        data = data_full_any
)

tbl_regression(
  m3_full_incident,
  exponentiate = T,
  conf.int = T,
  # show_single_row = everything()
) %>% 
  modify_column_hide(column = c(std.error, p.value)) #  estimates are the same as above, CIs are different

PAF_calc_discrete(model=m3_full_incident, riskfactor="overall_risk",
                  refval = 0, data = data_full_any,
                  ci = T,
                  calculation_method = "D", weight_vec=data_full_any$mwgtr,
                  boot_rep = 200
)

# with interation effect
data_full_any_int <- paf_data %>% 
  mutate(overall_risk = as.factor(case_when(
    fall_past_two == "Yes" | 
      heart == "Yes" | 
      arthritis == "Yes" |
      pain == "Yes" | 
      poor_balance == "Yes" |
      poor_hearing == "Yes" |
      inactive_vig == "No" # ? b/c main effect < 1
    ~ 1,
    TRUE ~ 0
  ))
  )

m4_full_incident <- glm(formula = incident_fall ~ 
                          overall_risk +
                          age + gender 
                        , 
                        family = "quasipoisson",
                        weights = mwgtr,
                        data = data_full_any_int
)

tbl_regression(
  m4_full_incident,
  exponentiate = T,
  conf.int = T,
  # show_single_row = everything()
) %>% 
  modify_column_hide(column = c(std.error, p.value)) #  estimates are the same as above, CIs are different

PAF_calc_discrete(model=m4_full_incident, riskfactor="overall_risk",
                  refval = 0, data = data_full_any,
                  ci = T,
                  calculation_method = "D", weight_vec=data_full_any$mwgtr,
                  boot_rep = 200
)

# No fall hx
# data_nohx_any <- paf_data_nohx %>% 
#   mutate(overall_risk = as.factor(case_when(
#     poor_hearing == "Yes" | 
#       stroke == "Yes"  |
#       pain == "Yes"
#     ~ 1,
#     TRUE ~ 0
#   ))
#   )
# 
# m3_nofallhx_incident <- glm(formula = incident_fall ~ 
#                           overall_risk +
#                           age + gender 
#                           # + schlyrs
#                         , 
#                         family = "quasipoisson",
#                         weights = mwgtr,
#                         data = data_nohx_any
# )
# 
# tbl_regression(
#   m3_nofallhx_incident,
#   exponentiate = T,
#   conf.int = T,
#   # show_single_row = everything()
# ) %>% 
#   modify_column_hide(column = c(std.error, p.value)) #  estimates are the same as above, CIs are different
# 
# PAF_calc_discrete(model=m3_nofallhx_incident, riskfactor="overall_risk",
#                   refval = 0, data = data_nohx_any,
#                   ci = T,
#                   calculation_method = "D", weight_vec=data_nohx_any$mwgtr,
#                   boot_rep = 200
# )
# 
# # rfs <- rf_summary(
# #   rf_names = c("fall_past_two","low_vision", 
# #                                "poor_hearing",
# #                                "diabetes", 
# #                                "highbp", 
# #                                "heart",
# #                                "stroke",
# #                                "arthritis",
# #                                "depression", 
# #                                "pain",
# #                                "subj_balance",
# #                                "inactive_mod",
# #                                "pain_meds",
# #                                "cog_imp",
# #                                "slow_gait",
# #                                "weak_strength"),
# #   rf_prev = c(.35, .19, .21, .22, .65, .29, .069, .7, .092, 
# #               .36, .33, .32, .23, .12, .13, .14),
# #   risk = c(2.17, 1.07, 1.2, 1, 1.07, 1.16, 1.15, 1.16, 1.12, 
# #            1.11, 1.17, .99, 1.04, .94, .96, .99),
# #   log = T
# # )

