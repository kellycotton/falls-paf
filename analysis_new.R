# Falls PAR analysis
# 03/24/2025

# Setup ----
library(tidyverse)
library(gtsummary)
library(survey)
library(graphPAF)

# Read data
data_cat <- read_csv(here::here("data", "output_data", "falls_hrs_cat.csv"))

# Look at data
skimr::skim(data_cat %>% 
              select(-id, -mwgtr))

# Clean data ----

# Numbers for exclusion sections
older <- data_cat %>% 
  filter(age >= 65) %>% # only people 65+
  filter(interview_type == 2) %>% # only ftf interviews
  filter(!is.na(fall_past_two)) %>% # answered falls question baseline
  filter(dementia == 0) %>% # without dementia
  filter(nurshm_2010 == 5) # 5 = no, nursing home at baseline removed
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
      gait_speed_conv > 2 # gait speeds > 200 cm/s 
  ) %>% 
  summarize(n()) 

# n before dropping those with missing data
included_pts1 <- older %>% 
  filter(
    nurshm_2012 == 5 & # nursing home at fu 
      dementia_2012 == 0 & # dementia at fu
      alive_2012 == 1 &  # dead at fu
      !is.na(incident_fall)   # missing fu fall info
  ) 
skimr::skim(included_pts1)
  
included_pts1 <- included_pts1 %>% 
  pull(id) 

# inaccurate/missing gait times
included_pts2 <- older %>% 
  filter(
      nurshm_2012 == 5 & # nursing home at fu 
      dementia_2012 == 0 & # dementia at fu
      alive_2012 == 1 & # dead at fu
      !is.na(incident_fall) &  # missing fu fall info
      !is.na(gait_time_mean) &
      gait_speed_conv <= 2
  ) %>% 
  pull(id)

length(included_pts1) - length(included_pts2)

# remove those with missing data
included_pts <- older %>% 
  filter(
    nurshm_2012 == 5 & # nursing home at fu 
      dementia_2012 == 0 & # dementia at fu
      alive_2012 == 1 & # dead at fu
      !is.na(incident_fall) &  # missing fu fall info
      !is.na(gait_time_mean) &
      gait_speed_conv <= 2
  ) %>% 
  select(-fall_number, -fall_injury, -incident_fall_number, -incident_fall_injury) %>% 
  drop_na() %>% 
  pull(id)

length(included_pts2) - length(included_pts)

length(included_pts)

# Create cuts for cognition, gait, and grip strength based on pts that will be included
cog_cuts <- data_cat %>% 
  filter(id %in% included_pts) %>% 
  summarise(mean_cog = mean(tot_cog, na.rm = T), sd_cog = sd(tot_cog, na.rm = T), 
            cog_imp = mean_cog - sd_cog) 

gait_cuts <- data_cat %>%
  filter(id %in% included_pts) %>%
  group_by(gender, age_group) %>%
  summarise(mean_gait = mean(gait_speed_conv, na.rm = T), sd_gait = sd(gait_speed_conv, na.rm = T),
            slow_gait_cut = mean_gait - sd_gait)

gait_cuts

grip_cuts <- data_cat %>% 
  filter(id %in% included_pts) %>% 
  group_by(gender, age_group) %>% 
  summarise(mean_grip = mean(grip_strength, na.rm = T), sd_grip = sd(grip_strength, na.rm = T), 
            weak_grip_cut = mean_grip - sd_grip)

# Clean up data
data_clean <- data_cat %>%
  # only relevant columns
  select(id, mwgtr, age, age_group, gender, schlyrs, 
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
         subj_balance,
         inactive_mod,
         pain_meds,
         tot_cog, gait_speed_conv, grip_strength, 
  ) %>% 
  # add cognition, gait, and grip cuts to main df
  left_join(., gait_cuts, by = c("age_group", "gender")) %>% 
  left_join(., grip_cuts, by = c("age_group", "gender")) %>% 
  mutate(cog_imp = case_when(
    tot_cog <= cog_cuts$cog_imp ~ 1,
    tot_cog > cog_cuts$cog_imp ~ 0,
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
    c(fall_past_two,
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
      subj_balance,
      inactive_mod,
      pain_meds,
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
design <- svydesign(id =~ id, weights =~ mwgtr, data = data_clean_factor)

# Subset based on exclusion criteria
data_sub <- subset(design, included == 1)

# Demographics ----

# Unweighted demographics
data_no_excl <- data_clean_factor %>% 
  filter(included == 1) 

tbl_unwt <- tbl_summary(
  data = data_no_excl %>% mutate(across(where(is.factor), ~ recode(.x, "0" = "No", "1" = "Yes"))),
  by = incident_fall, 
  include = c(-id, -mwgtr, -age_group, -slow_gait_cut, -mean_gait, 
              -sd_gait, -mean_grip, -sd_grip, -weak_grip_cut, -included),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{n} ({p}%)"
                   ),
  digits = all_continuous() ~ 1
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
  include = c(-id, -mwgtr, -age_group, -slow_gait_cut, -mean_gait, 
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
  include = c(-id, -mwgtr, -age_group, -slow_gait_cut, -mean_gait, 
              -sd_gait, -mean_grip, -sd_grip, -weak_grip_cut, -included),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{p}%"),
  digits = list(
                all_continuous() ~ 1,
                gait_speed_conv ~ 3
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

# Correlations between risk factors
design2 <- svydesign(id =~ id, weights =~ mwgtr, data = data_clean %>% 
                       select(id, mwgtr, included, fall_past_two, low_vision, poor_hearing, diabetes, highbp, 
                              heart, stroke, arthritis, depression, pain, subj_balance, 
                              inactive_mod, pain_meds, cog_imp, slow_gait, weak_strength) %>% 
                       mutate_if(is.factor, as.numeric)
                     )

# Subset based on exclusion criteria
data_sub2 <- subset(design2, included == 1)

data_cor <- as.matrix(data_clean %>% 
                        filter(included == 1) %>% 
                        select(fall_past_two, low_vision, poor_hearing, diabetes, highbp, 
                               heart, stroke, arthritis, depression, pain, subj_balance, 
                               inactive_mod, pain_meds, cog_imp, slow_gait, weak_strength) %>% 
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

# Full sample (of those who did gait assessment)
m_full_incident <- svyglm(incident_fall ~ 
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
                            subj_balance +
                            inactive_mod +
                            pain_meds +
                            cog_imp +
                            slow_gait +
                            weak_strength +
                            age + gender + schlyrs
                          , 
                          family = poisson(),
                          design = data_sub)

tbl_results_1 <- tbl_regression(
  m_full_incident,
  exponentiate = T,
  conf.int = T,
  estimate_fun = function(x) style_number(x, digits = 3),
  show_single_row = everything() # can comment out to see full model, easier to read with only 1 row
) %>% 
  add_significance_stars() %>% 
  modify_column_hide(column = std.error) %>%
  modify_column_unhide(column = ci) 

tbl_results_1

# export to Excel
# tbl_results_1 %>%
  # as_hux_xlsx( here::here("data", "output_data", "tbl_results_1.xlsx"))

## Model 2 ----
data_sub_no_hx <- subset(design, included == 1 & fall_past_two == 0) # correct?

# Only those without prior fall history
m_nofallhx_incident <- svyglm(incident_fall ~ 
                                low_vision + 
                                poor_hearing +
                                diabetes + 
                                highbp + 
                                heart +
                                stroke +
                                arthritis +
                                depression + 
                                pain +
                                subj_balance +
                                inactive_mod +
                                pain_meds +
                                cog_imp +
                                slow_gait +
                                weak_strength +
                                age + gender + schlyrs
                              , 
                              family = poisson(),
                              design = data_sub_no_hx)

tbl_results_2 <- tbl_regression(
  m_nofallhx_incident,
  exponentiate = T,
  conf.int = T,
  estimate_fun = function(x) style_number(x, digits = 3),
  show_single_row = everything()
) %>% 
  add_significance_stars() %>% 
  modify_column_hide(column = std.error) %>%
  modify_column_unhide(column = ci)

tbl_results_2

# export to Excel
# tbl_results_2 %>%
  # as_hux_xlsx( here::here("data", "output_data", "tbl_results_2.xlsx"))

## Model 3 ----
data_sub_inj <- subset(design, included == 1 & 
                         incident_fall == 1) # correct?

# Only those without prior fall history
m_fallinj <- svyglm(incident_fall_injury ~ 
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
                                subj_balance +
                                inactive_mod +
                                pain_meds +
                                cog_imp +
                                slow_gait +
                                weak_strength +
                                age + gender + schlyrs
                              , 
                              family = poisson(),
                              design = data_sub_inj)

tbl_results_3 <- tbl_regression(
  m_fallinj,
  exponentiate = T,
  conf.int = T,
  estimate_fun = function(x) style_number(x, digits = 3),
  show_single_row = everything()
) %>% 
  add_significance_stars() %>% 
  modify_column_hide(column = std.error) %>%
  modify_column_unhide(column = ci)

tbl_results_3

# export to Excel
# tbl_results_2 %>%
# as_hux_xlsx( here::here("data", "output_data", "tbl_results_2.xlsx"))

# Possible additional models = injurious falls

# PAF calculation ----

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
m2_full_incident <- glm(formula = incident_fall ~ 
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
                          subj_balance +
                          inactive_mod +
                          pain_meds +
                          cog_imp +
                          slow_gait +
                          weak_strength +
                          age + gender + schlyrs
                        , 
                        family = "poisson",
                        weights = mwgtr,
                        data = paf_data
)

m2_full_incident_results <- broom::tidy(m2_full_incident) %>% 
  mutate(
    refval = ifelse(estimate < 0, "Yes", "No")
  ) %>% 
  select(term, refval) %>% 
  filter(!term %in% c("(Intercept)", "age", "genderMale", "schlyrs")) %>% 
  mutate(
    term = gsub('.{3}$', '', term)
  )


for (i in 1:length(m2_full_incident_results$term)){
  p <- run_paf(m2_full_incident, m2_full_incident_results$term[i], m2_full_incident_results$refval[i], paf_data)
  result = data.frame(model ="m2_full_incident", riskfactor = m2_full_incident_results$term[i], 
                      estimate = p$estimate, ci = p$confidence_interval,
                      estimate_perc = p$estimate*100)
  pafs <- rbind(pafs, result)
  
}

## No fall hx, any fall----
paf_data_nohx = data_no_excl %>% 
  filter(fall_past_two == 0) %>% 
  mutate(across(where(is.factor), ~ recode(.x, "0" = "No", "1" = "Yes")))

m2_nofallhx_incident  <- glm(formula = incident_fall ~ 
                               low_vision + 
                               poor_hearing +
                               diabetes + 
                               highbp + 
                               heart +
                               stroke +
                               arthritis +
                               depression + 
                               pain +
                               subj_balance +
                               inactive_mod +
                               pain_meds +
                               cog_imp +
                               slow_gait +
                               weak_strength +
                               age + gender + schlyrs
                             , 
                             family = "poisson",
                             weights = mwgtr,
                             data = paf_data_nohx
)

m2_nofallhx_incident_results <- broom::tidy(m2_nofallhx_incident) %>% 
  mutate(
    refval = ifelse(estimate < 0, "Yes", "No")
  ) %>% 
  select(term, refval) %>% 
  filter(!term %in% c("(Intercept)", "age", "genderMale", "schlyrs")) %>% 
  mutate(
    term = gsub('.{3}$', '', term)
  )

for (i in 1:length(m2_nofallhx_incident_results$term)){
  p <- run_paf(m2_nofallhx_incident, m2_nofallhx_incident_results$term[i], 
               m2_nofallhx_incident_results$refval[i], paf_data_nohx)
  result = data.frame(model ="m2_nofallhx_incident", riskfactor = m2_nofallhx_incident_results$term[i], 
                      estimate = p$estimate, ci = p$confidence_interval,
                      estimate_perc = p$estimate*100)
  pafs <- rbind(pafs, result)
  
}

## Injurious falls----
paf_data_injfall = data_no_excl %>% 
  filter(incident_fall == 1) %>% 
  mutate(across(where(is.factor), ~ recode(.x, "0" = "No", "1" = "Yes")))

m3_fallinj  <- glm(formula = incident_fall_injury ~
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
                               subj_balance +
                               inactive_mod +
                               pain_meds +
                               cog_imp +
                               slow_gait +
                               weak_strength +
                               age + gender + schlyrs
                             , 
                             family = "poisson",
                             weights = mwgtr,
                             data = paf_data_injfall
)

m3_fallinj_results <- broom::tidy(m3_fallinj) %>% 
  mutate(
    refval = ifelse(estimate < 0, "Yes", "No")
  ) %>% 
  select(term, refval) %>% 
  filter(!term %in% c("(Intercept)", "age", "genderMale", "schlyrs")) %>% 
  mutate(
    term = gsub('.{3}$', '', term)
  )

for (i in 1:length(m3_fallinj_results$term)){
  p <- run_paf(m3_fallinj, m3_fallinj_results$term[i], 
               m3_fallinj_results$refval[i], paf_data_injfall)
  result = data.frame(model ="m3_fallinj", riskfactor = m3_fallinj_results$term[i], 
                      estimate = p$estimate, ci = p$confidence_interval,
                      estimate_perc = p$estimate*100)
  pafs <- rbind(pafs, result)
  
}

pafs 
# write_csv(pafs, here::here("data", "output_data", "paf_results.csv"))

## Overall PAF----

# Full data
data_full_any <- paf_data %>% 
  mutate(overall_risk = as.factor(case_when(
    fall_past_two == "Yes" | 
      heart == "Yes" | 
      poor_hearing == "Yes" | 
      subj_balance == "Yes" 
    ~ 1,
    TRUE ~ 0
  ))
  )

m3_full_incident <- glm(formula = incident_fall ~ 
                          overall_risk +
                          age + gender + schlyrs
                        , 
                        family = "poisson",
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
                  calculation_method = "D", weight_vec=data_full_any$mwgtr
)


# No fall hx
data_nohx_any <- paf_data_nohx %>% 
  mutate(overall_risk = as.factor(case_when(
    poor_hearing == "Yes" | 
      stroke == "Yes"  
    ~ 1,
    TRUE ~ 0
  ))
  )

m3_nofallhx_incident <- glm(formula = incident_fall ~ 
                          overall_risk +
                          age + gender + schlyrs
                        , 
                        family = "poisson",
                        weights = mwgtr,
                        data = data_nohx_any
)

tbl_regression(
  m3_nofallhx_incident,
  exponentiate = T,
  conf.int = T,
  # show_single_row = everything()
) %>% 
  modify_column_hide(column = c(std.error, p.value)) #  estimates are the same as above, CIs are different

PAF_calc_discrete(model=m3_nofallhx_incident, riskfactor="overall_risk",
                  refval = 0, data = data_nohx_any,
                  ci = T,
                  calculation_method = "D", weight_vec=data_nohx_any$mwgtr
)

# rfs <- rf_summary(
#   rf_names = c("fall_past_two","low_vision", 
#                                "poor_hearing",
#                                "diabetes", 
#                                "highbp", 
#                                "heart",
#                                "stroke",
#                                "arthritis",
#                                "depression", 
#                                "pain",
#                                "subj_balance",
#                                "inactive_mod",
#                                "pain_meds",
#                                "cog_imp",
#                                "slow_gait",
#                                "weak_strength"),
#   rf_prev = c(.35, .19, .21, .22, .65, .29, .069, .7, .092, 
#               .36, .33, .32, .23, .12, .13, .14),
#   risk = c(2.17, 1.07, 1.2, 1, 1.07, 1.16, 1.15, 1.16, 1.12, 
#            1.11, 1.17, .99, 1.04, .94, .96, .99),
#   log = T
# )

rfs <- rf_summary(
  rf_names = c("fall_past_two",
               "poor_hearing",
               "heart",
               "subj_balance"),
  rf_prev = c(.35, .21, .29, .33),
  risk = c(2.17, 1.2, 1.16, 1.17),
  log = T
)
plot(rfs, type = "rn")
