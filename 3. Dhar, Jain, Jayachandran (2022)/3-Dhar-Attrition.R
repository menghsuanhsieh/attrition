rm(list = ls())
cat("\014")
options(scipen=999)
# install.packages("pacman") # uncomment if needed

# set seed for pilot
set.seed(3333)

pacman::p_load(ramify, fastDummies, doParallel, foreach, data.table, nbpMatching, sandwich, lmtest, haven, tidyverse, xtable, rstudioapi, readstata13)

# # working directory  # if running this file alone, uncomment this + next line
# setwd(dirname(getSourceEditorContext()$path))

# result table
result_table_dhar <- data.table(outcome = c("Aspiration","Gender","Behaviour"), no_sfe = NA_real_, with_sfe = NA_real_, attrition = NA_real_)

# data -----------------
final_data <- as.data.table(readstata13::read.dta13("data/bt_analysis_final.dta"))

# aspiration -------------
aspiration1 <- lm(E_Saspiration_index2 ~ B_treat + B_Saspiration_index2 + district_gender +
                    district_gender_1 + district_gender_2 + district_gender_3 + district_gender_4 + district_gender_5 +
                district_gender_6 + district_gender_7 + district_gender_8 +
                  gender_grade + gender_grade_1 + gender_grade_2 + gender_grade_3 + gender_grade_4 + B_Sgrade6 +
                  E_Sboard_score_median_flag + E_Shighest_educ_median_flag + E_Sdiscuss_educ_flag +
                  E_Soccupa_25_white_flag + E_Scont_educ_flag, # + B_Saspiration_index2_flag + B_Saspiration_index2_m,
              data = final_data[B_Sgirl==1 & is.na(E_Steam_id)==FALSE]) # & attrition_el==0

summary(aspiration1)

result_table_dhar[1,2] <- aspiration1$coefficients[2]

aspiration_with_sfe <- lm(E_Saspiration_index2 ~ B_treat + B_Saspiration_index2 + district_gender +
                            district_gender_1 + district_gender_2 + district_gender_3 + district_gender_4 + district_gender_5 +
                            district_gender_6 + district_gender_7 + district_gender_8 +
                            gender_grade + gender_grade_1 + gender_grade_2 + gender_grade_3 + gender_grade_4 + B_Sgrade6 +
                            E_Sboard_score_median_flag + E_Shighest_educ_median_flag + E_Sdiscuss_educ_flag +
                            E_Soccupa_25_white_flag + E_Scont_educ_flag +
                            factor(B_stratum), # + B_Saspiration_index2_flag + B_Saspiration_index2_m,
                          data = final_data[B_Sgirl==1 & is.na(E_Steam_id)==FALSE]) # & attrition_el==0

summary(aspiration_with_sfe)

result_table_dhar[1,3] <- aspiration_with_sfe$coefficients[2]

table(is.na(final_data$E_Saspiration_index2))
table(is.na(final_data$E_Sgender_index2))
table(is.na(final_data$E_Sbehavior_index2))

table(final_data$attrition_el)

## gender -------------
gender1 <- lm(E_Sgender_index2 ~ B_treat + B_Sgender_index2 + 
                district_gender +
                district_gender_1 + district_gender_2 + district_gender_3 + district_gender_4 + district_gender_5 +
                district_gender_6 + district_gender_7 + district_gender_8 +
                gender_grade + gender_grade_1 + gender_grade_2 + gender_grade_3 + gender_grade_4 +
                E_Swives_less_edu_n_flag + E_Select_woman_y_flag +
                E_Sboy_more_oppo_n_flag + E_Stown_studies_y_flag +
                E_Sman_final_deci_n_flag + E_Swoman_viol_n_flag +
                E_Scontrol_daughters_n_flag + E_Swoman_role_home_n_flag +
                E_Smen_better_suited_n_flag + E_Ssimilar_right_y_flag +
                E_Smarriage_more_imp_n_flag + E_Steacher_suitable_n_flag +
                E_Sgirl_marriage_age_19_flag + E_Smarriage_age_diff_m_flag +
                E_Sstudy_marry_flag + E_Sallow_work_y_flag + E_Sfertility_flag,
              data = final_data[is.na(E_Steam_id)==FALSE & attrition_el==0]) # & attrition_el==0

summary(gender1)

result_table_dhar[2,2] <- gender1$coefficients[2]

gender_with_sfe <- lm(E_Sgender_index2 ~ B_treat + B_Sgender_index2 + 
                district_gender +
                district_gender_1 + district_gender_2 + district_gender_3 + district_gender_4 + district_gender_5 +
                district_gender_6 + district_gender_7 + district_gender_8 +
                gender_grade + gender_grade_1 + gender_grade_2 + gender_grade_3 + gender_grade_4 +
                E_Swives_less_edu_n_flag + E_Select_woman_y_flag +
                E_Sboy_more_oppo_n_flag + E_Stown_studies_y_flag +
                E_Sman_final_deci_n_flag + E_Swoman_viol_n_flag +
                E_Scontrol_daughters_n_flag + E_Swoman_role_home_n_flag +
                E_Smen_better_suited_n_flag + E_Ssimilar_right_y_flag +
                E_Smarriage_more_imp_n_flag + E_Steacher_suitable_n_flag +
                E_Sgirl_marriage_age_19_flag + E_Smarriage_age_diff_m_flag +
                E_Sstudy_marry_flag + E_Sallow_work_y_flag + E_Sfertility_flag + factor(B_stratum),
              data = final_data[is.na(E_Steam_id)==FALSE & attrition_el==0]) # & attrition_el==0

summary(gender_with_sfe)

result_table_dhar[2,3] <- gender_with_sfe$coefficients[2]

## beaviour ---------------

behaviour1 <- lm(E_Sbehavior_index2 ~ B_treat + B_Sbehavior_index2 + 
                district_gender +
                district_gender_1 + district_gender_2 + district_gender_3 + district_gender_4 + district_gender_5 +
                district_gender_6 + district_gender_7 + district_gender_8 +
                gender_grade + gender_grade_1 + gender_grade_2 + gender_grade_3 + gender_grade_4 +
                E_Stalk_opp_gender_comm_flag + E_Ssit_opp_gender_comm_flag +
                E_Scook_clean_comm + E_Sabsent_sch_hhwork_comm_flag +
                E_Sdiscourage_college_comm_flag + E_Sdiscourage_work_comm_flag,
              data = final_data[is.na(E_Steam_id)==FALSE & attrition_el==0]) # & attrition_el==0

summary(behaviour1)

result_table_dhar[3,2] <- behaviour1$coefficients[2]

behaviour_with_sfe <- lm(E_Sbehavior_index2 ~ B_treat + B_Sbehavior_index2 + 
                   district_gender +
                   district_gender_1 + district_gender_2 + district_gender_3 + district_gender_4 + district_gender_5 +
                   district_gender_6 + district_gender_7 + district_gender_8 +
                   gender_grade + gender_grade_1 + gender_grade_2 + gender_grade_3 + gender_grade_4 +
                   E_Stalk_opp_gender_comm_flag + E_Ssit_opp_gender_comm_flag +
                   E_Scook_clean_comm + E_Sabsent_sch_hhwork_comm_flag +
                   E_Sdiscourage_college_comm_flag + E_Sdiscourage_work_comm_flag +
                   factor(B_stratum),
                 data = final_data[is.na(E_Steam_id)==FALSE & attrition_el==0]) # & attrition_el==0

summary(behaviour_with_sfe)

result_table_dhar[3,3] <- behaviour_with_sfe$coefficients[2]

# attrition rates by outcomes -------------
result_table_dhar[1,4] <- table(is.na(final_data$E_Saspiration_index2))[2] / (table(is.na(final_data$E_Saspiration_index2))[1] + table(is.na(final_data$E_Saspiration_index2))[2]) * 100
result_table_dhar[2,4] <- table(is.na(final_data$E_Sgender_index2))[2] / (table(is.na(final_data$E_Sgender_index2))[1] + table(is.na(final_data$E_Sgender_index2))[2]) * 100

result_table_dhar[3,4] <- table(is.na(final_data$E_Sbehavior_index2))[2] / (table(is.na(final_data$E_Sbehavior_index2))[1] + table(is.na(final_data$E_Sbehavior_index2))[2]) * 100

# save point estimates results ----------
fwrite(result_table_dhar, "dhar-main-results.csv")

