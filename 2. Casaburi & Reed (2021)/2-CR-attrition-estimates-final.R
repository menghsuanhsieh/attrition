###############################
# date: 9/1/2022
# Groh & McKenzie application
###############################

rm(list = ls())
cat("\014")
options(scipen=999)
# install.packages("pacman") # uncomment if needed

# set seed for pilot
set.seed(3333)

pacman::p_load(ramify, fastDummies, doParallel, foreach, data.table, nbpMatching, sandwich, lmtest, haven, tidyverse, xtable, rstudioapi)

# # working directory  # if running this file alone, uncomment this + next line
# setwd(dirname(getSourceEditorContext()$path))

# data
bags <- as.data.table(read_dta("R2/dta/data_for_analysis/data_bags_cleaned_210630.dta"))

# result table
cr_attr_est <- data.table(reg = c("Bags (1)",
                                  "Bags (2)",
                                  "Bags (3)",
                                  "Bags (4)",
                                  "Bags (5)"),
                          Original = NA_real_, Theta = NA_real_, Attrition = NA_real_)

# bags ----------
t5_reg <- lm(pf ~ treat + factor(pair), data = bags[after==1][a16_1_gradeA==1])
t5_reg$coefficients[2]

cr_attr_est[1, 2:3] <- list(as.numeric(t5_reg$coefficients[2]),
                            as.numeric(lm(pf ~ treat, data = bags[after==1][a16_1_gradeA==1])$coefficients[2]))
cr_attr_est[1, 4] <- list(100 - 100 * length(unique(bags[after==1][a16_1_gradeA==1][is.na(pf)==FALSE]$sid)) / 80)

# bags FE ----------
t5_reg <- lm(pf ~ treat + factor(week) + factor(pair), data = bags[after==1][a16_1_gradeA==1])
t5_reg$coefficients[2]

cr_attr_est[2, 2:3] <- list(as.numeric(t5_reg$coefficients[2]),
                            as.numeric(lm(pf ~ treat + factor(week), data = bags[after==1][a16_1_gradeA==1])$coefficients[2]))
cr_attr_est[2, 4] <- list(100 - 100 * length(unique(bags[after==1][a16_1_gradeA==1][is.na(pf)==FALSE]$sid)) / 80)

# bags - FE TC -----------
t5_reg_5 <- lm(pf ~ treat + a6_weightA_B + villages_B + b_sup_sid_B + lshare_sid_B + b4_age_B   + b7_2_years_mid_B + nice_floor_B + mobile_B + storage_B  + factor(week) + factor(pair), data = bags[after==1][a16_1_gradeA==1])
t5_reg_5$coefficients[2]

length(unique(bags[after==1][a16_1_gradeA==1][!(is.na(a6_weightA_B) | is.na(villages_B) | is.na(b_sup_sid_B) | is.na(lshare_sid_B) | is.na(b4_age_B) | is.na(b7_2_years_mid_B) | is.na(nice_floor_B) | is.na(mobile_B) | is.na(storage_B))]$sid))

cr_attr_est[3, 2:3] <- list(as.numeric(t5_reg_5$coefficients[2]),
                            as.numeric(lm(pf ~ treat + a6_weightA_B + villages_B + b_sup_sid_B + lshare_sid_B + b4_age_B + b7_2_years_mid_B + nice_floor_B + mobile_B + storage_B + factor(week), data = bags[after==1][a16_1_gradeA==1])$coefficients[2]))
cr_attr_est[3, 4] <- list(100 - 100 * length(unique(bags[after==1][a16_1_gradeA==1][is.na(pf)==FALSE]$sid)) / 80)

# bags - FE VC -----------
t5_reg_5 <- lm(pf ~ treat + lshare + N_traders_oth + N_treat_oth + milesnearest + suppliers + chief_1 + chief_2 + chief_3 + chief_4 + chief_5 + factor(week) + factor(pair), data = bags[after==1][a16_1_gradeA==1])
t5_reg_5$coefficients[2] 

cr_attr_est[4, 2:3] <- list(as.numeric(t5_reg_5$coefficients[2]),
                            as.numeric(lm(pf ~ treat + lshare + N_traders_oth + N_treat_oth + milesnearest + suppliers + chief_1 + chief_2 + chief_3 + chief_4 + chief_5 + factor(week), data = bags[after==1][a16_1_gradeA==1])$coefficients[2]))
cr_attr_est[4, 4] <- list(100 - 100 * length(unique(bags[after==1][a16_1_gradeA==1][is.na(pf)==FALSE]$sid)) / 80)

#  bags - everything ----------
t5_reg_5 <- lm(pf ~ treat + a6_weightA_B + villages_B + b_sup_sid_B + lshare_sid_B + b4_age_B   + b7_2_years_mid_B + nice_floor_B + mobile_B + storage_B + lshare + N_traders_oth + N_treat_oth + milesnearest + suppliers + chief_1 + chief_2 + chief_3 + chief_4 + chief_5 + factor(week) + factor(pair), data = bags[after==1][a16_1_gradeA==1])
t5_reg_5$coefficients[2] 

cr_attr_est[5, 2:3] <- list(as.numeric(t5_reg_5$coefficients[2]),
                            as.numeric(lm(pf ~ treat + a6_weightA_B + villages_B + b_sup_sid_B + lshare_sid_B + b4_age_B   + b7_2_years_mid_B + nice_floor_B + mobile_B + storage_B + lshare + N_traders_oth + N_treat_oth + milesnearest + suppliers + chief_1 + chief_2 + chief_3 + chief_4 + chief_5 + factor(week), data = bags[after==1][a16_1_gradeA==1])$coefficients[2]))
cr_attr_est[5, 4] <- list(100 - 100 * length(unique(bags[after==1][a16_1_gradeA==1][is.na(pf)==FALSE]$sid)) / 80)

# final table making -------------
final_table <- dcast(melt.data.table(copy(cr_attr_est)), variable ~ reg) #.GRP, by = .(reg) # [,-c("Attrition")]

# percentage deviatio ---------------
cr_attr_est[, ss := ifelse(reg=="Bags", 0, 1)
][, pct := abs((Original - Theta)/abs(Theta)) * 100
][, avg_pct_chg := mean(pct)
  ][, pct_1 := abs((Theta - Original)/abs(Original)) * 100]

fwrite(cr_attr_est, "casaburi-main-results.csv")